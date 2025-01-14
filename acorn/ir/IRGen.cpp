#include "IRGen.h"

#include "../Context.h"
#include "../Logger.h"
#include "DebugGen.h"
#include "SourceFile.h"

#define push_scope()         \
IRScope new_scope;           \
new_scope.parent = ir_scope; \
ir_scope = &new_scope;

#define pop_scope()               \
gen_call_loc_scope_destructors(); \
ir_scope = ir_scope->parent;

acorn::IRGenerator::IRGenerator(Context& context)
    : context(context),
      ll_context(context.get_ll_context()),
      ll_module(context.get_ll_module()),
      builder(ll_context),
      should_emit_debug_info(context.should_emit_debug_info())
{
}

void acorn::IRGenerator::gen_function(Func* func) {

    // -- Debug
    // Logger::debug("generating function: %s", func->name);

    gen_function_decl(func);
    gen_function_body(func);

}

void acorn::IRGenerator::gen_global_variable(Var* var) {

    gen_global_variable_decl(var);
    gen_global_variable_body(var);

}

void acorn::IRGenerator::gen_implicit_function(ImplicitFunc* implicit_func) {
    di_emitter = implicit_func->structn->file->di_emitter;
    if (implicit_func->implicit_kind == ImplicitFunc::ImplicitKind::DefaultConstructor) {
        gen_implicit_default_constructor(implicit_func->structn);
    } else if (implicit_func->implicit_kind == ImplicitFunc::ImplicitKind::Destructor) {
        gen_implicit_destructor(implicit_func->structn);
    } else if (implicit_func->implicit_kind == ImplicitFunc::ImplicitKind::CopyConstructor) {
        gen_implicit_copy_constructor(implicit_func->structn);
    } else if (implicit_func->implicit_kind == ImplicitFunc::ImplicitKind::MoveConstructor) {
        gen_implicit_move_constructor(implicit_func->structn);
    } else {
        acorn_fatal("Unreachable. Uknown implicit function");
    }
}

void acorn::IRGenerator::add_return_to_global_init_function() {
    if (!context.ll_global_init_function) {
        return;
    }

    builder.SetInsertPoint(&context.ll_global_init_function->back());
    builder.CreateRetVoid();
}

llvm::Value* acorn::IRGenerator::gen_node(Node* node) {
    switch (node->kind) {
    case NodeKind::BinOp:
        return gen_binary_op(static_cast<BinOp*>(node));
    case NodeKind::UnaryOp:
        return gen_unary_op(static_cast<UnaryOp*>(node));
    case NodeKind::Var:
        return gen_variable(static_cast<Var*>(node));
    case NodeKind::Number:
        return gen_number(static_cast<Number*>(node));
    case NodeKind::IdentRef:
        return gen_ident_reference(static_cast<IdentRef*>(node));
    case NodeKind::ReturnStmt:
        return gen_return(static_cast<ReturnStmt*>(node));
    case NodeKind::IfStmt:
        return gen_if(static_cast<IfStmt*>(node));
    case NodeKind::ScopeStmt:
        return gen_scope_with_dbg_scope(static_cast<ScopeStmt*>(node));
    case NodeKind::FuncCall:
        return gen_function_call(static_cast<FuncCall*>(node), nullptr);
    case NodeKind::Bool:
        return gen_bool(static_cast<Bool*>(node));
    case NodeKind::String:
        return gen_string(static_cast<String*>(node));
    case NodeKind::Null:
        return gen_null();
    case NodeKind::Cast:
        return gen_cast(static_cast<Cast*>(node));
    case NodeKind::MemoryAccess:
        return gen_memory_access(static_cast<MemoryAccess*>(node));
    case NodeKind::Array:
        return gen_array(static_cast<Array*>(node), nullptr);
    case NodeKind::DotOperator:
        return gen_dot_operator(static_cast<DotOperator*>(node));
    case NodeKind::PredicateLoopStmt:
        return gen_predicate_loop(static_cast<PredicateLoopStmt*>(node));
    case NodeKind::RangeLoopStmt:
        return gen_range_loop(static_cast<RangeLoopStmt*>(node));
    case NodeKind::IteratorLoopStmt:
        return gen_iterator_loop(static_cast<IteratorLoopStmt*>(node));
    case NodeKind::BreakStmt:
    case NodeKind::ContinueStmt:
        return gen_loop_control(static_cast<LoopControlStmt*>(node));
    case NodeKind::SwitchStmt:
        return gen_switch(static_cast<SwitchStmt*>(node));
    case NodeKind::StructInitializer:
        return gen_struct_initializer(static_cast<StructInitializer*>(node), nullptr);
    case NodeKind::This:
        return gen_this(static_cast<This*>(node));
    case NodeKind::SizeOf:
        return gen_sizeof(static_cast<SizeOf*>(node));
    case NodeKind::Ternary:
        return gen_ternary(static_cast<Ternary*>(node), nullptr);
    case NodeKind::Reflect:
        return gen_reflect(static_cast<Reflect*>(node));
    default:
        acorn_fatal("gen_value: Missing case");
        return nullptr;
    }
}

llvm::Value* acorn::IRGenerator::gen_rvalue(Expr* node) {
    auto ll_value = gen_node(node);

    if (node->kind == NodeKind::IdentRef ||
        node->kind == NodeKind::DotOperator) {
        IdentRef* ref = static_cast<IdentRef*>(node);
        if (ref->is_var_ref() &&
            !ref->is_foldable &&       // If the reference is foldable then no memory address is provided for storage to load.
            !ref->type->is_aggregate() // Aggregates are not loaded because they are suppose to copy their memory through memcpy
                                       // or copy constructors.
            ) {
            ll_value = builder.CreateLoad(gen_type(node->type), ll_value);
        }
    } else if (node->kind == NodeKind::UnaryOp) {
        // Although the code for the unary operator may already have loaded
        // the value it must be loaded again when a rvalue is needed.
        // For example if you have:
        //
        //    char* a
        //    char b = *a
        //
        // We allocate `a` in LLVM as a char* which means the original type
        // of the identifier reference to `a` is i8**. When it is loaded
        // via the operator * this will load the value as i8*. But this
        // is not what we need, we need just i8, so it must be loaded again.
        //
        // Additionally this is suitable for situations like the following:
        //
        //    char* a
        //    *a = 5
        //
        // Again `a` is allocated and has the value i8** originally, when
        // loaded it becomes the type i8* which is the address we need to
        // store the value `5` to.
        //
        UnaryOp* unary_op = static_cast<UnaryOp*>(node);
        if (unary_op->op == '*' &&
            !node->type->is_array() // Do not load an array because they must be treated like lvalues.
            ) {
            ll_value = builder.CreateLoad(gen_type(node->type), ll_value);
        }
    } else if (node->kind == NodeKind::MemoryAccess) {
        if (!node->type->is_array()) { // Arrays are always lvalues
            ll_value = builder.CreateLoad(gen_type(node->type), ll_value);
        }
    }

    if (node->cast_type) {
        ll_value = gen_cast(node->cast_type, node, ll_value);

        node->cast_type = nullptr;
    }

    return ll_value;
}

void acorn::IRGenerator::gen_function_decl(Func* func) {

    if (func->ll_func) {
        return; // Return early because the declaration has already been generated.
    }

    context.queue_gen(func);

    if (func->has_modifier(Modifier::Native)) {
        if (func->ll_intrinsic_id != llvm::Intrinsic::not_intrinsic) {
            return; // Return early because the declaration has already been "generated" (it is intrinsic).
        }
    }

    llvm::SmallVector<llvm::Type*> ll_param_types;
    ll_param_types.reserve(func->params.size());

    bool is_main = func == context.get_main_function();
    auto ll_ret_type = gen_function_return_type(func, is_main);

    // Creating the parameter types.
    if (func->uses_aggr_param) {
        ll_param_types.push_back(builder.getPtrTy());
    }

    if (func->structn) {
        ll_param_types.push_back(builder.getPtrTy());
    }

    for (Var* param : func->params) {
        ll_param_types.push_back(gen_function_param_type(param));
    }

    auto ll_func_type = llvm::FunctionType::get(ll_ret_type,
                                                ll_param_types,
                                                func->uses_native_varargs);

    auto get_name = [func, &context=this->context, is_main] {
        bool dont_fix_name = is_main || func->has_modifier(Modifier::Native);
        if (!func->linkname.empty())
            return llvm::Twine(func->linkname);
        llvm::Twine ll_name = func->name.to_string();
        return dont_fix_name ? ll_name : ll_name.concat(".acorn");
    };

    bool needs_external_linkage = (is_main || func->has_modifier(Modifier::Native));
    auto ll_linkage = needs_external_linkage ? llvm::GlobalValue::ExternalLinkage
                                             : llvm::GlobalValue::InternalLinkage;
    auto ll_func = llvm::Function::Create(
        ll_func_type,
        ll_linkage,
        get_name(),
        ll_module
    );
    func->ll_func = ll_func;

    if (func->has_modifier(Modifier::Native)) {
#ifdef _WIN32
        ll_func->setCallingConv(llvm::CallingConv::X86_StdCall);
#endif
    } else if (!func->has_modifier(Modifier::DllImport)) {
        // Tell LLVM that the return value cannot be poisioned or undefined
        // allowing for better optimization.
        if (!ll_ret_type->isVoidTy()) {
            ll_func->addRetAttr(llvm::Attribute::NoUndef);
        }
    }

    if (func->has_modifier(Modifier::DllImport)) {
        ll_func->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    }

    // Assigning names to parameters and attributes to parameters.
    auto assign_param_info = [ll_func](size_t param_idx, llvm::Twine ll_name) finline{
        auto ll_param = ll_func->getArg(static_cast<unsigned int>(param_idx));
        ll_param->setName(ll_name);
        ll_param->addAttr(llvm::Attribute::NoUndef);
        return ll_param;
    };

    size_t param_idx = 0;
    if (func->uses_aggr_param) {
        auto ll_param = assign_param_info(param_idx, "aggr.ret.addr");
        // Add noalias attribute which tells the compiler no other pointer
        // could possibly point to this pointer.
        ll_param->addAttr(llvm::Attribute::NoAlias);
        ++param_idx;
    }
    if (func->structn) {
        auto ll_param = assign_param_info(param_idx++, "in.this");
    }

    for (Var* param : func->params) {
        assign_param_info(param_idx++, llvm::Twine("in.") + param->name.to_string());
    }

    if (func->is_constructor && func->params.empty()) {
        func->structn->ll_default_constructor = ll_func;
    }
    if (func->is_destructor) {
        func->structn->ll_destructor = ll_func;
    }
    if (func->is_copy_constructor) {
        func->structn->ll_copy_constructor = ll_func;
    }
}

llvm::Type* acorn::IRGenerator::gen_function_return_type(Func* func, bool is_main) {
    if (is_main) {
        return llvm::Type::getInt32Ty(ll_context);
    } else if (func->return_type->is_aggregate()) {
        auto ll_aggr_type = gen_type(func->return_type);
        uint64_t aggr_mem_size = sizeof_type_in_bytes(ll_aggr_type) * 8;
        if (aggr_mem_size <= ll_module.getDataLayout().getPointerSizeInBits()) {
            // The aggregate can fit into an integer.
            auto ll_type = llvm::Type::getIntNTy(ll_context, static_cast<unsigned int>(next_pow2(aggr_mem_size)));
            func->ll_aggr_int_ret_type = ll_type;
            return ll_type;
        }
        func->uses_aggr_param = true;
        return llvm::Type::getVoidTy(ll_context);
    }
    return gen_type(func->return_type);
}

void acorn::IRGenerator::gen_function_body(Func* func) {
    if (func->has_modifier(Modifier::Native)) return;

    cur_func    = func;
    cur_struct  = func->structn;
    ll_cur_func = func->ll_func;

    auto ll_entry = gen_bblock("entry", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    di_emitter = func->file->di_emitter;
    emit_dbg(di_emitter->emit_function(func));

    push_scope();

    // Creating the return block and address if they are needed.
    bool is_main = func == context.get_main_function();
    if (func->num_returns > 1) {
        ll_ret_block = gen_bblock("ret.block");
        if (!func->uses_aggr_param) {
            if (func->return_type->is_not(context.void_type)) {
                ll_ret_addr = builder.CreateAlloca(gen_type(func->return_type), nullptr, "ret.addr");
            } else if (is_main) {
                ll_ret_addr = builder.CreateAlloca(llvm::Type::getInt32Ty(ll_context), nullptr, "ret.addr");
            }
        }
    }

    // Setup takedown function dependencies.
    if (is_main) {
        context.ll_global_init_call_bb = ll_entry;
    }

    // Allocating and storing incoming variables.
    unsigned int param_idx = 0;
    if (func->uses_aggr_param) {
        ll_ret_addr = ll_cur_func->getArg(param_idx++);
    }
    if (func->structn) {
        ll_this = ll_cur_func->getArg(param_idx++);

        if (should_emit_debug_info) {
            // In debug mode the 'this' pointer needs an address for the debugger
            // to read it's memory.
            auto ll_this_address = builder.CreateAlloca(builder.getPtrTy(), nullptr, "this.addr");
            builder.CreateStore(ll_this, ll_this_address);
            ll_this = builder.CreateLoad(builder.getPtrTy(), ll_this_address);
            ll_this->setName("this");
            di_emitter->emit_struct_this_variable(ll_this_address, func, builder);
        }
    }

    for (Var* param : func->params) {
        if (!param->is_aggr_param) {
            auto ll_param_type = gen_type(param->type);
            gen_variable_address(param, ll_param_type);
            emit_dbg(di_emitter->emit_function_variable(param, builder));
            builder.CreateStore(ll_cur_func->getArg(param_idx++), param->ll_address);
        } else {
            // There is no reason to store the incoming parameter we can just
            // point to the incoming parameter value.
            param->ll_address = ll_cur_func->getArg(param_idx++);
            emit_dbg(di_emitter->emit_function_variable(param, builder));
        }

        process_destructor_state(param->type, param->ll_address);
    }

    // Allocating memory for variables declared in the function.
    for (Var* var : func->vars_to_alloc) {
        if (func->uses_aggr_param && var == func->aggr_ret_var) {
            var->ll_address = ll_ret_addr;
        } else {
            gen_variable_address(var, gen_type(var->type));
        }
    }

    if (func->is_constructor) {
        // Need to initialize the field's values.
        auto structn = func->structn;
        auto ll_struct_type = gen_type(structn->struct_type);

        // TODO: once there are initializer lists this will need
        //       to only initialize the values not in the initializer
        //       list.

        unsigned field_idx = 0;
        for (; field_idx < structn->fields.size(); field_idx++) {
            Var* field = structn->fields[field_idx];
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_this, field_idx);
            if (field->assignment) {
                gen_assignment(ll_field_addr, field->type, field->assignment);
            } else {
                gen_default_value(ll_field_addr, field->type);
            }
        }
    }

    gen_scope(func->scope);

    // Before branching to the common shared return block need
    // to clean up the main scope's destructors.
    pop_scope();

    if (func->is_destructor) {
        // The function is a destructor so need to call the destructors
        // of any of the fields that have destructors themselves.
        auto structn = func->structn;
        if (structn->fields_need_destruction) {
            auto ll_struct_type = gen_struct_type(structn->struct_type);
            for (Var* field : structn->fields) {
                if (field->type->needs_destruction()) {
                    auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_this, field->field_idx);
                    gen_call_destructors(field->type, ll_field_addr);
                }
            }
        }
    }
    if (func->is_copy_constructor) {
        // The function is a copy constructor so need to call the copy
        // constructors of any of the fields that have copy constructors
        // themselves.
        auto structn = func->structn;
        if (structn->fields_need_copy_call) {
            auto ll_struct_type = gen_struct_type(structn->struct_type);
            auto ll_from_struct_address = ll_cur_func->getArg(1);

            for (Var* field : structn->fields) {
                copy_struct_field_constructor(field,
                                              ll_this,
                                              ll_from_struct_address,
                                              ll_struct_type);
            }
        }
    }
    if (func->is_move_constructor) {
        auto structn = func->structn;
        if (structn->fields_need_move_call) {
            auto ll_struct_type = gen_struct_type(structn->struct_type);
            auto ll_from_struct_address = ll_cur_func->getArg(1);

            for (Var* field : structn->fields) {
                try_move_then_copy_struct_field_constructor(field,
                                                            ll_this,
                                                            ll_from_struct_address,
                                                            ll_struct_type);
            }
        }
    }

    if (func->num_returns > 1) {
        // Have to check for termination because the last statement might
        // have been an if statement that already branched.
        insert_bblock_at_end(ll_ret_block);
        gen_branch_if_not_term(ll_ret_block);
        builder.SetInsertPoint(ll_ret_block);

        if (!func->uses_aggr_param &&
            func->return_type->is_not(context.void_type) && !is_main) {

            // The return value returns to an address so need to load
            // the value.
            auto ll_load_type = is_main ? llvm::Type::getInt32Ty(ll_context) : gen_type(func->return_type);

            if (func->ll_aggr_int_ret_type) {
                // We actually return an optimized integer so we need to load the returning
                // struct as that integer.
                ll_load_type = func->ll_aggr_int_ret_type;
            }

            ll_ret_value = builder.CreateLoad(ll_load_type, ll_ret_addr, "ret.val");
        }
    }

    if (is_main) {
        context.ll_global_cleanup_call_bb = builder.GetInsertBlock();
    }

    gen_call_destructors(always_initialized_destructor_objects);

    llvm::Instruction* ll_return;
    if (func->return_type->is(context.void_type) && !is_main) {
        ll_return = builder.CreateRetVoid();
    } else if (is_main &&
               ((!func->scope->empty() && func->scope->back()->is_not(NodeKind::ReturnStmt))
               || func->scope->empty())) {
        // Implicit return for main function but since the main function always returns an
        // integer it is handled specially.
        ll_return = builder.CreateRet(builder.getInt32(0));
    } else if (func->uses_aggr_param) {
        ll_return = builder.CreateRetVoid();
    } else {
        ll_return = builder.CreateRet(ll_ret_value);
    }

    if (should_emit_debug_info) {
        bool emitted_for_return = false;
        if (func->num_returns == 1 && !func->scope->empty()) {
            Node* last = func->scope->back();
            if (last->is(NodeKind::ReturnStmt)) {
                emitted_for_return = true;
                di_emitter->emit_location(ll_return, last->loc);
            }
        }
        if (!emitted_for_return) {
            // Still need to emit it if there is multiple returns because otherwise the debugger
            // will go back to the line where the function is defined which is not what we want.
            di_emitter->emit_location(ll_return, cur_func->scope->end_loc);
        }
        di_emitter->emit_function_end(func);
    }
}

void acorn::IRGenerator::gen_variable_address(Var* var, llvm::Type* ll_alloc_type) {
    var->ll_address = gen_alloca(ll_alloc_type, var->name.to_string());
}

llvm::AllocaInst* acorn::IRGenerator::gen_alloca(llvm::Type* ll_alloc_type, llvm::Twine ll_name) {
    // LLVM uses preferred alignment for some reason by default. Possibly because they
    // still want to be able to allow for generating IR without setting the data layout.
    //
    // But we always want to use ABI alignment so we generate the instruction manually.

    llvm::Align ll_alignment = get_alignment(ll_alloc_type); // Uses ABI alignment.
    unsigned ll_address_space = ll_module.getDataLayout().getAllocaAddrSpace();

    auto ll_address = new llvm::AllocaInst(ll_alloc_type,
                                           ll_address_space,
                                           nullptr, // "Array size"
                                           ll_alignment);
    builder.Insert(ll_address, llvm::Twine(ll_name));
    return ll_address;
}

llvm::Type* acorn::IRGenerator::gen_function_param_type(Var* param) const {
    if (param->type->is_array()) {
        param->is_aggr_param = true;
        return llvm::PointerType::get(ll_context, 0);
    } else if (param->type->is_struct()) {
        auto struct_type = static_cast<StructType*>(param->type);

        auto ll_aggr_type = gen_type(struct_type);
        uint64_t aggr_mem_size = sizeof_type_in_bytes(ll_aggr_type) * 8;
        if (aggr_mem_size <= ll_module.getDataLayout().getPointerSizeInBits()) {
            // The struct can fit into an integer.
            auto ll_param_type = llvm::Type::getIntNTy(ll_context, static_cast<unsigned int>(next_pow2(aggr_mem_size)));
            return ll_param_type;
        }

        // Will pass a pointer and memcpy over at the call site.
        param->is_aggr_param = true;
        return llvm::PointerType::get(ll_context, 0);
    } else if (param->type->is_slice()) {
        param->is_aggr_param = true;
        return llvm::PointerType::get(ll_context, 0);
    }
    return gen_type(param->type);
}

void acorn::IRGenerator::gen_global_variable_decl(Var* var) {

    if (var->ll_address) {
        return; // Return early because the declaration has already been generated.
    }

    context.queue_gen(var);

    // Have to check for a destructor because unless the global variable
    // is reassigned it will not request generation for the destructor.
    if (var->type->is_struct()) {
        auto struct_type = static_cast<StructType*>(var->type);
        auto structn = struct_type->get_struct();
        if (structn->destructor) {
            context.queue_gen(structn->destructor);
            context.globals_needing_destroyed.push_back(var);
        }
    } else if (var->type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(var->type);
        auto base_type = arr_type->get_base_type();
        if (base_type->is_struct()) {
            auto struct_type = static_cast<StructType*>(base_type);
            auto structn = struct_type->get_struct();
            if (structn->destructor) {
                context.queue_gen(structn->destructor);
                context.globals_needing_destroyed.push_back(var);
            }
        }
    }

    bool is_const = false;
    if (var->type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(var->type);
        if (arr_type->get_base_type()->is_const()) {
            is_const = true;
        }
    }

    auto ll_linkage = var->has_modifier(Modifier::Native) ? llvm::GlobalValue::ExternalLinkage
                                                          : llvm::GlobalValue::InternalLinkage;
    auto ll_name = var->linkname.empty() ? var->name.to_string() : var->linkname;
    auto ll_final_name = var->has_modifier(Modifier::Native) ? ll_name
                                                             : "global." + ll_name + "." + llvm::Twine(context.global_counter++);
    auto ll_address = gen_global_variable(ll_final_name,
                                          gen_type(var->type),
                                          is_const,
                                          nullptr,
                                          ll_linkage);
    ll_address->setAlignment(get_alignment(gen_type(var->type)));

    if (var->has_modifier(Modifier::DllImport)) {
        ll_address->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    }

    var->ll_address = ll_address;

    emit_dbg(var->file->di_emitter->emit_global_variable(var));

}

void acorn::IRGenerator::gen_global_variable_body(Var* var) {
    if (var->has_modifier(Modifier::Native)) return;

    auto ll_global = llvm::cast<llvm::GlobalVariable>(var->ll_address);

    auto gen_constant_value = [this, var]() {
        if (var->assignment->type->is_array()) {
            return gen_constant_array_for_global(static_cast<Array*>(var->assignment));
        } else {
            auto ll_value = gen_rvalue(var->assignment);
            return llvm::cast<llvm::Constant>(ll_value);
        }
    };

    if (!var->should_default_initialize) {
        ll_global->setInitializer(gen_zero(var->type));
        return;
    }

    if (!var->assignment) {
        if (var->type->is_struct()) {
            // Initialize as many fields as possible then post-pone the initialization
            // of the rest of the values in the initialize function.

            auto struct_type = static_cast<StructType*>(var->type);

            llvm::Constant* ll_constant_struct;
            bool all_values_initialized = gen_constant_struct_for_global(struct_type, ll_constant_struct);

            ll_global->setInitializer(ll_constant_struct);
            if (!all_values_initialized) {
                finish_incomplete_global_variable(var);
            }
        } else {
            ll_global->setInitializer(gen_zero(var->type));
        }
        return;
    }

    if (var->assignment->is_foldable) {
        ll_global->setInitializer(gen_constant_value());
    } else {
        ll_global->setInitializer(gen_zero(var->type));
        finish_incomplete_global_variable(var);
    }
}

void acorn::IRGenerator::finish_incomplete_global_variable(Var* var) {
    if (!context.ll_global_init_function) {
        context.ll_global_init_function = gen_void_function_decl("__acorn.global_init");

        // Move the insert point to the call location for the init globals function
        // from the main function.
        builder.SetInsertPoint(context.ll_global_init_call_bb,
                               context.ll_global_init_call_bb->getFirstInsertionPt());
        builder.CreateCall(context.ll_global_init_function);

        gen_bblock("entry", context.ll_global_init_function);
    }

    builder.SetInsertPoint(&context.ll_global_init_function->back());

    if (!var->assignment && var->type->is_struct()) {
        auto struct_type = static_cast<StructType*>(var->type);
        finish_incomplete_struct_type_global(var->ll_address, struct_type);
    } else {
        gen_assignment(var->ll_address, var->type, var->assignment);
    }
}

bool acorn::IRGenerator::gen_constant_struct_for_global(StructType* struct_type, llvm::Constant*& ll_constant_struct) {

    bool all_values_initialized = true;

    auto structn        = struct_type->get_struct();
    auto ll_struct_type = gen_struct_type(struct_type);

    llvm::SmallVector<llvm::Constant*, 16> ll_field_values;

    for (Var* field : structn->fields) {
        if (field->type->is_struct()) {
            auto field_struct_type = static_cast<StructType*>(field->type);
            llvm::Constant* ll_constant;
            all_values_initialized &= gen_constant_struct_for_global(field_struct_type, ll_constant);
            ll_field_values.push_back(ll_constant);
        } else if (field->type->is_array()) {
            if (!field->assignment) {
                ll_field_values.push_back(gen_zero(field->type));
                continue;
            }

            if (field->assignment->is_foldable) {
                auto arr = static_cast<Array*>(field->assignment);
                ll_field_values.push_back(gen_constant_array_for_global(arr));
            } else {
                ll_field_values.push_back(gen_zero(field->type));
                all_values_initialized = false;
            }
        } else if (field->assignment && field->assignment->is_foldable) {
            auto ll_constant = llvm::cast<llvm::Constant>(gen_rvalue(field->assignment));
            ll_field_values.push_back(ll_constant);
        } else {
            ll_field_values.push_back(gen_zero(field->type));
            if (field->assignment) {
                all_values_initialized = false;
            }
        }
    }

    // LLVM expects non empty struct so we need to give it the dummy struct value.
    if (ll_field_values.empty()) {
        ll_field_values.push_back(builder.getInt8(0));
    }

    ll_constant_struct = llvm::ConstantStruct::get(ll_struct_type, ll_field_values);

    return all_values_initialized;
}

llvm::Constant* acorn::IRGenerator::gen_constant_array_for_global(Array* arr) {
    // TODO: This is probably broken for assigning directly to a pointer.
    auto arr_type = static_cast<ArrayType*>(arr->get_final_type());
    auto ll_array_type = llvm::cast<llvm::ArrayType>(gen_type(arr_type));
    return gen_constant_array(arr, arr_type, ll_array_type);
}

void acorn::IRGenerator::finish_incomplete_struct_type_global(llvm::Value* ll_address,
                                                              StructType* struct_type,
                                                              const std::function<llvm::Value* ()>& address_getter) {

    auto structn = struct_type->get_struct();
    auto ll_struct_type = gen_struct_type(struct_type);

    auto get_struct_address = [&ll_address, address_getter]() finline{
        if (!ll_address) {
            ll_address = address_getter();
        }
        return ll_address;
    };

    unsigned field_idx = 0;
    for (Var* field : structn->fields) {
        if (field->type->is_struct()) {
            auto field_struct_type = static_cast<StructType*>(field->type);
            finish_incomplete_struct_type_global(nullptr, field_struct_type, [=, this]() -> llvm::Value* {
                return builder.CreateStructGEP(ll_struct_type, get_struct_address(), field_idx);
            });
        } else if (field->assignment && !field->assignment->is_foldable) {
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, get_struct_address(), field_idx);
            gen_assignment(ll_field_addr, field->type, field->assignment);
        }
        ++field_idx;
    }
}

void acorn::IRGenerator::destroy_global_variables() {
    if (context.globals_needing_destroyed.empty()) {
        return;
    }

    auto ll_func = gen_void_function_decl("__acorn.global_cleanup");

    // Move the insert point to the call location for the cleanup globals function.
    auto& ll_last_inst = context.ll_global_cleanup_call_bb->back();

    if (!llvm::isa<llvm::ReturnInst>(ll_last_inst)) {
        acorn_fatal("Expected the main function to end in a return statement");
    }

    builder.SetInsertPoint(&ll_last_inst);
    builder.CreateCall(ll_func);

    auto ll_entry = gen_bblock("entry", ll_func);
    builder.SetInsertPoint(ll_entry);

    for (Var* var : context.globals_needing_destroyed) {
        gen_call_destructors(var->type, var->ll_address);
    }

    builder.CreateRetVoid();

}

void acorn::IRGenerator::gen_implicit_default_constructor(Struct* structn) {

    cur_struct = structn;
    ll_cur_func = structn->ll_default_constructor;

    auto ll_struct_type = gen_struct_type(structn->struct_type);

    auto ll_entry = gen_bblock("entry", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    auto ll_this = ll_cur_func->getArg(0);

    unsigned field_idx = 0;
    for (; field_idx < structn->fields.size(); field_idx++) {
        Var* field = structn->fields[field_idx];
        auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_this, field_idx);
        if (field->assignment) {
            gen_assignment(ll_field_addr, field->type, field->assignment);
        } else {
            gen_default_value(ll_field_addr, field->type);
        }
    }

    builder.CreateRetVoid();
}

void acorn::IRGenerator::gen_implicit_destructor(Struct* structn) {

    cur_struct = structn;
    ll_cur_func = structn->ll_destructor;

    auto ll_struct_type = gen_struct_type(structn->struct_type);

    auto ll_entry = gen_bblock("entry", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    auto ll_this = ll_cur_func->getArg(0);

    for (Var* field : structn->fields) {
        if (field->type->needs_destruction()) {
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_this, field->field_idx);
            gen_call_destructors(field->type, ll_field_addr);
        }
    }

    builder.CreateRetVoid();
}

void acorn::IRGenerator::gen_implicit_copy_constructor(Struct* structn) {

    cur_struct = structn;
    ll_cur_func = structn->ll_copy_constructor;

    auto ll_struct_type = gen_struct_type(structn->struct_type);

    auto ll_entry = gen_bblock("entry", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    auto ll_this                = ll_cur_func->getArg(0);
    auto ll_from_struct_address = ll_cur_func->getArg(1);

    for (Var* field : structn->fields) {
        copy_struct_field_constructor(field,
                                      ll_this,
                                      ll_from_struct_address,
                                      ll_struct_type);
    }

    builder.CreateRetVoid();
}

void acorn::IRGenerator::gen_implicit_move_constructor(Struct* structn) {

    cur_struct = structn;
    ll_cur_func = structn->ll_move_constructor;

    auto ll_struct_type = gen_struct_type(structn->struct_type);

    auto ll_entry = gen_bblock("entry", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    auto ll_this                = ll_cur_func->getArg(0);
    auto ll_from_struct_address = ll_cur_func->getArg(1);

    for (Var* field : structn->fields) {
        auto ll_to_field_addr   = builder.CreateStructGEP(ll_struct_type, ll_this, field->field_idx);
        auto ll_from_field_addr = builder.CreateStructGEP(ll_struct_type, ll_from_struct_address, field->field_idx);
        try_move_then_copy_struct_field_constructor(field,
                                                    ll_this,
                                                    ll_from_struct_address,
                                                    ll_struct_type);
    }

    builder.CreateRetVoid();
}

void acorn::IRGenerator::add_object_with_destructor(Type* type, llvm::Value* ll_address) {
    if (!encountered_return && !ir_scope->parent) {
        always_initialized_destructor_objects.push_back({ type, ll_address });
    } else {
        ir_scope->objects_needing_destroyed.push_back({ type, ll_address });
    }
}

void acorn::IRGenerator::gen_call_destructors(llvm::SmallVector<DestructorObject>& objects) {
    for (auto itr = objects.rbegin(); itr != objects.rend(); ++itr) {
        auto& object = *itr;
        gen_call_destructors(object.type, object.ll_address);
    }
}

void acorn::IRGenerator::gen_call_destructors(Type* type, llvm::Value* ll_address) {

    auto gen_struct_destructor = [this, ll_address](StructType* struct_type) finline {
        auto structn = struct_type->get_struct();
        if (!structn->ll_destructor) {
            auto name = llvm::Twine("~") + structn->name.to_string();
            structn->ll_destructor =
                gen_no_param_member_function_decl(structn, name + (structn->destructor ? ".acorn" : ".implicit.acorn"));
            if (structn->destructor) {
                structn->destructor->ll_func = structn->ll_destructor;
                context.queue_gen(structn->destructor);
            } else if (!structn->has_requested_gen_implicits) {
                structn->has_requested_gen_implicits = true;
                auto implicit_func = create_implicit_function(ImplicitFunc::ImplicitKind::Destructor, structn);
                context.queue_gen_implicit_function(implicit_func);
            }
        }
    };

    auto create_call = [this, ll_address](Struct* structn) finline {
        builder.CreateCall(structn->ll_destructor, ll_address);
        // TODO: Is this really what we want?
        emit_dbg(di_emitter->emit_location_at_last_statement(builder));
    };

    if (type->is_struct()) {
        auto struct_type = static_cast<StructType*>(type);
        auto structn = struct_type->get_struct();
        gen_struct_destructor(struct_type);
        create_call(structn);
    } else if (type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(type);
        auto base_type = arr_type->get_base_type();
        auto struct_type = static_cast<StructType*>(base_type);
        auto structn = struct_type->get_struct();

        gen_struct_destructor(struct_type);

        // TODO: this code is basically a duplicate of some of the code inside of gen_default_value!
        uint64_t total_linear_length = arr_type->get_total_linear_length();

        auto ll_arr_type = gen_type(type);

        llvm::SmallVector<llvm::Value*> ll_indexes(arr_type->get_depth() + 1, gen_isize(0));
        auto ll_arr_start_ptr    = builder.CreateInBoundsGEP(ll_arr_type, ll_address, ll_indexes);
        auto ll_total_arr_length = gen_isize(total_linear_length);
        gen_abstract_array_loop(base_type,
                                ll_arr_start_ptr,
                                ll_total_arr_length,
                                [this, structn, ll_address, &create_call](auto ll_elm) {
            create_call(structn);
        });
    } else {
        acorn_fatal("Unreachable. Not a destructible type");
    }
}

void acorn::IRGenerator::gen_call_loc_scope_destructors() {
    // Only want to destroy the objects if the scope did not
    // branch because branching handles destruction.
    if (builder.GetInsertBlock()->getTerminator()) {
        return;
    }
    for (auto& object : ir_scope->objects_needing_destroyed) {
        gen_call_destructors(object.type, object.ll_address);
    }
}

void acorn::IRGenerator::process_destructor_state(Type* type, llvm::Value* ll_address) {
    if (type->needs_destruction()) {
        add_object_with_destructor(type, ll_address);
    }
}


// Explaination of returning aggregate types.
//
// The number of different cases involved when returning aggregate
// types is quite a lot so a description of the different cases is
// given here for clarify the IR generation.
//
//
//
//
// ** Returning local non-parameter variables:
//
// Ex.
//
// A foo() {
//     A a;
//     a.j = 5;
//     return a;
// }
//
// When returning a single local variable the IR generation will
// pass the local variable in as a parameter if the aggregate type
// cannot fit into an integer and behavior of returning the variable
// ends up just being a matter of of direct assignment to the parameter.
//
// Ex. the IR generation becomes:
//
// define void @foo.acorn(ptr %aggr.ret.addr) {
// entry:
//   call void @llvm.memset.p0.i64(ptr align 8 %aggr.ret.addr, i8 0, i64 32, i1 false)
//   %0 = getelementptr inbound nuw %A, ptr %aggr.ret.addr, i32 0, i32 0
//   store i64 5, ptr %0, align 8
//   ret void
// }
//
// Otherwise if the local variable can fit into an integer then the variable is
// cast to an integer and returned.
//
//
//
//
//
// ** The case above is distinct from the case in which multiple local variables
//    are returned:
//
// Ex.
//
// A foo(bool t) {
//     if t {
//         A a1;
//         return a1;
//     }
//
//     A a2;
//     return a2;
// }
//
// In this case case there is still a %aggr.ret.addr but each local variable
// must be copied over into this return address.
//
// Ex. the IR generation becomes:
//
// define void @foo.acorn(ptr %aggr.ret.addr, i1 %in.t) {
// entry:
//   %t = alloca i1, align 1
//   store i1 %in.t, ptr %t, align 1
//   %a1 = alloca %A, align 8                 ; local variable 1
//   %a2 = alloca % A, align 8                ; local variable 2
//   %0 = load i1, ptr % t, align 1
//   br i1 % 0, label% if.then, label% if.end
//
// if.then:
//   call void @llvm.memset.p0.i64(ptr align 8 %a1, i8 0, i64 32, i1 false)
//   call void @llvm.memcpy.p0.p0.i64(ptr align 8 %aggr.ret.addr, ptr align 8 %a1, i64 32, i1 false)  ; copies memory from a1 to return address.
//   br label %ret.block
//
// if.end:
//   call void @llvm.memset.p0.i64(ptr align 8 %a2, i8 0, i64 32, i1 false)
//   call void @llvm.memcpy.p0.p0.i64(ptr align 8 %aggr.ret.addr, ptr align 8 %a2, i64 32, i1 false)  ; copies memory from a2 to return address.
//   br label %ret.block
//
// ret.block:
//   ret void
// }
//
//
//
//
//
// ** Similar to the case of returning multiple variables returning a non-local
//    variable will also copy over into the return address:
//
// Ex.
//
// A a; // global variable a.
//
// A foo() {
//     return a; // returning a non-local variable.
// }
//
// This simply copies over the memory into the return address:
//
// Ex. the IR generation becomes:
//
// define void @foo.acorn(ptr %aggr.ret.addr) {
// entry:
//   call void @llvm.memcpy.p0.p0.i64(ptr align 8 %aggr.ret.addr, ptr align 8 @global.a.0, i64 32, i1 false)
//   ret void
// }
//
//
//
//
// ** There is also the case for returning inline aggregate variables.
//
// This case essentially behaves like a returning a local variable where
// if it needs it will pass an %aggr.ret.addr then directly assign or
// will store into a the return address.
//
llvm::Value* acorn::IRGenerator::gen_return(ReturnStmt* ret) {
    encountered_return = true;

    bool not_void = cur_func->return_type->is_not(context.void_type);
    bool is_main = cur_func == context.get_main_function();

    if (cur_func->num_returns > 1) {
        if (not_void && !cur_func->aggr_ret_var) {
            // Not-void so store the return value into the return address
            // if it was not already stored due to an aggregate return variable.
            bool try_move = false;
            if (ret->value->is(NodeKind::IdentRef)) {
                auto ref = static_cast<IdentRef*>(ret->value);
                // Make sure to only move the address if it is a local variable.
                try_move = ref->is_var_ref() && !ref->var_ref->is_global && !ref->var_ref->is_field();
            }

            gen_assignment(ll_ret_addr,
                           cur_func->return_type,
                           ret->value,
                           /* lvalue */ nullptr,
                           /* is assignment op */ false,
                           /* try move */ try_move);
        } else if (is_main) {
            // Special case for main because even if the user declares main as having
            // type void it still must return an integer.
            builder.CreateStore(builder.getInt32(0), ll_ret_addr);
        }

        // Returning so need to destroy all the objects encountered up until this point.
        auto ir_scope_itr = ir_scope;
        while (ir_scope_itr) {
            gen_call_destructors(ir_scope_itr->objects_needing_destroyed);
            ir_scope_itr = ir_scope_itr->parent;
        }

        // Jumping to the return block.
        builder.CreateBr(ll_ret_block);
        emit_dbg(di_emitter->emit_location(builder, ret->loc));
        return nullptr;
    }

    if (not_void) {
        if (cur_func->return_type->is_aggregate()) {
            // If there is an aggregate return variable then the value would
            // have already been stored into the address so there is nothing
            // to do.
            if (cur_func->uses_aggr_param && !cur_func->aggr_ret_var) {
                gen_assignment(ll_ret_addr, cur_func->return_type, ret->value);
            } else if (!cur_func->uses_aggr_param) {
                // Checking if the function returns a temporary object because
                // if it does then we have to make sure not to call the destructor
                // since the inline object would effectively be owned by the caller.
                //
                // If we simply called gen_node then the generation code would create
                // a temporary object and request that the object be destroyed at the
                // end of the scope.
                //
                llvm::Value* ll_value;
                if (ret->value->is(NodeKind::StructInitializer) ||
                    ret->value->is(NodeKind::Array)) {
                    ll_value = gen_unseen_alloca(cur_func->return_type, "tmp.inline.aggr");
                    gen_assignment(ll_value, cur_func->return_type, ret->value);
                } else if (ret->value->is(NodeKind::FuncCall)) {
                    // Treating the function call case as a special case since the function
                    // call may return an integer representation in which case there is no
                    // reason to create a temporary object except if the type has a destructor.
                    auto call = static_cast<FuncCall*>(ret->value);
                    llvm::Value* ll_tmp_address = nullptr;
                    if (cur_func->return_type->needs_destruction()) {
                        ll_tmp_address = gen_unseen_alloca(cur_func->return_type, "tmp.inline.aggr");
                    }
                    ll_value = gen_function_call(call, ll_tmp_address);
                } else {
                    ll_value = gen_node(ret->value);
                    // Checking if the aggregate needs it's copy constructor called.
                    // Since it does not use an aggregate return address the ll_value is
                    // essentially an integer so normally it is possible to just load
                    // the aggregate value as an integer but if it has a copy constructor
                    // then the copy constructor still needs called.
                    if (!cur_func->aggr_ret_var) {
                        if (cur_func->return_type->is_struct()) {
                            auto struct_type = static_cast<StructType*>(cur_func->return_type);
                            auto structn = struct_type->get_struct();

                            if (structn->needs_copy_call) {
                                auto ll_struct_type = gen_struct_type(struct_type);
                                auto ll_tmp_struct  = gen_unseen_alloca(ll_struct_type, "tmp.struct.ret");
                                gen_copy_struct(ll_tmp_struct, ll_value, struct_type);
                                ll_value = ll_tmp_struct;
                            }
                        } else if (cur_func->return_type->is_array()) {
                            auto arr_type = static_cast<ArrayType*>(cur_func->return_type);
                            auto base_type = arr_type->get_base_type();

                            if (base_type->is_struct()) {
                                auto ll_arr_type = gen_type(arr_type);
                                auto struct_type = static_cast<StructType*>(base_type);
                                auto structn = struct_type->get_struct();

                                auto ll_tmp_array = gen_unseen_alloca(ll_arr_type, "tmp.arr.ret");
                                if (structn->needs_copy_call) {
                                    gen_call_array_copy_constructors(ll_tmp_array,
                                                                     ll_value,
                                                                     arr_type,
                                                                     structn);
                                    ll_value = ll_tmp_array;
                                }
                            }
                        } else {
                            acorn_fatal("unreachable. not a valid aggregate type");
                        }
                    }
                }

                if (!ll_value->getType()->isIntegerTy()) {
                    ll_value = builder.CreateLoad(cur_func->ll_aggr_int_ret_type, ll_value);
                }
                ll_ret_value = ll_value;
            }
            // else we may just be using an aggregate return variable that is just a local
            // variable to the function and does not need returned since it's memory is passed
            // in.
            //
            // Ex.
            //
            // struct A { int64 a, b, c, d, e;  }
            //
            // A foo() {
            //     A a; // Local to the function but actually becomes a parameter
            //          // for returning.
            //
            //     return a;
            // }
            //
        } else {
            // Return non-aggregate value.
            ll_ret_value = gen_rvalue(ret->value);
        }
    } else if (is_main) {
        // Special case for main. Read above for explaination.
        ll_ret_value = builder.getInt32(0);
    }

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_if(IfStmt* ifs, llvm::BasicBlock* ll_end_bb) {

    auto load_variable_cond = [this, ifs](Var* var) finline -> llvm::Value* {

        if (!var->is_foldable) {
            gen_variable(var); // Generate assignment.
        }

        if (ifs->post_variable_cond) {
            return gen_condition(static_cast<Expr*>(ifs->post_variable_cond));
        }

        auto ll_value = var->is_foldable ? gen_node(var->assignment)
                                         : builder.CreateLoad(gen_type(var->type), var->ll_address);
        if (var->type->is_real_pointer()) {
            return builder.CreateIsNotNull(ll_value);
        }
        return ll_value;
    };

    auto ll_then_bb = gen_bblock("if.then");
    ll_end_bb = ll_end_bb ? ll_end_bb : gen_bblock("if.end");
    auto ll_else_bb = ifs->elseif ? gen_bblock("if.else") : ll_end_bb;

    push_scope();

    // Jump to either the then or else block depending on the condition.
    if (ifs->cond->is(NodeKind::Var)) {
        auto ll_cond = load_variable_cond(static_cast<Var*>(ifs->cond));
        builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);
        emit_dbg(di_emitter->emit_location(builder, ifs->loc));
    } else {
        gen_branch_on_condition(static_cast<Expr*>(ifs->cond), ll_then_bb, ll_else_bb);
    }

    // Insert the then and else blocks after the condition because the condition might
    // branch.
    insert_bblock_at_end(ll_then_bb);
    builder.SetInsertPoint(ll_then_bb);
    gen_scope(ifs->scope);

    // Jump to end after the else conidtion block.
    //
    // Need to use gen_branch_if_not_term because our scope may
    // have ended in a return statement or some other form of jump.
    gen_branch_if_not_term_with_dbg_loc(ll_end_bb, ifs->scope->end_loc);

    pop_scope();

    if (Node* elif = ifs->elseif) {
        // Insert after the if.then because it might create more blocks.
        insert_bblock_at_end(ll_else_bb);

        builder.SetInsertPoint(ll_else_bb);
        if (elif->is(NodeKind::IfStmt)) {
            // Pass the ll_end_bb so that all the if statements jump to
            // the same ll_end_bb. This is actually needed to generate
            // proper debugging information because if you have multiple
            // end blocks and join them together through multiple jumps
            // the the debugger will just get confused about control flow.
            //
            gen_if(static_cast<IfStmt*>(elif), ll_end_bb);
        } else {
            // ** If the if statement ends in an else then we have
            // to create an unconditional branch to the end of
            // the if statement since an else statement is represented
            // by a ScopeStmt which does not branch.
            //
            // Ex.
            //
            // void main() {
            //     ...
            //     if cond {
            //         ...
            //     } else {
            //         ...
            //     } // Here we would branch out of of the else block.
            //     ...
            // }
            //
            // The codegen becomes:
            //
            // define @main() {
            // entry:
            //   ...
            //   %gt = icmp ...
            //   br i1 %gt, label %if.then, label %if.else
            //
            // if.then:
            //   br label %if.end
            //
            // if.else:
            //   br %if.end   ; Here we need a branch to %if.end
            //
            // if.end:
            //   ret i32 0
            // }
            //
            auto else_scope = static_cast<ScopeStmt*>(elif);
            gen_scope(else_scope);
            gen_branch_if_not_term_with_dbg_loc(ll_end_bb, else_scope->end_loc);
        }
    }

    // Continue with the end block after our if statement.
    builder.SetInsertPoint(ll_end_bb);

    // Make sure to insert the end after all the other blocks!
    if (!ifs->elseif || ifs->elseif->is(NodeKind::ScopeStmt)) {
        insert_bblock_at_end(ll_end_bb);
    }

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_predicate_loop(PredicateLoopStmt* loop) {

    auto ll_cond_bb = gen_bblock("loop.cond", ll_cur_func);
    auto ll_body_bb = gen_bblock("loop.body", ll_cur_func);
    auto ll_end_bb  = gen_bblock("loop.end");

    loop_break_stack.push_back(ll_end_bb);
    loop_continue_stack.push_back(ll_cond_bb);

    builder.CreateBr(ll_cond_bb);
    builder.SetInsertPoint(ll_cond_bb);

    push_scope();
    ir_scope->is_loop_scope = true;

    if (loop->cond) {
        gen_cond_branch_for_loop(loop->cond, ll_body_bb, ll_end_bb);
    } else {
        builder.CreateBr(ll_body_bb);
    }
    // Debug location of the conditional branch for branching to loop.body or loop.end.
    emit_dbg(di_emitter->emit_location(builder, loop->loc));

    builder.SetInsertPoint(ll_body_bb);
    emit_dbg(di_emitter->emit_scope_start(loop->scope->loc));
    gen_scope(loop->scope);
    emit_dbg(di_emitter->emit_scope_end());

    pop_scope();

    loop_break_stack.pop_back();
    loop_continue_stack.pop_back();

    // End of loop scope, jump back to the condition.
    gen_branch_if_not_term_with_dbg_loc(ll_cond_bb, loop->scope->end_loc);

    // Continue generating code after the the loop.
    builder.SetInsertPoint(ll_end_bb);

    // Make sure the end block goes after all the other blocks!
    insert_bblock_at_end(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_range_loop(RangeLoopStmt* loop) {

    auto ll_inc_bb = loop->inc ? gen_bblock("loop.inc", ll_cur_func) : nullptr;
    auto ll_cond_bb = gen_bblock("loop.cond", ll_cur_func);
    auto ll_body_bb = gen_bblock("loop.body", ll_cur_func);
    auto ll_end_bb = gen_bblock("loop.end");
    auto ll_continue_bb = ll_inc_bb ? ll_inc_bb : ll_cond_bb;

    loop_break_stack.push_back(ll_end_bb);
    loop_continue_stack.push_back(ll_continue_bb);

    // Emit early so it thinks the variable node is effectively part of the loop scope.
    emit_dbg(di_emitter->emit_scope_start(loop->scope->loc));
    push_scope();
    ir_scope->is_loop_scope = true;

    if (loop->init_node) {
        gen_node(loop->init_node);
    }

    builder.CreateBr(ll_cond_bb);
    builder.SetInsertPoint(ll_cond_bb);

    gen_cond_branch_for_loop(loop->cond, ll_body_bb, ll_end_bb);
    // Debug location of the conditional branch for branching to loop.body or loop.end.
    emit_dbg(di_emitter->emit_location(builder, loop->loc));

    // We need to keep the increment logic up here because otherwise it gets confused and thinks
    // it is part of a debug lexical scope that exists after the loop and cause double stepping
    // and other nonsense.
    if (loop->inc) {
        builder.SetInsertPoint(ll_inc_bb);
        gen_node(loop->inc);
        builder.CreateBr(ll_cond_bb);
        emit_dbg(di_emitter->emit_location(builder, loop->scope->loc));
    }

    builder.SetInsertPoint(ll_body_bb);

    gen_scope(loop->scope);
    emit_dbg(di_emitter->emit_scope_end());

    pop_scope();

    loop_break_stack.pop_back();
    loop_continue_stack.pop_back();

    // End of loop scope, jump back to condition or increment.
    gen_branch_if_not_term_with_dbg_loc(ll_continue_bb, loop->scope->end_loc); // This is actually needed to properly step into the closing }.

    // Continue generating code after the the loop.
    builder.SetInsertPoint(ll_end_bb);

    // Make sure the end block goes after all the other blocks!
    insert_bblock_at_end(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_iterator_loop(IteratorLoopStmt* loop) {

    auto ll_inc_bb  = gen_bblock("loop.inc", ll_cur_func);
    auto ll_cond_bb = gen_bblock("loop.cond", ll_cur_func);
    auto ll_body_bb = gen_bblock("loop.body", ll_cur_func);
    auto ll_end_bb = gen_bblock("loop.end");

    loop_break_stack.push_back(ll_end_bb);
    loop_continue_stack.push_back(ll_inc_bb);

    push_scope();
    ir_scope->is_loop_scope = true;

    if (loop->container->type->is_array()) {

        // Calculate beginning and end of the array for determining
        // the stop condition later.
        //
        auto ll_ptr_type = builder.getPtrTy();
        auto arr_type = static_cast<ArrayType*>(loop->container->type);
        auto elm_type = arr_type->get_elm_type();
        auto ll_elm_type = gen_type(elm_type);
        auto ll_arr_itr_ptr = gen_unseen_alloca(ll_ptr_type, "arr.itr.ptr");

        auto ll_arr_length = gen_isize(arr_type->get_length());
        auto ll_arr_type = gen_type(arr_type);

        auto ll_arr_beg = gen_node(loop->container);

        auto ll_arr_end = gen_array_memory_access(ll_arr_beg, gen_type(arr_type), ll_arr_length);
        builder.CreateStore(ll_arr_beg, ll_arr_itr_ptr);

        // Branch into the condition block and determine when to stop
        // iterating.
        //
        builder.CreateBr(ll_cond_bb);
        builder.SetInsertPoint(ll_cond_bb);

        auto ll_ptr_index1 = builder.CreateLoad(ll_ptr_type, ll_arr_itr_ptr);
        auto ll_cond = builder.CreateICmpNE(ll_ptr_index1, ll_arr_end);

        builder.CreateCondBr(ll_cond, ll_body_bb, ll_end_bb);
        emit_dbg(di_emitter->emit_location(builder, loop->loc));

        // Generate code for incrementing the array pointer.
        //
        builder.SetInsertPoint(ll_inc_bb);


        auto ll_ptr_index2 = builder.CreateLoad(ll_ptr_type, ll_arr_itr_ptr);
        auto ll_ptr_next = builder.CreateInBoundsGEP(ll_elm_type, ll_ptr_index2, gen_isize(1));
        builder.CreateStore(ll_ptr_next, ll_arr_itr_ptr);
        // We need to output the location here so that when it jumps to the increment block it
        // does not get confused and not know where it is at.
        emit_dbg(di_emitter->emit_location(builder, loop->loc));

        builder.CreateBr(ll_cond_bb);
        emit_dbg(di_emitter->emit_location(builder, loop->loc));

        builder.SetInsertPoint(ll_body_bb);
        emit_dbg(di_emitter->emit_scope_start(loop->scope->loc));

        // Store the pointer value into the variable.
        auto ll_ptr_index3 = builder.CreateLoad(ll_ptr_type, ll_arr_itr_ptr);
        auto ll_index_value = ll_ptr_index3;
        if (!loop->references_memory && !loop->var->type->is_aggregate()) {
            ll_index_value = builder.CreateLoad(ll_elm_type, ll_ptr_index3);
        }

        emit_dbg(di_emitter->emit_function_variable(loop->var, builder));
        if (loop->var->type->is_struct()) {
            auto struct_type = static_cast<StructType*>(loop->var->type);
            gen_copy_struct(loop->var->ll_address, ll_index_value, struct_type);
            emit_dbg(di_emitter->emit_location(builder, loop->var->loc));
        } else if (loop->var->type->is_array()) {
            auto elm_arr_type = static_cast<ArrayType*>(loop->var->type);
            gen_copy_array(loop->var->ll_address, ll_index_value, elm_arr_type, loop->var);
        } else {
            builder.CreateStore(ll_index_value, loop->var->ll_address);
            emit_dbg(di_emitter->emit_location(builder, loop->var->loc));
        }
    } else if (loop->container->type->is_range()) {

        auto range_type = static_cast<RangeType*>(loop->container->type);
        auto value_type = range_type->get_value_type();
        auto range = static_cast<BinOp*>(loop->container);

        auto ll_one = gen_one(value_type);

        // Storing the beginning index of the range.
        //
        builder.CreateStore(gen_rvalue(range->lhs), loop->var->ll_address);

        // Calculating the end index. Adding one to the index if checking
        // when equal since it is possible the range starts past the end
        // so we want to compare less than in all cases. Otherwise it is
        // possible to end up in an infinite loop.
        //
        llvm::Value* ll_compare_index = gen_rvalue(range->rhs);
        if (range->op == Token::RangeEq) {
            ll_compare_index = builder.CreateNSWAdd(ll_compare_index, ll_one);
        }

        // Branch into the condition block and determine when to stop
        // iterating.
        //
        builder.CreateBr(ll_cond_bb);
        builder.SetInsertPoint(ll_cond_bb);

        llvm::Type*  ll_index_type = gen_type(value_type);
        llvm::Value* ll_index      = builder.CreateLoad(ll_index_type, loop->var->ll_address);

        auto ll_cond = value_type->is_signed() ? builder.CreateICmpSLT(ll_index, ll_compare_index)
                                               : builder.CreateICmpULT(ll_index, ll_compare_index);
        builder.CreateCondBr(ll_cond, ll_body_bb, ll_end_bb);
        emit_dbg(di_emitter->emit_location(builder, loop->loc));

        // Generate code for incrementing the index.
        //
        builder.SetInsertPoint(ll_inc_bb);

        ll_index = builder.CreateLoad(ll_index_type, loop->var->ll_address);
        ll_index = builder.CreateNSWAdd(ll_index, ll_one);
        // We need to output the location here so that when it jumps to the increment block it
        // does not get confused and not know where it is at.
        //emit_dbg(di_emitter->emit_location(builder, loop->loc));

        builder.CreateStore(ll_index, loop->var->ll_address);
        emit_dbg(di_emitter->emit_location(builder, loop->var->loc));

        builder.CreateBr(ll_cond_bb);
        emit_dbg(di_emitter->emit_location(builder, loop->loc));

        builder.SetInsertPoint(ll_body_bb);
        emit_dbg(di_emitter->emit_scope_start(loop->scope->loc));

        emit_dbg(di_emitter->emit_function_variable(loop->var, builder));

    } else {
        acorn_fatal("unreachable iteration type");
    }

    gen_scope(loop->scope);
    emit_dbg(di_emitter->emit_scope_end());

    pop_scope();

    loop_break_stack.pop_back();
    loop_continue_stack.pop_back();

    // End of loop scope, jump back to increment.
    //gen_branch_if_not_term(ll_inc_bb);
    gen_branch_if_not_term_with_dbg_loc(ll_inc_bb, loop->scope->end_loc);

    // Continue generating code after the the loop.
    builder.SetInsertPoint(ll_end_bb);

    // Make sure the end block goes after all the other blocks!
    insert_bblock_at_end(ll_end_bb);

    return nullptr;
}

void acorn::IRGenerator::gen_cond_branch_for_loop(Expr* cond, llvm::BasicBlock* ll_body_bb, llvm::BasicBlock* ll_end_bb) {
    auto ll_cond = cond ? gen_condition(cond) : builder.getTrue();
    builder.CreateCondBr(ll_cond, ll_body_bb, ll_end_bb);
}

llvm::Value* acorn::IRGenerator::gen_loop_control(LoopControlStmt* loop_control) {
    auto& target_stack = loop_control->is(NodeKind::BreakStmt) ? loop_break_stack : loop_continue_stack;

    // Calling destructors for every loop scope the break or continue
    // jumps out of.
    auto scope_itr = ir_scope;
    while (scope_itr) {
        gen_call_destructors(scope_itr->objects_needing_destroyed);

        if (scope_itr->is_loop_scope) {
            break;
        }

        scope_itr = scope_itr->parent;
    }

    llvm::BasicBlock* target_bb = target_stack.back();
    builder.CreateBr(target_bb);
    emit_dbg(di_emitter->emit_location(builder, loop_control->loc));

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_switch(SwitchStmt* switchn) {
    if (switchn->all_conds_foldable) {
        return gen_switch_foldable(switchn);
    }
    return gen_switch_non_foldable(switchn);
}

llvm::Value* acorn::IRGenerator::gen_switch_non_foldable(SwitchStmt* switchn) {

    if (should_emit_debug_info) {
        // Emitting a nop so the user can still debug at the start of the switch.
        gen_nop();
        di_emitter->emit_location(builder, switchn->loc);
    }

    auto ll_end_bb = gen_bblock("sw.end");

    auto ll_value = gen_rvalue(switchn->on);

    auto add_cases = [this, switchn, ll_value]
        (SwitchCase scase, llvm::BasicBlock* ll_then_bb, llvm::BasicBlock* ll_else_bb) finline {
        // Jump to either the then or else block depending on the condition.
        if (scase.cond->type->is_ignore_const(context.bool_type)) {
            // It is an operation that results in a boolean so branch on that condition.
            gen_branch_on_condition(static_cast<Expr*>(scase.cond), ll_then_bb, ll_else_bb);
        } else if (scase.cond->type->is_range()) {
            auto range = static_cast<BinOp*>(scase.cond);
            auto range_type = static_cast<RangeType*>(range->type);
            auto value_type = range_type->get_value_type();

            llvm::Value* ll_lhs = nullptr, *ll_rhs = nullptr;
            if (value_type->is_signed()) {
                ll_lhs = builder.CreateICmpSGE(ll_value, gen_rvalue(range->lhs), "gte");
                if (range->op == Token::RangeEq) {
                    ll_rhs = builder.CreateICmpSLE(ll_value, gen_rvalue(range->rhs), "lte");
                } else {
                    ll_rhs = builder.CreateICmpSLT(ll_value, gen_rvalue(range->rhs), "lt");
                }
            } else {
                ll_lhs = builder.CreateICmpUGE(ll_value, gen_rvalue(range->lhs), "gte");
                if (range->op == Token::RangeEq) {
                    ll_rhs = builder.CreateICmpULE(ll_value, gen_rvalue(range->rhs), "lte");
                } else {
                    ll_rhs = builder.CreateICmpULT(ll_value, gen_rvalue(range->rhs), "lt");
                }
            }
            auto ll_cond = builder.CreateOr(ll_lhs, ll_rhs, "or");

            builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);
            emit_dbg(di_emitter->emit_location(builder, scase.cond->loc));
        } else {
            auto ll_cond = gen_equal(ll_value, gen_rvalue(scase.cond));
            builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);
            emit_dbg(di_emitter->emit_location(builder, scase.cond->loc));
        }
    };

    // TODO: The code to debug visualization is still a bit off.
    for (size_t case_count = 0; case_count < switchn->cases.size(); case_count++) {

        SwitchCase scase = switchn->cases[case_count];

        bool is_default_scope = scase.scope == switchn->default_scope;
        bool is_last = case_count == switchn->cases.size() - 1;

        auto ll_then_bb = is_default_scope ? builder.GetInsertBlock() : gen_bblock("sw.then");

        llvm::BasicBlock* ll_else_bb = nullptr;
        if (scase.scope->empty() && !is_last) {
            // As long as the cases are empty we can just generate the
            // cases to point at a single sw.then block.
            do {

                ll_else_bb = is_last ? ll_end_bb : gen_bblock("sw.else");
                add_cases(scase, ll_then_bb, ll_else_bb);
                if (!is_last) {
                    insert_bblock_at_end(ll_else_bb);
                    builder.SetInsertPoint(ll_else_bb);
                }

                scase = switchn->cases[++case_count];
                is_last = case_count == switchn->cases.size() - 1;
                if (!scase.scope->empty() || is_last) {
                    break;
                }
            } while (true);
        }

        if (is_default_scope) {
            ll_then_bb->setName("sw.default");
        } else {
            ll_else_bb = is_last ? ll_end_bb : gen_bblock("sw.else");
            add_cases(scase, ll_then_bb, ll_else_bb);
            if (!is_last)
                insert_bblock_at_end(ll_else_bb);

            // Insert the sw.then block after all the switch cases that may jump to it.
            insert_bblock_at_end(ll_then_bb);
        }

        // Generating the switch scope.
        builder.SetInsertPoint(ll_then_bb);
        gen_scope_with_dbg_scope(scase.scope);
        gen_branch_if_not_term_with_dbg_loc(ll_end_bb, scase.scope->end_loc);

        // Continue generating the rest of the switch.
        builder.SetInsertPoint(ll_else_bb);

    }

    builder.SetInsertPoint(ll_end_bb);

    // Make sure the end block goes after all the other blocks!
    insert_bblock_at_end(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_switch_foldable(SwitchStmt* switchn) {

    auto ll_end_bb = gen_bblock("sw.end");

    size_t num_cases = switchn->cases.size() + (switchn->default_scope ? 1 : 0);
    auto ll_default_bb = switchn->default_scope ? gen_bblock("sw.default") : ll_end_bb;
    auto ll_switch = builder.CreateSwitch(gen_rvalue(switchn->on),
                                          ll_default_bb,
                                          static_cast<unsigned int>(num_cases));

    // Allow breaking on the switch.
    emit_dbg(di_emitter->emit_location(builder, switchn->loc));

    // The default scope is forced to be at the end so we can
    // just safely add it here.
    if (switchn->default_scope) {
        insert_bblock_at_end(ll_default_bb);
    }

    auto add_cases = [this, ll_switch](SwitchCase scase, llvm::BasicBlock* ll_case_bb) finline {
        if (scase.cond->type->is_range()) {
            auto range = static_cast<BinOp*>(scase.cond);
            auto range_type = static_cast<RangeType*>(scase.cond->type);
            auto value_type = range_type->get_value_type();

            auto ll_int_type = llvm::Type::getIntNTy(context.get_ll_context(), value_type->get_number_of_bits());
            iterate_over_range_values(range, [ll_int_type, ll_case_bb, ll_switch, value_type](uint64_t value) {
                auto ll_case_value = llvm::ConstantInt::get(ll_int_type, value, value_type->is_signed());
                ll_switch->addCase(ll_case_value, ll_case_bb);
            });
        } else {
            auto ll_case_value = llvm::cast<llvm::ConstantInt>(gen_rvalue(scase.cond));
            ll_switch->addCase(ll_case_value, ll_case_bb);
        }
    };

    for (size_t case_count = 0; case_count < switchn->cases.size(); case_count++) {

        SwitchCase scase = switchn->cases[case_count];

        bool is_last = case_count == switchn->cases.size() - 1;

        auto ll_case_bb = gen_bblock("sw.case", ll_cur_func);

        if (scase.scope->empty() && !is_last) {
            // As long as the cases are empty we can just generate the
            // cases to point at a single sw.case block.
            do {

                add_cases(scase, ll_case_bb);

                scase = switchn->cases[++case_count];
                is_last = case_count == switchn->cases.size() - 1;
                if (!scase.scope->empty() || is_last) {
                    break;
                }
            } while (true);
        }

        bool is_default_scope = scase.scope == switchn->default_scope;
        if (is_default_scope) {
            auto ll_original_bb = ll_case_bb;
            ll_case_bb->replaceAllUsesWith(ll_default_bb);
            ll_case_bb = ll_default_bb;
            ll_original_bb->removeFromParent();
        } else {
            add_cases(scase, ll_case_bb);
        }

        builder.SetInsertPoint(ll_case_bb);

        // Insert a new lexical debug scope per switch scope because we want to treat
        // the variables within the scopes as self contained.
        gen_scope_with_dbg_scope(scase.scope);
        gen_branch_if_not_term_with_dbg_loc(ll_end_bb, scase.scope->end_loc);

    }

    builder.SetInsertPoint(ll_end_bb);

    // Make sure the end block goes after all the other blocks!
    insert_bblock_at_end(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_struct_initializer(StructInitializer* initializer, llvm::Value* ll_dest_addr, Node* lvalue) {

    auto ll_struct_type = gen_type(initializer->type);
    if (!ll_dest_addr) {
        ll_dest_addr = gen_unseen_alloca(ll_struct_type, "tmp.struct");
        process_destructor_state(initializer->type, ll_dest_addr);
    }

    if (initializer->called_constructor) {

        Func* called_func = initializer->called_constructor;
        gen_function_decl(called_func);

        return gen_function_decl_call(called_func,
                                      initializer->loc,
                                      initializer->values,
                                      nullptr,
                                      ll_dest_addr,
                                      false,
                                      lvalue);
    }

    if (initializer->values.empty()) {
        gen_default_value(ll_dest_addr, initializer->type, lvalue);
        return ll_dest_addr;
    }

    auto structn = initializer->structn;

    llvm::SmallVector<bool, 32> fields_set_list(structn->fields.size(), false);

    unsigned field_idx = 0;
    for (Expr* value : initializer->values) {
        if (value->is(NodeKind::NamedValue)) {
            auto named_val = static_cast<NamedValue*>(value);
            Var* field = structn->fields[named_val->mapped_idx];
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_dest_addr, named_val->mapped_idx);
            gen_assignment(ll_field_addr, field->type, named_val->assignment, lvalue);

            fields_set_list[named_val->mapped_idx] = true;
        } else {
            Var* field = structn->fields[field_idx];
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_dest_addr, field_idx);
            gen_assignment(ll_field_addr, field->type, value, lvalue);

            fields_set_list[field_idx] = true;
        }
        ++field_idx;
    }

    // Filling in remainder fields that were not set.
    if (initializer->non_named_vals_offset == -1) {
        initializer->non_named_vals_offset = 0;
    }
    for (field_idx = initializer->non_named_vals_offset; field_idx < structn->fields.size(); field_idx++) {
        if (!fields_set_list[field_idx]) {
            Var* field = structn->fields[field_idx];
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_dest_addr, field_idx);
            if (field->assignment) {
                gen_assignment(ll_field_addr, field->type, field->assignment, lvalue);
            } else {
                gen_default_value(ll_field_addr, field->type, lvalue);
            }
        }
    }

    return ll_dest_addr;
}

llvm::Value* acorn::IRGenerator::gen_this(This* thisn) {
    return ll_this;
}

llvm::Value* acorn::IRGenerator::gen_sizeof(SizeOf* sof) {
    return builder.getInt32((uint32_t) sizeof_type_in_bytes(gen_type(sof->type_with_size)));
}

void acorn::IRGenerator::gen_scope(ScopeStmt* scope) {
    for (Node* stmt : *scope) {
        if (should_emit_debug_info) {
            di_emitter->set_last_statement(stmt);
        }
        gen_node(stmt);
    }
}

llvm::Value* acorn::IRGenerator::gen_scope_with_dbg_scope(ScopeStmt* scope) {
    emit_dbg(di_emitter->emit_scope_start(scope->loc));
    push_scope();
    gen_scope(scope);
    pop_scope();
    emit_dbg(di_emitter->emit_scope_end());
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_variable(Var* var) {
    if (var->is_foldable) return nullptr; // Nothing to generate since the variable doesn't have an address.

    if (var != cur_func->aggr_ret_var) {
         process_destructor_state(var->type, var->ll_address);
    }

    if (should_emit_debug_info) {
        di_emitter->emit_function_variable(var, builder);
    }

    if (var->assignment) {
        gen_assignment(var->ll_address, var->type, var->assignment, var);
    } else if (var->should_default_initialize) {
        gen_default_value(var->ll_address, var->type);
    }

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_number(Number* number) {
    auto type_kind = number->type->get_kind();
    if (type_kind == TypeKind::Float64) {
        return llvm::ConstantFP::get(ll_context, llvm::APFloat(number->value_f64));
    } else if (type_kind == TypeKind::Float32) {
        return llvm::ConstantFP::get(ll_context, llvm::APFloat(number->value_f32));
    }

    auto int_bit_size = number->type->get_number_of_bits();
    return llvm::ConstantInt::get(ll_context, llvm::APInt(int_bit_size, number->value_u64, true));
}

llvm::Value* acorn::IRGenerator::gen_ident_reference(IdentRef* ref) {
    if (ref->is_var_ref()) {
        Var* var = ref->var_ref;

        if (var->is_foldable) {
            // The type is a very basic foldable type with a foldable
            // assignment so there is no code associated with the variable
            // and the value of the assignment is returned instead.

            if (var->ll_comptime_value) {
                return var->ll_comptime_value;
            }

            auto ll_value = gen_rvalue(var->assignment);
            var->ll_comptime_value = ll_value;
            return ll_value;
        }

        if (var->is_field()) {
            auto ll_struct_type = gen_type(cur_struct->struct_type);
            return builder.CreateStructGEP(ll_struct_type, ll_this, var->field_idx);
        }

        if (var->is_global) {
            gen_global_variable_decl(var);
        }

        return var->ll_address;
    } else if (ref->is_universal_ref()) {
        return gen_rvalue(ref->universal_ref);
    } else if (ref->is_funcs_ref()) {
        auto func = (*ref->funcs_ref)[0];
        gen_function_decl(func);

        return func->ll_func;
    } else if (ref->is_enum_value_ref()) {
        auto enum_type = static_cast<EnumType*>(ref->type);
        return gen_enum_index(ref->enum_value_ref->index, enum_type->get_index_type());
    }

    acorn_fatal("unreachable: gen_ident_reference()");
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_function_call(FuncCall* call, llvm::Value* ll_dest_addr, Node* lvalue) {

    bool call_func_type = call->site->type->is_function();
    if (call->site->type->is_function()) {
        return gen_function_type_call(call, ll_dest_addr, lvalue);
    } else {
        Func* called_func = call->called_func;
        gen_function_decl(called_func);

        if (called_func->ll_intrinsic_id != llvm::Intrinsic::not_intrinsic) {
            auto ll_ret = gen_intrinsic_call(call);
            if (should_emit_debug_info) {
                // LLVM instrinsics can behave very weirdly and replace the call
                // with basic instructions so attaching the location to the intrinisc
                // call does not actually work. We will create a NOP instruction so
                // the debugger can set a breakpoint.
                gen_nop();
                di_emitter->emit_location(builder, call->loc);
            }
            if (ll_dest_addr) {
                builder.CreateStore(ll_ret, ll_dest_addr);
            }
            return ll_ret;
        }

        // When a member function is called from a variable the
        // function call has the dot operator as its child. Otherwise
        // the member function must be a call from the current function
        // where the current function is a member function of the same
        // struct.
        //
        llvm::Value* ll_in_this = nullptr;
        if (call->called_func->structn) {
            if (call->site->is(NodeKind::DotOperator)) {
                auto dot_operator = static_cast<DotOperator*>(call->site);
                ll_in_this = gen_node(dot_operator->site);

                if (dot_operator->site->type->is_pointer() && is_pointer_lvalue(dot_operator->site)) {
                    // The function call auto-dereferences the pointer.
                    ll_in_this = builder.CreateLoad(builder.getPtrTy(), ll_in_this);
                } else if (dot_operator->site->is(NodeKind::FuncCall)) {
                    auto dot_operator_call = static_cast<FuncCall*>(dot_operator->site);
                    ll_in_this = gen_handle_returned_aggregate_obj(dot_operator_call, ll_in_this, "tmp.struct");
                }
            } else {
                ll_in_this = ll_this;
            }
        }

        return gen_function_decl_call(called_func,
                                      call->loc,
                                      call->args,
                                      ll_dest_addr,
                                      ll_in_this,
                                      call->implicitly_converts_return,
                                      lvalue);
    }
}

llvm::Value* acorn::IRGenerator::gen_function_call_arg(Expr* arg) {

    bool try_move = false;
    if (arg->is(NodeKind::MoveObj)) {
        try_move = true;
        auto move_obj = static_cast<MoveObj*>(arg);
        arg = move_obj->value;
    }

    if (!context.should_stand_alone()) {
        if (arg->cast_type && arg->cast_type->is(context.std_any_struct_type)) {
            return gen_rvalue(arg);
        }
    }

    if (arg->type->is_struct()) {

        if (arg->is(NodeKind::MoveObj)) {
            auto move_obj = static_cast<MoveObj*>(arg);
            auto ll_tmp = gen_unseen_alloca(arg->type, "tmp.move.obj");
            gen_assignment(ll_tmp, arg->type, move_obj->value, nullptr, false, true);
        }

        auto struct_type = static_cast<StructType*>(arg->type);
        auto ll_struct_type = gen_struct_type(struct_type);
        uint64_t aggr_mem_size_bytes = sizeof_type_in_bytes(ll_struct_type);
        uint64_t aggr_mem_size_bits = aggr_mem_size_bytes * 8;

        bool uses_optimized_int_passing = aggr_mem_size_bits <= ll_module.getDataLayout().getPointerSizeInBits();

        auto finish_aggregate_arg = [this, uses_optimized_int_passing, aggr_mem_size_bits]
            (auto ll_tmp_arg) finline -> llvm::Value* {
            if (uses_optimized_int_passing) {
                // The struct can fit into an integer.
                auto ll_int_type = llvm::Type::getIntNTy(ll_context, static_cast<unsigned int>(next_pow2(aggr_mem_size_bits)));
                return builder.CreateLoad(ll_int_type, ll_tmp_arg, "opt.int.tmp.arg");
            }
            return ll_tmp_arg;
        };

        // First checking for if it is an rvalue since if it is
        // then there is no reason to make a copy of the argument
        // since it is temporary anyway.
        if (arg->is(NodeKind::FuncCall) ||
            arg->is(NodeKind::StructInitializer)) {
            auto ll_tmp_arg = gen_node(arg);

            if (uses_optimized_int_passing && arg->is(NodeKind::FuncCall)) {
                // If the parameter is the result of a function call and
                // it can use an integer type as the argument then the
                // function call must have returned an integer, so there
                // is no need to convert to an integer as it already is.
                return ll_tmp_arg;
            }

            return finish_aggregate_arg(ll_tmp_arg);
        }

        auto ll_from_addr = gen_node(arg);
        auto ll_tmp_arg = gen_unseen_alloca(ll_struct_type, "aggr.arg");

        auto structn = struct_type->get_struct();
        if (try_move && structn->needs_move_call) {
            gen_call_move_constructor(ll_tmp_arg, ll_from_addr, structn);
        } else {
            gen_copy_struct(ll_tmp_arg, ll_from_addr, struct_type);
        }

        return finish_aggregate_arg(ll_tmp_arg);
    }

    if (arg->is(NodeKind::FuncCall)) {
        auto arg_call = static_cast<FuncCall*>(arg);
        if (arg_call->implicitly_converts_return) {
            auto ptr_type = static_cast<PointerType*>(arg_call->type);
            auto elm_type = ptr_type->get_elm_type();

            auto ll_ret = gen_rvalue(arg);

            if (elm_type->is_aggregate()) {

                uint64_t aggr_mem_size_bytes = sizeof_type_in_bytes(gen_type(elm_type));
                uint64_t aggr_mem_size_bits = aggr_mem_size_bytes * 8;

                bool uses_optimized_int_passing = aggr_mem_size_bits <= ll_module.getDataLayout().getPointerSizeInBits();
                if (uses_optimized_int_passing) {
                    // The aggregate can fit into an integer.
                    auto ll_int_type = llvm::Type::getIntNTy(ll_context, static_cast<unsigned int>(next_pow2(aggr_mem_size_bits)));
                    return builder.CreateLoad(ll_int_type, ll_ret, "opt.int.tmp.arg");
                } else {
                    // Can just pass the pointer along since it uses an aggregate argument.
                    return ll_ret;
                }
            } else {
                return builder.CreateLoad(gen_type(elm_type), ll_ret);
            }
        }
    }

    return gen_rvalue(arg);
}

llvm::Value* acorn::IRGenerator::gen_function_call_arg_for_implicit_ptr(Expr* arg) {
    if (arg->cast_type) {
        // The cast type was not needed anyway since this function effectively handles
        // the "casting".
        arg->cast_type = nullptr;

        if (arg->kind == NodeKind::IdentRef ||
            arg->kind == NodeKind::DotOperator) {
            auto ref = static_cast<IdentRef*>(arg);
            return gen_node(arg);
        } else if (arg->kind == NodeKind::UnaryOp) {
            // Read the description under gen_rvalue for why this can be considered
            // an lvalue.
            auto unary_op = static_cast<UnaryOp*>(arg);
            if (unary_op->op == '*') {
                return gen_node(arg);
            }
        } else if (arg->kind == NodeKind::MemoryAccess) {
            return gen_node(arg);
        }

        // It is an rvalue so we need to create a temporary to pass it to the function.
        auto ll_tmp_address = gen_unseen_alloca(arg->type, "tmp.imp.ptr.arg");
        process_destructor_state(arg->type, ll_tmp_address);
        gen_assignment(ll_tmp_address, arg->type, arg);

        return ll_tmp_address;
    } else {
        // Well must already be the pointer!
        return gen_rvalue(arg);
    }
}

llvm::Value* acorn::IRGenerator::gen_function_decl_call(Func* called_func,
                                                        SourceLoc call_loc,
                                                        llvm::SmallVector<Expr*>& args,
                                                        llvm::Value* ll_dest_addr,
                                                        llvm::Value* ll_in_this,
                                                        bool apply_implicit_return_ptr,
                                                        Node* lvalue) {

    bool uses_aggr_param = called_func->uses_aggr_param;
    bool uses_default_param_values = called_func->default_params_offset != -1;

    llvm::SmallVector<llvm::Value*> ll_args;
    bool is_member_func = called_func->structn;
    size_t ll_num_args = uses_default_param_values ? called_func->params.size()
                                                   : args.size();
    size_t arg_offset = 0;

    if (is_member_func) {
        ++ll_num_args;
    }
    if (uses_aggr_param) {
        ++ll_num_args;
    }
    ll_args.resize(ll_num_args);

    if (!ll_dest_addr) {
        gen_call_return_aggr_type_temporary(called_func->return_type, uses_aggr_param, ll_dest_addr);
    }

    // Pass the return address as an argument.
    if (uses_aggr_param) {
        ll_args[arg_offset++] = ll_dest_addr;
    }

    // Pass the address of the struct for the member function.
    if (is_member_func) {
        ll_args[arg_offset++] = ll_in_this;
    }

    if (uses_default_param_values) {
        // Zero initializing the arguments after the start of the default parameter values
        // then filling them in later if they were not filled by the named parameter.
        size_t default_params_offset = called_func->default_params_offset + arg_offset;
        std::fill(ll_args.begin() + default_params_offset, ll_args.end(), nullptr);
    }

    for (size_t i = 0; i < args.size(); ++i) {
        Expr* arg = args[i];
        if (arg->is(NodeKind::NamedValue)) {
            auto named_arg = static_cast<NamedValue*>(arg);
            size_t arg_idx = arg_offset + named_arg->mapped_idx;
            Var* param = called_func->params[named_arg->mapped_idx];
            if (param->has_implicit_ptr) {
                ll_args[arg_idx] = gen_function_call_arg(named_arg->assignment);
            } else {
                ll_args[arg_idx] = gen_function_call_arg_for_implicit_ptr(named_arg->assignment);
            }
        } else {
            size_t arg_idx = arg_offset + i;
            if (called_func->uses_native_varargs) {
                ll_args[arg_idx] = gen_function_call_arg(arg);
                continue;
            }

            Var* param = called_func->params[i];
            if (!param->has_implicit_ptr) {
                ll_args[arg_idx] = gen_function_call_arg(arg);
            } else {
                ll_args[arg_idx] = gen_function_call_arg_for_implicit_ptr(arg);
            }
        }
    }

    // Fill in slots with default parameter values.
    if (uses_default_param_values) {
        auto& params = called_func->params;
        size_t default_params_offset = called_func->default_params_offset;
        size_t start = default_params_offset;
        if (uses_aggr_param) {
            ++start;
        }
        if (is_member_func) {
            ++start;
        }

        size_t param_idx = called_func->default_params_offset;
        for (size_t i = start; i < ll_num_args; i++) {
            if (ll_args[i] == nullptr) {
                Expr* arg = params[param_idx]->assignment;
                ll_args[i] = gen_function_call_arg(arg);
            }
            ++param_idx;
        }
    }

    // -- Debug
    // std::string debug_info = "Calling function with name: " + called_func->name.to_string().str() + "\n";
    // debug_info += "         LLVM Types passed to function:  [";
    // for (auto ll_arg : ll_args) {
    //     debug_info += to_string(ll_arg->getType());
    //     if (ll_arg != ll_args.back()) {
    //         debug_info += ", ";
    //     }
    // }
    // debug_info += "]\n";
    // debug_info += "         Types expected by the function: [";
    // for (size_t count = 0; llvm::Argument & ll_param : called_func->ll_func->args()) {
    //     debug_info += to_string(ll_param.getType());
    //     if (count + 1 != called_func->ll_func->arg_size()) {
    //         debug_info += ", ";
    //     }
    //     ++count;
    // }
    // debug_info += "]\n";
    // Logger::debug(debug_info.c_str());

    llvm::Value* ll_ret = builder.CreateCall(called_func->ll_func, ll_args);
    emit_dbg(di_emitter->emit_location(builder, call_loc));

    if (!ll_ret->getType()->isVoidTy()) {
        ll_ret->setName("call.ret");
    }

    if (ll_dest_addr && !uses_aggr_param) {
        if (apply_implicit_return_ptr) {
            auto ptr_type = static_cast<PointerType*>(called_func->return_type);
            auto elm_type = ptr_type->get_elm_type();
            if (elm_type->is_struct()) {
                auto struct_type = static_cast<StructType*>(elm_type);
                gen_copy_struct(ll_dest_addr, ll_ret, struct_type);
            } else if (elm_type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(elm_type);
                gen_copy_array(ll_dest_addr, ll_ret, arr_type);
            } else {
                ll_ret = builder.CreateLoad(gen_type(elm_type), ll_ret);
                builder.CreateStore(ll_ret, ll_dest_addr);
            }
        } else {
            builder.CreateStore(ll_ret, ll_dest_addr);
        }
    }

    if (uses_aggr_param) {
        // If it uses an aggregate parameter value then we need to
        // return the ll_dest_addr since it is possible that the
        // address needs to be used in inline code.
        return ll_dest_addr;
    } else {
        return ll_ret;
    }
}

llvm::Value* acorn::IRGenerator::gen_function_type_call(FuncCall* call, llvm::Value* ll_dest_addr, Node* lvalue) {
    auto func_type = static_cast<FunctionType*>(call->site->type);
    auto return_type = func_type->get_return_type();
    auto& param_types = func_type->get_param_types();

    bool uses_aggr_param = false;
    llvm::Type* ll_ret_type;
    if (return_type->is_aggregate()) {
        auto ll_aggr_type = gen_type(return_type);
        uint64_t aggr_mem_size = sizeof_type_in_bytes(ll_aggr_type) * 8;
        if (aggr_mem_size <= ll_module.getDataLayout().getPointerSizeInBits()) {
            // The aggregate can fit into an integer.
            auto ll_type = llvm::Type::getIntNTy(ll_context, static_cast<unsigned int>(next_pow2(aggr_mem_size)));
            ll_ret_type = ll_type;
        } else {
            uses_aggr_param = true;
            ll_ret_type = llvm::Type::getVoidTy(ll_context);
        }
    } else {
        ll_ret_type = gen_type(return_type);
    }

    llvm::SmallVector<llvm::Value*> ll_args;
    size_t ll_num_args = call->args.size();
    size_t arg_offset = 0;

    if (uses_aggr_param) {
        ++ll_num_args;
    }
    ll_args.resize(ll_num_args);

    if (!ll_dest_addr) {
        gen_call_return_aggr_type_temporary(return_type, uses_aggr_param, ll_dest_addr);
    }

    // Pass the return address as an argument.
    if (uses_aggr_param) {
        ll_args[arg_offset++] = ll_dest_addr;
    }

    for (size_t i = 0; i < call->args.size(); ++i) {
        Expr* arg = call->args[i];
        ll_args[arg_offset + i] = gen_function_call_arg(arg);
    }

    auto ll_site = gen_rvalue(call->site);

    llvm::SmallVector<llvm::Type*> ll_param_types;
    ll_param_types.reserve(ll_num_args);
    if (uses_aggr_param) {
        ll_param_types.push_back(builder.getPtrTy());
    }
    for (Type* type : param_types) {
        if (type->is_array()) { // Array types need to be decayed.
            ll_param_types.push_back(builder.getPtrTy());
        } else {
            ll_param_types.push_back(gen_type(type));
        }
    }

    // -- Debug
    // std::string debug_info = "";
    // debug_info += "LLVM Types passed to function:  [";
    // for (auto ll_arg : ll_args) {
    //     debug_info += to_string(ll_arg->getType());
    //     if (ll_arg != ll_args.back()) {
    //         debug_info += ", ";
    //     }
    // }
    // debug_info += "]\n";
    // debug_info += "Types expected by the function: [";
    // for (size_t count = 0; llvm::Type* ll_type : ll_param_types) {
    //     debug_info += to_string(ll_type);
    //     if (count + 1 != ll_param_types.size()) {
    //         debug_info += ", ";
    //     }
    //     ++count;
    // }
    // debug_info += "]\n";
    // Logger::debug(debug_info.c_str());

    auto ll_func_type = llvm::FunctionType::get(ll_ret_type, ll_param_types, false);
    auto ll_ret = builder.CreateCall(ll_func_type, ll_site, ll_args);

    if (should_emit_debug_info && cur_func) { // TODO: deal with emitting for global context? Or struct context?
        di_emitter->emit_location(builder, call->loc);
    }

    if (!ll_ret->getType()->isVoidTy()) {
        ll_ret->setName("call.ret");
    }

    if (ll_dest_addr && !uses_aggr_param) {
        builder.CreateStore(ll_ret, ll_dest_addr);
        if (should_emit_debug_info && lvalue) {
            di_emitter->emit_location(builder, lvalue->loc);
        }
    }

    if (uses_aggr_param) {
        // If it uses an aggregate parameter value then we need to
        // return the ll_dest_addr since it is possible that the
        // address needs to be used in inline code.
        return ll_dest_addr;
    } else {
        return ll_ret;
    }
}

void acorn::IRGenerator::gen_call_return_aggr_type_temporary(Type* return_type,
                                                             bool uses_aggr_param,
                                                             llvm::Value*& ll_dest_addr) {

    // Making sure a temporary object is constructed if the returned
    // type contains a struct with a destructor then ensuring the
    // object is added to be destroyed.
    //
    if (return_type->needs_destruction()) {
        auto ll_ret_type = gen_type(return_type);
        ll_dest_addr = gen_unseen_alloca(ll_ret_type, "tmp.aggr.ret");
        add_object_with_destructor(return_type, ll_dest_addr);
    } else  if (uses_aggr_param) {
        // The user decided to ignore the return or the value
        // is a temporary value so we must create a temporary
        // value to place the return value into.
        auto ll_ret_type = gen_type(return_type);
        ll_dest_addr = gen_unseen_alloca(ll_ret_type, "tmp.aggr.ret");
    }
}

llvm::Value* acorn::IRGenerator::gen_intrinsic_call(FuncCall* call) {

    auto get_arg = [call](size_t idx) finline -> Expr* {
        if (idx < call->non_named_args_offset) {
            return call->args[idx];
        }

        for (size_t i = call->non_named_args_offset; i < call->args.size(); i++) {
            Expr* arg = call->args[i];
            if (!arg->is(NodeKind::NamedValue)) continue;

            auto named_arg = static_cast<NamedValue*>(arg);
            if (named_arg->mapped_idx == idx) {
                return named_arg->assignment;
            }
        }

        acorn_fatal("Unreachable. Failed to find named argument mapping");
        return nullptr;
    };

#define call_args_1(name)                                       \
auto ll_arg0 = gen_rvalue(get_arg(0));                          \
auto ll_func = llvm::Intrinsic::getDeclaration(                 \
    &ll_module, llvm::Intrinsic::name, { ll_arg0->getType() }); \
return builder.CreateCall(ll_func, ll_arg0);

#define call_args_2(name)                       \
auto ll_arg0 = gen_rvalue(get_arg(0));          \
auto ll_arg1 = gen_rvalue(get_arg(1));          \
auto ll_func = llvm::Intrinsic::getDeclaration( \
    &ll_module, llvm::Intrinsic::name, {        \
        ll_arg0->getType(),                     \
        ll_arg1->getType()                      \
    });                                         \
return builder.CreateCall(ll_func, { ll_arg0, ll_arg1 });

    switch (call->called_func->ll_intrinsic_id) {
    case llvm::Intrinsic::memcpy: {
        // We know the first argument is a pointer/array type since
        // it is cast to a void* so to get the type that we are
        // aligning with we need to get the element type
        // of that pointer/array.

        Expr* arg0 = get_arg(0);
        Expr* arg1 = get_arg(1);
        Expr* arg2 = get_arg(2);

        auto container_type = static_cast<ContainerType*>(arg0->get_final_type());
        auto elm_type = container_type->get_elm_type();
        auto ll_elm_type = elm_type->is(context.void_type) ? llvm::Type::getInt8Ty(ll_context)
                                                           : gen_type(elm_type);
        llvm::Align ll_alignment = get_alignment(ll_elm_type);

        return builder.CreateMemCpy(
            gen_rvalue(arg0), ll_alignment,
            gen_rvalue(arg1), ll_alignment,
            gen_rvalue(arg2)
        );
    }
    case llvm::Intrinsic::memmove: {

        Expr* arg0 = get_arg(0);
        Expr* arg1 = get_arg(1);
        Expr* arg2 = get_arg(2);

        auto container_type = static_cast<ContainerType*>(arg0->get_final_type());
        auto elm_type = container_type->get_elm_type();
        auto ll_elm_type = elm_type->is(context.void_type) ? llvm::Type::getInt8Ty(ll_context)
                                                           : gen_type(elm_type);
        llvm::Align ll_alignment = get_alignment(ll_elm_type);

        return builder.CreateMemMove(
            gen_rvalue(arg0), ll_alignment,
            gen_rvalue(arg1), ll_alignment,
            gen_rvalue(arg2)
        );
    }
    case llvm::Intrinsic::memset: {
        // We know the first argument is a pointer/array type since
        // it is cast to a void* so to get the type that we are
        // aligning with we need to get the element type
        // of that pointer/array.

        Expr* arg0 = get_arg(0);
        Expr* arg1 = get_arg(1);
        Expr* arg2 = get_arg(2);

        auto container_type = static_cast<ContainerType*>(arg0->get_final_type());
        auto elm_type = container_type->get_elm_type();
        auto ll_elm_type = elm_type->is(context.void_type) ? llvm::Type::getInt8Ty(ll_context)
                                                           : gen_type(elm_type);
        llvm::Align ll_alignment = get_alignment(ll_elm_type);

        return builder.CreateMemSet(
            gen_rvalue(arg0),
            gen_rvalue(arg1),
            gen_rvalue(arg2),
            ll_alignment
        );
    }
    case llvm::Intrinsic::floor: {
        call_args_1(floor);
    }
    case llvm::Intrinsic::ceil: {
        call_args_1(ceil);
    }
    case llvm::Intrinsic::pow: {
        call_args_2(pow);
    }
    case llvm::Intrinsic::log: {
        call_args_1(log);
    }
    case llvm::Intrinsic::log10: {
        call_args_1(log10);
    }
    case llvm::Intrinsic::sqrt: {
        call_args_1(sqrt);
    }
    case llvm::Intrinsic::sin: {
        call_args_1(sin);
    }
    case llvm::Intrinsic::cos: {
        call_args_1(cos);
    }
    case llvm::Intrinsic::tan: {
        call_args_1(tan);
    }
    case llvm::Intrinsic::asin: {
        call_args_1(asin);
    }
    case llvm::Intrinsic::acos: {
        call_args_1(acos);
    }
    case llvm::Intrinsic::atan: {
        call_args_1(atan);
    }
    default:
        acorn_fatal("Unreachabled. not a intrinsic");
        return nullptr;
    }
#undef call_args_1
#undef call_args_2
}

llvm::Value* acorn::IRGenerator::gen_bool(Bool* b) {
    return b->value ? builder.getTrue() : builder.getFalse();
}

llvm::Value* acorn::IRGenerator::gen_string(String* string) {
    auto text_to_const_array = [](const auto& text, llvm::Type* ll_elm_type) {
        llvm::ArrayType* ll_arr_type = llvm::ArrayType::get(ll_elm_type, text.size() + 1);

        llvm::SmallVector<llvm::Constant*> ll_elements;
        ll_elements.reserve(text.size() + 1);

        for (const auto c : text) {
            ll_elements.push_back(llvm::ConstantInt::get(ll_elm_type, c));
        }
        ll_elements.push_back(llvm::ConstantInt::get(ll_elm_type, 0)); // Null terminate.

        return llvm::ConstantArray::get(ll_arr_type, ll_elements);
    };
    auto text_to_global_array = [this, text_to_const_array](const auto& text,
                                                            llvm::Type* ll_elm_type,
                                                            uint64_t alignment) {
        llvm::Constant* const_array = text_to_const_array(text, ll_elm_type);
        auto ll_global = gen_const_global_variable(get_global_name("global.string"),
                                                   const_array->getType(),
                                                   const_array);
        ll_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        ll_global->setAlignment(llvm::Align(alignment));
        return ll_global;
    };

    if (string->bit_type == String::Str8Bit) {
        if (string->cast_type->is(context.const_char16_ptr_type)) {
            return text_to_global_array(string->text16bit, llvm::Type::getInt16Ty(ll_context), 2);
        } else if (string->cast_type->is(context.const_char32_ptr_type)) {
            return text_to_global_array(string->text32bit, llvm::Type::getInt32Ty(ll_context), 4);
        } else {
            return text_to_global_array(string->text8bit, llvm::Type::getInt8Ty(ll_context), 1);
        }
    } else if (string->bit_type == String::Str16Bit) {
        if (string->cast_type->is(context.const_char32_ptr_type)) {
            return text_to_global_array(string->text32bit, llvm::Type::getInt32Ty(ll_context), 4);
        } else {
            return text_to_global_array(string->text16bit, llvm::Type::getInt16Ty(ll_context), 2);
        }
    } else if (string->bit_type == String::Str32Bit) {
        return text_to_global_array(string->text32bit, llvm::Type::getInt32Ty(ll_context), 4);
    } else {
        acorn_fatal("unreachable: not valid bit type for string");
        return nullptr;
    }
}

llvm::Value* acorn::IRGenerator::gen_null() {
    return llvm::Constant::getNullValue(builder.getPtrTy());
}

llvm::Value* acorn::IRGenerator::gen_array(Array* arr, llvm::Value* ll_dest_addr, Node* lvalue) {
    if (!ll_dest_addr) {
        ll_dest_addr = gen_unseen_alloca(arr->type, "tmp.arr");
        process_destructor_state(arr->type, ll_dest_addr);
    }

    // Check for a cast to an array because it is possible the user is assigning the array to
    // a variable with an array of a larger size.
    auto to_type = arr->type;
    if (arr->cast_type && arr->cast_type->is_array()) {
        to_type = arr->cast_type;
    }

    auto arr_type = static_cast<ArrayType*>(to_type);
    auto ll_elm_type = gen_type(arr_type->get_elm_type());
    auto ll_arr_type = llvm::ArrayType::get(ll_elm_type, arr_type->get_length());

    // If the array is foldable then a constant global array is created which
    // will then be copied over using memcpy into the destination array.
    if (arr->is_foldable) {

        llvm::Align ll_alignment = get_alignment(ll_elm_type);

        auto ll_const_arr = gen_constant_array(arr, arr_type, ll_arr_type);
        auto ll_global_arr = gen_const_global_variable("const_array", ll_arr_type, ll_const_arr);
        ll_global_arr->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        ll_global_arr->setAlignment(ll_alignment);

        auto ll_base_type = gen_type(arr_type->get_base_type());

        uint64_t total_linear_length = arr_type->get_total_linear_length();
        builder.CreateMemCpy(
            ll_dest_addr, ll_alignment,
            ll_global_arr, ll_alignment,
            total_linear_length * sizeof_type_in_bytes(ll_base_type)
        );
        if (should_emit_debug_info && lvalue) {
            di_emitter->emit_location(builder, lvalue->loc);
        }
        return ll_dest_addr;
    }

    // Indexing all the addresses of the array and assigning a value.
    auto gen_gep_index = [this, ll_arr_type, ll_dest_addr](uint64_t i) finline {
        auto ll_index = gen_isize(i);
        return builder.CreateInBoundsGEP(ll_arr_type->getElementType(),
                                         ll_dest_addr,
                                         { ll_index });
    };

    bool elms_are_arrays = arr_type->get_elm_type()->is_array();
    for (uint64_t i = 0; i < arr->elms.size(); i++) {

        Expr* elm = arr->elms[i];
        auto ll_elm_addr = gen_gep_index(i);

        if (elm) {
            // We have to pass the lvalue in because if the only elements are those
            // which require gen_assignment then we still need to attach a store to
            // at least one of those instructions.
            gen_assignment(ll_elm_addr, arr_type->get_elm_type(), elm, lvalue);
        } else {
            builder.CreateStore(gen_zero(arr_type->get_elm_type()), ll_elm_addr);
        }
    }
    // Zero fill the rest.
    for (uint64_t i = arr->elms.size(); i < ll_arr_type->getNumElements(); ++i) {
        auto ll_elm_addr = gen_gep_index(i);
        builder.CreateStore(gen_zero(arr_type->get_elm_type()), ll_elm_addr);
    }
    // Note: clang will actually emit for every array element access and every store
    //       but the debugger should only need to know a single location. So we will
    //       only attach the information to the last store instruction.
    if (should_emit_debug_info && lvalue && arr->elms.size() != ll_arr_type->getNumElements()) {
        di_emitter->emit_location(builder, lvalue->loc);
    }

    return ll_dest_addr;
}

llvm::Constant* acorn::IRGenerator::gen_constant_array(Array* arr, ArrayType* arr_type, llvm::ArrayType* ll_arr_type) {

    bool elms_are_arrays = arr_type->get_elm_type()->is_array();
    auto ll_elm_type = ll_arr_type->getElementType();

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(ll_arr_type->getNumElements());

    auto get_element = [this, elms_are_arrays, ll_elm_type](Expr* elm) finline {
        if (elms_are_arrays) {
            auto elm_arr = static_cast<Array*>(elm);
            auto elm_arr_type = static_cast<ArrayType*>(elm_arr->get_final_type());
            return gen_constant_array(elm_arr, elm_arr_type, llvm::cast<llvm::ArrayType>(ll_elm_type));
        } else {
            return llvm::cast<llvm::Constant>(gen_rvalue(elm));
        }
    };

    for (Expr* elm : arr->elms) {
        if (elm) {
            ll_values.push_back(get_element(elm));
        } else {
            ll_values.push_back(gen_zero(arr_type->get_elm_type()));
        }
    }
    // Zero fill the rest.
    for (uint32_t i = arr->elms.size(); i < ll_arr_type->getNumElements(); ++i) {
        ll_values.push_back(gen_zero(arr_type->get_elm_type()));
    }

    return llvm::ConstantArray::get(ll_arr_type, ll_values);
}

llvm::Value* acorn::IRGenerator::gen_memory_access(MemoryAccess* mem_access) {

    auto ll_memory = gen_rvalue(mem_access->site);
    bool mem_access_ptr = mem_access->site->type->is_pointer();

    Type* type = mem_access->site->type;

    if (mem_access->site->is(NodeKind::FuncCall)) {
        auto call = static_cast<FuncCall*>(mem_access->site);
        ll_memory = gen_handle_returned_aggregate_obj(call, ll_memory, "tmp.arr");
    }

    if (mem_access_ptr) {
        auto ctr_type = static_cast<ContainerType*>(type);
        auto ll_load_type = gen_type(ctr_type->get_elm_type());
        return builder.CreateInBoundsGEP(ll_load_type, ll_memory, {gen_rvalue(mem_access->index)});
    } else if (type->is_array()) {
        return gen_array_memory_access(ll_memory, type, mem_access->index);
    } else {
        // Should be a slice type otherwise.
        auto ll_slice_type = gen_type(type);

        auto ll_elements_field_addr = builder.CreateStructGEP(ll_slice_type, ll_memory, 0);
        auto ll_elements_ptr = builder.CreateLoad(builder.getPtrTy(), ll_elements_field_addr);

        auto ctr_type = static_cast<ContainerType*>(type);
        auto ll_load_type = gen_type(ctr_type->get_elm_type());
        return builder.CreateInBoundsGEP(ll_load_type, ll_elements_ptr, {gen_rvalue(mem_access->index)});
    }
}

llvm::Value* acorn::IRGenerator::gen_dot_operator(DotOperator* dot) {

    auto gen_struct_type_access = [this, dot](StructType* struct_type) finline{
        auto site = dot->site;

        auto ll_struct_type = gen_type(struct_type);
        auto ll_struct_address = gen_node(site);

        if (site->is(NodeKind::FuncCall)) {
            auto call = static_cast<FuncCall*>(site);
            ll_struct_address = gen_handle_returned_aggregate_obj(call, ll_struct_address, "tmp.struct");
        }

        // Automatically dereferencing pointers.
        if (site->type->is_pointer() && is_pointer_lvalue(site)) {
            ll_struct_address = builder.CreateLoad(builder.getPtrTy(), ll_struct_address);
        }

        auto field = dot->var_ref;
        return builder.CreateStructGEP(ll_struct_type, ll_struct_address, field->field_idx);
    };

    if (dot->is_array_length) {
        if (dot->site->type->is_array()) {
            auto arr_type = static_cast<ArrayType*>(dot->site->type);
            return builder.getInt32(arr_type->get_length());
        } else {
            auto slice_type = static_cast<SliceType*>(dot->site->type);
            auto ll_slice_type = gen_type(slice_type);
            auto ll_slice = gen_node(dot->site);

            auto ll_length_field_addr = builder.CreateStructGEP(ll_slice_type, ll_slice, 1);
            return builder.CreateLoad(builder.getInt32Ty(), ll_length_field_addr);
        }
    } else if (dot->is_funcs_ref()) {
        auto func = (*dot->funcs_ref)[0];
        gen_function_decl(func);

        return func->ll_func;
    } else if (dot->site->type->is_struct()) {
        return gen_struct_type_access(static_cast<StructType*>(dot->site->type));
    } else if (dot->is_slice_ptr) {
        auto slice_type = static_cast<SliceType*>(dot->site->type);
        auto ll_slice_type = gen_type(slice_type);
        auto ll_slice = gen_node(dot->site);

        auto ll_length_field_addr = builder.CreateStructGEP(ll_slice_type, ll_slice, 0);
        return builder.CreateLoad(builder.getPtrTy(), ll_length_field_addr);
    } else if (dot->site->type->is_pointer()) {
        auto ptr_type = static_cast<PointerType*>(dot->site->type);
        auto elm_type = ptr_type->get_elm_type();
        if (elm_type->is_struct()) {
            return gen_struct_type_access(static_cast<StructType*>(elm_type));
        } else {
            return gen_ident_reference(dot);
        }
    } else if (dot->is_enum_value) {
        auto enum_type = static_cast<EnumType*>(dot->site->type);

        auto ll_index = gen_rvalue(dot->site);
        if (enum_type->get_values_type()->is_integer()) {
            return ll_index;
        }

        return gen_enum_value_from_enum_array(enum_type, ll_index);
    } else {
        return gen_ident_reference(dot);
    }
}

void acorn::IRGenerator::gen_assignment(llvm::Value* ll_address,
                                        Type* to_type,
                                        Expr* value,
                                        Node* lvalue,
                                        bool is_assign_op,
                                        bool try_move) {

    auto process_implicit_assignment_operator = [=, this](llvm::Value* ll_from_addr,
                                                          const std::function<void()>& copy_cb) finline {
        // Note: In c++ they have support for overloading the assignment operator.
        //       By default unless you overload the operator in C++ the behavior will
        //       be to simply copy the object over using memcpy, not call the copy
        //       constructor, and not call any destructor. However, when the assignment
        //       operator is overloaded and when working with dynamic memory it is a
        //       common idiom to check if the memory assigned to is the same as the
        //       address being assigned from.
        //
        //       For now the compiler will minic this idiom without the need for an
        //       assignment operator. It will rely on the copy constructor to do the
        //       copying and call the destructor if the memory does not match.

        if (to_type->needs_destruction()) {

            // Checking for common cases to avoid having to generate code generation if
            // we can garentee the addresses are not the same.

            if (value->is(NodeKind::IdentRef) &&
                lvalue->is(NodeKind::IdentRef)) {
                auto from_ident_ref = static_cast<IdentRef*>(value);
                auto to_ident_ref   = static_cast<IdentRef*>(lvalue);

                if (from_ident_ref->is_var_ref() && to_ident_ref->is_var_ref()) {
                    auto from_var = from_ident_ref->var_ref;
                    auto to_var   = to_ident_ref->var_ref;

                    // Both are referencing varialbes but those variables are distinct
                    // so we can garentee that we can destroy the memory since there is
                    // no possibility that the memory is overlapping.
                    //
                    // Note: we know that these are not variables of pointers because pointer
                    // versions do not reduce to a destructable type. And when dereferencing from
                    // a pointer and then assigning the `to_expr_for_assign_op` is a `NodeKind::UnaryOp`
                    // with an `op` of '*'.
                    //
                    // Ex.
                    //
                    // void main() {
                    //     A a1;
                    //     A a2;
                    //     a1 = a2;  // We can garentee that a1 and a2 do not share memory.
                    // }
                    //
                    if (from_var != to_var) {
                        gen_call_destructors(to_type, ll_address);
                        copy_cb();
                        return;
                    }
                }
            }

            auto ll_then_bb = gen_bblock("if.not.same.mem.then", ll_cur_func);
            auto ll_else_bb = gen_bblock("if.not.same.mem.end" , ll_cur_func);

            auto ll_cond = builder.CreateICmpNE(ll_address, ll_from_addr, "neq");
            builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);

            // Reassigning so need to destroy the existing memory
            // before assignign new memory.

            builder.SetInsertPoint(ll_then_bb);

            gen_call_destructors(to_type, ll_address);
            copy_cb();

            // Jump out of the then block after destroying the
            // object.
            builder.CreateBr(ll_else_bb);

            builder.SetInsertPoint(ll_else_bb);

        } else {
            // Does not need destruction but still need to copy over the object.
            copy_cb();
        }
    };

    // If the object is destructable we need to copy the original because it
    // is possible the function call references our object memory in some way.
    auto create_tmp_then_assign_and_destroy = [=, this]
        (const std::function<void(llvm::Value*)>& gen_cb) finline {

        auto ll_tmp_obj = gen_unseen_alloca(to_type, "tmp.obj");
        gen_cb(ll_tmp_obj);

        if (to_type->is_struct()) {
            auto struct_type = static_cast<StructType*>(to_type);
            gen_copy_struct(ll_address, ll_tmp_obj, struct_type);
        } else {
            auto arr_type = static_cast<ArrayType*>(to_type);
            gen_copy_array(ll_address, ll_tmp_obj, arr_type);
        }

        // destroy temporary memory.
        gen_call_destructors(to_type, ll_tmp_obj);
    };

    if (!context.should_stand_alone()) {
        if (value->cast_type && value->cast_type->is(context.std_any_struct_type)) {
            value->cast_type = nullptr;
            store_value_to_any(ll_address, value, gen_node(value));
            return;
        }
    }

    if (value->is(NodeKind::Array)) {
        if (to_type->is_slice()) {

            auto arr_type = static_cast<ArrayType*>(value->type);
            auto ll_slice_type = gen_type(to_type);

            auto ll_array = gen_array(static_cast<Array*>(value), nullptr);
            gen_store_array_in_slice(ll_slice_type, ll_address, ll_array, arr_type);

            if (should_emit_debug_info && lvalue)
                di_emitter->emit_location(builder, lvalue->loc);

            return;
        }

        if (is_assign_op && to_type->needs_destruction()) {
            create_tmp_then_assign_and_destroy([this, value, lvalue](llvm::Value* ll_address) {
                gen_array(static_cast<Array*>(value), ll_address, lvalue);
            });
        } else {
            gen_array(static_cast<Array*>(value), ll_address, lvalue);
        }
    } else if (value->is(NodeKind::FuncCall)) {
        if (is_assign_op && to_type->needs_destruction()) {
            create_tmp_then_assign_and_destroy([this, value, lvalue](llvm::Value* ll_address) {
                gen_function_call(static_cast<FuncCall*>(value), ll_address, lvalue);
            });
        } else {
            gen_function_call(static_cast<FuncCall*>(value), ll_address, lvalue);
        }
    } else if (value->is(NodeKind::StructInitializer)) {
        if (is_assign_op && to_type->needs_destruction()) {
            create_tmp_then_assign_and_destroy([this, value, lvalue](llvm::Value* ll_address) {
                auto initializer = static_cast<StructInitializer*>(value);
                gen_struct_initializer(initializer, ll_address, lvalue);
            });
        } else {
            auto initializer = static_cast<StructInitializer*>(value);
            gen_struct_initializer(initializer, ll_address, lvalue);
        }
    } else if (value->is(NodeKind::Ternary)) {
        auto ll_value = gen_ternary(static_cast<Ternary*>(value), ll_address, lvalue, is_assign_op, try_move);
        if (!value->type->is_aggregate()) { // If it is an aggregate it calls gen_assignment for the passed ll_address.
            builder.CreateStore(ll_value, ll_address);
            if (should_emit_debug_info && lvalue) {
                di_emitter->emit_location(builder, lvalue->loc);
            }
        }
    } else if (to_type->is_struct()) {
        if (value->is(NodeKind::MoveObj)) {
            try_move = true;
            auto move_obj = static_cast<MoveObj*>(value);
            value = move_obj->value;
        }

        auto copy_or_move_struct = [this, to_type, try_move, ll_address, lvalue, value](llvm::Value* ll_from_addr) finline {
            auto struct_type = static_cast<StructType*>(to_type);
            auto structn = struct_type->get_struct();

            if (try_move && structn->needs_move_call) {
                gen_call_move_constructor(ll_address, ll_from_addr, structn);
            } else {
                gen_copy_struct(ll_address, ll_from_addr, struct_type);
            }
            if (should_emit_debug_info && lvalue) // Should work because gen_copy_struct either makes a call or memcpy.
                di_emitter->emit_location(builder, lvalue->loc);
        };

        auto ll_from_addr = gen_node(value);

        if (is_assign_op) {
            process_implicit_assignment_operator(ll_from_addr, [this, ll_from_addr, &copy_or_move_struct]() {
                copy_or_move_struct(ll_from_addr);
            });
        } else {
            copy_or_move_struct(ll_from_addr);
        }
    } else if (to_type->is_array()) {
        if (value->is(NodeKind::MoveObj)) {
            try_move = true;
            auto move_obj = static_cast<MoveObj*>(value);
            value = move_obj->value;
        }

        auto copy_or_move_array_of_structs = [this, to_type, try_move, ll_address, lvalue](llvm::Value* ll_from_addr) finline {
            auto arr_type = static_cast<ArrayType*>(to_type);
            auto elm_type = arr_type->get_elm_type();
            if (elm_type->is_struct()) {
                auto struct_type = static_cast<StructType*>(elm_type);
                auto structn = struct_type->get_struct();
                if (try_move && structn->needs_move_call) {
                    gen_call_array_move_constructors(ll_address, ll_from_addr, arr_type, structn, lvalue);
                    return;
                }
            }
            gen_copy_array(ll_address, ll_from_addr, arr_type, lvalue);
        };

        auto ll_from_addr = gen_node(value);
        if (is_assign_op) {
            process_implicit_assignment_operator(ll_from_addr, [this, ll_from_addr, &copy_or_move_array_of_structs]() {
                copy_or_move_array_of_structs(ll_from_addr);
            });
        } else {
            copy_or_move_array_of_structs(ll_from_addr);
        }
    } else if (to_type->is_slice()) {
        if (value->type->is_array()) {

            auto arr_type = static_cast<ArrayType*>(value->type);
            auto ll_slice_type = gen_type(to_type);

            auto ll_array = gen_node(value);
            gen_store_array_in_slice(ll_slice_type, ll_address, ll_array, arr_type);

        } else {

            auto ll_slice_type = gen_type(to_type);

            // TODO: Is the alignment correct?
            llvm::Align ll_alignment = get_alignment(ll_slice_type);

            auto ll_from_address = gen_node(value);

            builder.CreateMemCpy(
                ll_address, ll_alignment,
                ll_from_address, ll_alignment,
                sizeof_type_in_bytes(ll_slice_type)
            );
        }

        if (should_emit_debug_info && lvalue)
            di_emitter->emit_location(builder, lvalue->loc);
    } else {
        if (value->is(NodeKind::MoveObj)) {
            auto move_obj = static_cast<MoveObj*>(value);
            value = move_obj->value;
        }

        auto ll_assignment = gen_rvalue(value);
        builder.CreateStore(ll_assignment, ll_address);
        if (should_emit_debug_info && lvalue) {
            di_emitter->emit_location(builder, lvalue->loc);
        }
    }
}

void acorn::IRGenerator::gen_default_value(llvm::Value* ll_address, Type* type, Node* lvalue) {

    auto type_kind = type->get_kind();
    if (type_kind == TypeKind::Array) {
        // There is no reason to take into lvalue here since when assigning
        // with arrays gen_default_value is not called. The `lvalue` variable
        // is passed in because struct initialization requires it.

        auto arr_type = static_cast<ArrayType*>(type);
        auto base_type = arr_type->get_base_type();

        uint64_t total_linear_length = arr_type->get_total_linear_length();

        if (base_type->get_kind() == TypeKind::Struct) {
            auto struct_type = static_cast<StructType*>(base_type);
            auto structn = struct_type->get_struct();

            if (structn->needs_default_call) {
                auto ll_arr_type = gen_type(type);

                llvm::SmallVector<llvm::Value*> ll_indexes(arr_type->get_depth() + 1, gen_isize(0));
                auto ll_arr_start_ptr    = builder.CreateInBoundsGEP(ll_arr_type, ll_address, ll_indexes);
                auto ll_total_arr_length = gen_isize(total_linear_length);
                gen_abstract_array_loop(base_type,
                                        ll_arr_start_ptr,
                                        ll_total_arr_length,
                                        [this, structn](auto ll_elm) {
                    gen_call_default_constructor(ll_elm, structn);
                });
                return;
            }
        }


        // TODO: Are we really sure we want to use the alignment of the element type?
        //       Clang seems use the alignment of the array itself.
        auto ll_alignment = get_alignment(arr_type->get_elm_type());
        auto ll_base_type = gen_type(arr_type->get_base_type());

        builder.CreateMemSet(
            ll_address,
            builder.getInt8(0),
            total_linear_length * sizeof_type_in_bytes(ll_base_type),
            ll_alignment
        );
    } else if (type_kind == TypeKind::Struct) {

        auto struct_type = static_cast<StructType*>(type);
        auto structn = struct_type->get_struct();

        auto ll_struct_type = gen_type(type);
        auto ll_alignment = get_alignment(ll_struct_type);

        if (!structn->needs_default_call) {
            builder.CreateMemSet(
                ll_address,
                builder.getInt8(0),
                sizeof_type_in_bytes(ll_struct_type),
                ll_alignment
            );
        } else {
            gen_call_default_constructor(ll_address, structn);
        }

        if (should_emit_debug_info && lvalue)
            di_emitter->emit_location(builder, lvalue->loc);
    } else if (type_kind == TypeKind::Enum) {

        auto enum_type = static_cast<EnumType*>(type);
        uint64_t default_index = enum_type->get_default_index();

        auto ll_index = gen_enum_index(default_index, enum_type->get_index_type());
        builder.CreateStore(ll_index, ll_address);
        if (should_emit_debug_info && lvalue)
            di_emitter->emit_location(builder, lvalue->loc);
    } else {
        builder.CreateStore(gen_zero(type), ll_address);
        if (should_emit_debug_info && lvalue)
            di_emitter->emit_location(builder, lvalue->loc);
    }
}

llvm::Constant* acorn::IRGenerator::gen_zero(Type* type) {
    switch (type->get_kind()) {
    case TypeKind::Int8: case TypeKind::UInt8: case TypeKind::Char:
        return builder.getInt8(0);
    case TypeKind::Int16: case TypeKind::UInt16: case TypeKind::Char16:
        return builder.getInt16(0);
    case TypeKind::Int: case TypeKind::Int32: case TypeKind::UInt32: case TypeKind::Char32:
        return builder.getInt32(0);
    case TypeKind::Int64: case TypeKind::UInt64:
        return builder.getInt64(0);
    case TypeKind::Float32:
        return llvm::ConstantFP::get(ll_context, llvm::APFloat((float)0.0F));
    case TypeKind::Float64:
        return llvm::ConstantFP::get(ll_context, llvm::APFloat((double)0.0));
    case TypeKind::Bool:
        return builder.getInt1(0);
    case TypeKind::ISize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), 0, true);
    case TypeKind::USize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), 0, false);
    case TypeKind::Pointer:
    case TypeKind::Function:
        return llvm::Constant::getNullValue(builder.getPtrTy());
    case TypeKind::Array:
    case TypeKind::Struct:
    case TypeKind::Slice:
        return llvm::ConstantAggregateZero::get(gen_type(type));
    default:
        acorn_fatal("gen_zero(): Missing case");
        return nullptr;
    }
}

llvm::Constant* acorn::IRGenerator::gen_one(Type* type) {
    switch (type->get_kind()) {
    case TypeKind::Int8: case TypeKind::UInt8: case TypeKind::Char:
        return builder.getInt8(1);
    case TypeKind::Int16: case TypeKind::UInt16: case TypeKind::Char16:
        return builder.getInt16(1);
    case TypeKind::Int: case TypeKind::Int32: case TypeKind::UInt32: case TypeKind::Char32:
        return builder.getInt32(1);
    case TypeKind::Int64: case TypeKind::UInt64:
        return builder.getInt64(1);
    case TypeKind::ISize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), 1, true);
    case TypeKind::USize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), 1, false);
    case TypeKind::Bool:
        return builder.getInt1(1);
    default:
        acorn_fatal("gen_one(): Missing case");
        return nullptr;
    }
}

llvm::Value* acorn::IRGenerator::gen_cast(Cast* cast) {
    return gen_cast(cast->type, cast->value, gen_rvalue(cast->value));
}

llvm::Value* acorn::IRGenerator::gen_cast(Type* to_type, Expr* value, llvm::Value* ll_value) {

    Type* from_type = value->type;
    switch (to_type->get_kind()) {
    case TypeKind::Pointer: {
        if (from_type->is_integer() || from_type->is_bool()) {
            return builder.CreateIntToPtr(ll_value, builder.getPtrTy(), "cast");
        } else if (from_type->is_real_pointer() || from_type->is(context.null_type)) {
            // Pointer to pointer doesn't need casting because of opaque pointers.
            return ll_value;
        } else if (from_type->is_array()) {
            // TODO: should this GEP? llvm seems fine with assigning directly to a pointer
            // but there may be GEP issues if the cast happens inline. That and clang uses GEP
            // to perform a cast from an array to a pointer.
            return ll_value;
        }
        goto NoCastFound;
    }
    case TypeKind::Int:
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::USize:
    case TypeKind::ISize:
    case TypeKind::Char:
    case TypeKind::Char16:
    case TypeKind::Char32:
    case TypeKind::Bool: {
        if (from_type->is_integer() || from_type->is_bool()) {
            return builder.CreateIntCast(ll_value, gen_type(to_type), to_type->is_signed(), "cast");
        } else if (from_type->is_real_pointer()) {
            return builder.CreatePtrToInt(ll_value, gen_type(to_type), "cast");
        } else if (from_type->is_float()) {
            if (to_type->is_signed()) {
                return builder.CreateFPToSI(ll_value, gen_type(to_type), "cast");
            } else {
                return builder.CreateFPToUI(ll_value, gen_type(to_type), "cast");
            }
        }
        goto NoCastFound;
    }
    case TypeKind::Float32:
    case TypeKind::Float64: {
        if (from_type->is_float()) {
            if (to_type->get_number_of_bits() > from_type->get_number_of_bits()) {
                // Upcasting float.
                return builder.CreateFPExt(ll_value, gen_type(to_type), "cast");
            } else {
                // Downcastinf float.
                return builder.CreateFPTrunc(ll_value, gen_type(to_type), "cast");
            }
        } else if (from_type->is_integer()) {
            if (from_type->is_signed()) {
                return builder.CreateSIToFP(ll_value, gen_type(to_type), "cast");
            } else {
                return builder.CreateUIToFP(ll_value, gen_type(to_type), "cast");
            }
        }
        goto NoCastFound;
    }
    case TypeKind::Function: {
        // NOTE: FuncsRef is not the same as is_function()!
        if (from_type->get_kind() == TypeKind::FuncsRef) {
            return ll_value;
        } else if (from_type->is_pointer()) {
            return ll_value;
        }

        goto NoCastFound;
    }
    case TypeKind::Slice: {
        if (from_type->is_array()) {
            // Creating a temporary slice and storing the array into it.
            auto ll_slice_type = gen_type(to_type);
            auto ll_slice = gen_unseen_alloca(gen_type(to_type), "tmp.slice");

            auto arr_type = static_cast<ArrayType*>(from_type);
            return gen_store_array_in_slice(ll_slice_type, ll_slice, ll_value, arr_type);
        }

        goto NoCastFound;
    }
    case TypeKind::Struct: {
        if (!context.should_stand_alone() && to_type->is(context.std_any_struct_type)) {
            auto ll_any_struct_type = gen_struct_type(context.std_any_struct_type);
            auto ll_address = gen_unseen_alloca(ll_any_struct_type, "tmp.any.obj");
            store_value_to_any(ll_address, value, ll_value);
            return ll_address;
        }

        goto NoCastFound;
    }

    default:
    NoCastFound:
        acorn_fatal("gen_cast(): Failed to implement case");
        break;
    }


    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_enum_index(uint64_t index, Type* index_type) {
    switch (index_type->get_kind()) {
    case TypeKind::Int8: case TypeKind::UInt8: case TypeKind::Char:
        return builder.getInt8(static_cast<uint8_t>(index));
    case TypeKind::Int16: case TypeKind::UInt16: case TypeKind::Char16:
        return builder.getInt16(static_cast<uint16_t>(index));
    case TypeKind::Int: case TypeKind::Int32: case TypeKind::UInt32: case TypeKind::Char32:
        return builder.getInt32(static_cast<uint32_t>(index));
    case TypeKind::Int64: case TypeKind::UInt64:
        return builder.getInt64(index);
    case TypeKind::ISize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), index, true);
    case TypeKind::USize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), index, false);
    default:
        acorn_fatal("Unknown integer type");
        return nullptr;
    }
}

llvm::Value* acorn::IRGenerator::gen_enum_value_from_enum_array(EnumType* enum_type, llvm::Value* ll_index) {

    auto ll_values_type = gen_type(enum_type->get_values_type());
    auto enumn = enum_type->get_enum();

    auto ll_array_type = llvm::ArrayType::get(ll_values_type, enumn->values.size());

    if (enumn->ll_array) {
        ll_index = builder.CreateInBoundsGEP(ll_array_type, enumn->ll_array, {gen_isize(0), ll_index});
        return builder.CreateLoad(ll_values_type, ll_index, "cast");
    }

    llvm::SmallVector<llvm::Constant*> ll_elements;
    ll_elements.reserve(enumn->values.size());
    for (auto& enum_value : enumn->values) {
        ll_elements.push_back(llvm::cast<llvm::Constant>(gen_rvalue(enum_value.assignment)));
    }

    auto ll_const_array = llvm::ConstantArray::get(ll_array_type, ll_elements);
    auto ll_name = "global." + llvm::Twine(enumn->name.to_string()) + "." + llvm::Twine(context.global_counter++);
    auto ll_array = gen_global_variable(ll_name,
                                          ll_array_type,
                                          true, // is constant
                                          ll_const_array,
                                          llvm::GlobalValue::InternalLinkage);
    enumn->ll_array = ll_array;

    ll_index = builder.CreateInBoundsGEP(ll_array_type, ll_array, {gen_isize(0), ll_index});
    return builder.CreateLoad(ll_values_type, ll_index, "cast");
}

llvm::Value* acorn::IRGenerator::gen_store_array_in_slice(llvm::Type* ll_slice_type,
                                                          llvm::Value* ll_slice_addr,
                                                          llvm::Value* ll_array_addr,
                                                          ArrayType* arr_type) {

    auto elements_field_addr = builder.CreateStructGEP(ll_slice_type, ll_slice_addr, 0);
    builder.CreateStore(ll_array_addr, elements_field_addr);

    auto length_field_addr = builder.CreateStructGEP(ll_slice_type, ll_slice_addr, 1);
    builder.CreateStore(builder.getInt32(arr_type->get_length()), length_field_addr);

    return ll_slice_addr;
}

llvm::Value* acorn::IRGenerator::gen_condition(Expr* cond) {
    auto ll_value = gen_rvalue(cond);
    if (cond->type->is_real_pointer()) {
        return builder.CreateIsNotNull(ll_value);
    }
    return ll_value;
}

llvm::BasicBlock* acorn::IRGenerator::gen_bblock(const char* name, llvm::Function* ll_func) {
    return llvm::BasicBlock::Create(ll_context, name, ll_func);
}

void acorn::IRGenerator::insert_bblock_at_end(llvm::BasicBlock* ll_bb) {
    ll_cur_func->insert(ll_cur_func->end(), ll_bb);
}

llvm::Function* acorn::IRGenerator::gen_no_param_member_function_decl(Struct* structn, llvm::Twine name) {
    auto ll_param_types = llvm::SmallVector<llvm::Type*>{ builder.getPtrTy() };
    auto ll_func_type   = llvm::FunctionType::get(builder.getVoidTy(), ll_param_types, false);

    auto ll_func = llvm::Function::Create(
        ll_func_type,
        llvm::GlobalValue::InternalLinkage,
        name,
        ll_module
    );

    return ll_func;
}

void acorn::IRGenerator::gen_call_default_constructor(llvm::Value* ll_address, Struct* structn) {
    if (!structn->ll_default_constructor) {
        llvm::Twine ll_name = structn->name.to_string();
        structn->ll_default_constructor =
            gen_no_param_member_function_decl(structn, ll_name + (structn->default_constructor ? ".acorn" : ".implicit.acorn"));
        if (structn->default_constructor) {
            structn->default_constructor->ll_func = structn->ll_default_constructor;
            context.queue_gen(structn->default_constructor);
        } else if (!structn->has_requested_gen_implicits) {
            structn->has_requested_gen_implicits = true;
            auto implicit_func = create_implicit_function(ImplicitFunc::ImplicitKind::DefaultConstructor, structn);
            context.queue_gen_implicit_function(implicit_func);
        }
    }
    builder.CreateCall(structn->ll_default_constructor, ll_address);
    emit_dbg(di_emitter->emit_location_at_last_statement(builder));
}

void acorn::IRGenerator::gen_abstract_array_loop(Type* base_type,
                                                 llvm::Value* ll_arr_start_ptr,
                                                 llvm::Value* ll_total_arr_length,
                                                 const std::function<void(llvm::PHINode*)>& codegen_cb) {

    auto ll_base_type = gen_type(base_type);

    auto ll_orginal_bb  = builder.GetInsertBlock();
    auto ll_arr_end_ptr = builder.CreateInBoundsGEP(ll_base_type, ll_arr_start_ptr, { ll_total_arr_length });

    auto ll_loop_bb     = gen_bblock("arr.loop"    , ll_cur_func);
    auto ll_loop_end_bb = gen_bblock("arr.loop.end", ll_cur_func);

    builder.CreateBr(ll_loop_bb);
    builder.SetInsertPoint(ll_loop_bb);

    // Pointer used to traverse through the array.
    //
    auto ll_cur_elm = builder.CreatePHI(builder.getPtrTy(), 0, "arr.elm");
    // Incoming value to the start of the array from the incoming block.
    ll_cur_elm->addIncoming(ll_arr_start_ptr, ll_orginal_bb);

    codegen_cb(ll_cur_elm);

    // Move to the next element in the arra.
    auto ll_next_elm = builder.CreateInBoundsGEP(ll_base_type, ll_cur_elm, { gen_isize(1) });

    // Checking if all objects have been looped over.
    auto ll_done_cond = builder.CreateICmpEQ(ll_next_elm, ll_arr_end_ptr);
    builder.CreateCondBr(ll_done_cond, ll_loop_end_bb, ll_loop_bb);

    auto ll_cur_bb = builder.GetInsertBlock();
    ll_cur_elm->addIncoming(ll_next_elm, ll_cur_bb);

    // Continue generating code after the loop.
    builder.SetInsertPoint(ll_loop_end_bb);

}

void acorn::IRGenerator::gen_abstract_double_array_loop(Type* base_type,
                                                        llvm::Value* ll_to_arr_start_ptr,
                                                        llvm::Value* ll_from_arr_start_ptr,
                                                        llvm::Value* ll_total_arr_length,
                                                        const std::function<void(llvm::PHINode*, llvm::PHINode*)>& codegen_cb) {

    auto ll_base_type = gen_type(base_type);

    auto ll_orginal_bb  = builder.GetInsertBlock();
    auto ll_arr_end_ptr = builder.CreateInBoundsGEP(ll_base_type, ll_to_arr_start_ptr, { ll_total_arr_length });

    auto ll_loop_bb     = gen_bblock("arr.loop"    , ll_cur_func);
    auto ll_loop_end_bb = gen_bblock("arr.loop.end", ll_cur_func);

    builder.CreateBr(ll_loop_bb);
    builder.SetInsertPoint(ll_loop_bb);

    // Pointer used to traverse through the array.
    //
    auto ll_to_cur_elm   = builder.CreatePHI(builder.getPtrTy(), 0, "to.arr.elm");
    auto ll_from_cur_elm = builder.CreatePHI(builder.getPtrTy(), 0, "from.arr.elm");
    // Incoming value to the start of the array from the incoming block.
    ll_to_cur_elm->addIncoming(ll_to_arr_start_ptr, ll_orginal_bb);
    ll_from_cur_elm->addIncoming(ll_from_arr_start_ptr, ll_orginal_bb);

    codegen_cb(ll_to_cur_elm, ll_from_cur_elm);

    // Move to the next element in the arra.
    auto ll_to_next_elm   = builder.CreateInBoundsGEP(ll_base_type, ll_to_cur_elm  , { gen_isize(1) });
    auto ll_from_next_elm = builder.CreateInBoundsGEP(ll_base_type, ll_from_cur_elm, { gen_isize(1) });

    // Checking if all objects have been looped over.
    auto ll_done_cond = builder.CreateICmpEQ(ll_to_next_elm, ll_arr_end_ptr);
    builder.CreateCondBr(ll_done_cond, ll_loop_end_bb, ll_loop_bb);

    auto ll_cur_bb = builder.GetInsertBlock();
    ll_to_cur_elm->addIncoming(ll_to_next_elm, ll_cur_bb);
    ll_from_cur_elm->addIncoming(ll_from_next_elm, ll_cur_bb);

    // Continue generating code after the loop.
    builder.SetInsertPoint(ll_loop_end_bb);
}

llvm::Value* acorn::IRGenerator::gen_isize(uint64_t v) {
    auto ll_type = gen_ptrsize_int_type();
    return llvm::ConstantInt::get(ll_type, v, true);
}

void acorn::IRGenerator::gen_branch_on_condition(Expr* cond, llvm::BasicBlock* ll_true_bb, llvm::BasicBlock* ll_false_bb) {
    // See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CodeGenFunction.cpp
    // EmitBranchOnBoolExpr

    if (cond->is(NodeKind::BinOp)) {
        auto bin_op = static_cast<BinOp*>(cond);

        // Binary operators in the form:  a && b
        if (bin_op->op == Token::AndAnd) {
            // Check for case where it is foldable and can just unconditionally branch.
            if (bin_op->is_foldable) {
                auto ll_lhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->lhs));
                auto ll_rhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->rhs));
                if (ll_lhs->isOne() && ll_rhs->isOne()) {
                    builder.CreateBr(ll_true_bb);
                } else {
                    builder.CreateBr(ll_false_bb);
                }
                emit_dbg(di_emitter->emit_location(builder, cond->loc));
                return;
            }

            // Create the true block that will be taken if the lhs is true.
            auto ll_lhs_true_bb = gen_bblock("and.lhs.true", ll_cur_func);
            // Generate the code for branching to the true block if the lhs is
            // true.
            gen_branch_on_condition(bin_op->lhs, ll_lhs_true_bb, ll_false_bb);

            // Continue by determining if the rhs is true when the lhs is true.
            builder.SetInsertPoint(ll_lhs_true_bb);
            gen_branch_on_condition(bin_op->rhs, ll_true_bb, ll_false_bb);
            return;
        }
        // Binary operators in the form:  a || b
        else if (bin_op->op == Token::OrOr) {
            // Check for case where it is foldable and can just unconditionally branch.
            if (bin_op->is_foldable) {
                auto ll_lhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->lhs));
                auto ll_rhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->rhs));
                if (ll_lhs->isOne() || ll_rhs->isOne()) {
                    builder.CreateBr(ll_true_bb);
                } else {
                    builder.CreateBr(ll_false_bb);
                }
                emit_dbg(di_emitter->emit_location(builder, cond->loc));
                return;
            }

            // Create the false block that will be taken if the lhs is false.
            auto ll_lhs_false_bb = gen_bblock("or.lhs.false", ll_cur_func);
            // Generate the code for branching to the false block if the lhs is
            // false.
            gen_branch_on_condition(bin_op->lhs, ll_true_bb, ll_lhs_false_bb);

            // Continue by determining if the statement still might be true even
            // though the lhs was false.
            builder.SetInsertPoint(ll_lhs_false_bb);
            gen_branch_on_condition(bin_op->rhs, ll_true_bb, ll_false_bb);
            return;
        }
    }

    auto ll_cond = gen_condition(cond);
    builder.CreateCondBr(ll_cond, ll_true_bb, ll_false_bb);
    emit_dbg(di_emitter->emit_location(builder, cond->loc));
}

void acorn::IRGenerator::gen_branch_if_not_term(llvm::BasicBlock* ll_bb) {
    auto ll_cur_bb = builder.GetInsertBlock();
    if (!ll_cur_bb->getTerminator()) {
        builder.CreateBr(ll_bb);
    }
}

void acorn::IRGenerator::gen_branch_if_not_term_with_dbg_loc(llvm::BasicBlock* ll_bb, SourceLoc branch_loc) {
    auto ll_cur_bb = builder.GetInsertBlock();
    if (!ll_cur_bb->getTerminator()) {
        builder.CreateBr(ll_bb);
        emit_dbg(di_emitter->emit_location(builder, branch_loc));
    }
}

llvm::Twine acorn::IRGenerator::get_global_name(const char* name) {
    return llvm::Twine(name) + llvm::Twine(context.global_counter++);
}

llvm::GlobalVariable* acorn::IRGenerator::gen_const_global_variable(const llvm::Twine& name,
                                                                    llvm::Type* ll_type,
                                                                    llvm::Constant* ll_initial_value,
                                                                    llvm::GlobalValue::LinkageTypes linkage) {
    return gen_global_variable(name, ll_type, true, ll_initial_value, linkage);
}

llvm::GlobalVariable* acorn::IRGenerator::gen_const_global_struct_variable(const char* name,
                                                                           llvm::StructType* ll_struct_type,
                                                                           llvm::SmallVector<llvm::Constant*>& ll_values,
                                                                           llvm::GlobalValue::LinkageTypes linkage) {

    auto ll_global_name = llvm::Twine(name) + llvm::Twine(context.global_counter++);
    auto ll_global_data = llvm::ConstantStruct::get(ll_struct_type, ll_values);
    return gen_global_variable(ll_global_name, ll_struct_type, true, ll_global_data);
}

llvm::GlobalVariable* acorn::IRGenerator::gen_global_variable(const llvm::Twine& name,
                                                              llvm::Type* ll_type,
                                                              bool is_const,
                                                              llvm::Constant* ll_initial_value,
                                                              llvm::GlobalValue::LinkageTypes linkage) {
    return new llvm::GlobalVariable(
        ll_module,
        ll_type,
        is_const,
        linkage,
        ll_initial_value,
        name
    );
}

void acorn::IRGenerator::store_value_to_any(llvm::Value* ll_any_address, Expr* value, llvm::Value* ll_value) {

    auto ll_any_struct_type = gen_struct_type(context.std_any_struct_type);

    auto ll_field_type_addr = builder.CreateStructGEP(ll_any_struct_type, ll_any_address, 0);
    auto ll_reflect_type = gen_reflect_type_info(value->type);
    builder.CreateStore(ll_reflect_type, ll_field_type_addr);

    bool has_address = value->type->is_aggregate(); // If it is an aggregate it always has an address.
    if (value->kind == NodeKind::IdentRef ||
        value->kind == NodeKind::DotOperator) {
        IdentRef* ref = static_cast<IdentRef*>(value);
        if (ref->is_var_ref()) {
            has_address = true;
        }
    } else if (value->kind == NodeKind::UnaryOp) {
        UnaryOp* unary_op = static_cast<UnaryOp*>(value);
        if (unary_op->op == '*') {
            has_address = true;
        }
    } else if (value->kind == NodeKind::MemoryAccess) {
        has_address = true;
    } else if (value->type->is_aggregate() && value->is(NodeKind::FuncCall)) {
        // Optimized integers as returns from functions even tho they are aggregates
        // do not actually have an address.

        uint64_t aggr_mem_size_bytes = sizeof_type_in_bytes(gen_type(value->type));
        uint64_t aggr_mem_size_bits = aggr_mem_size_bytes * 8;
        if (aggr_mem_size_bits <= ll_module.getDataLayout().getPointerSizeInBits()) {
            has_address = false;
        }
    }

    llvm::Value* ll_value_address = !has_address ? gen_unseen_alloca(value->type, "tmp.any.value")
                                                 : ll_value;

    if (!has_address) {
        builder.CreateStore(ll_value, ll_value_address);
    }

    auto ll_field_value_addr = builder.CreateStructGEP(ll_any_struct_type, ll_any_address, 1);
    builder.CreateStore(ll_value_address, ll_field_value_addr);
}

llvm::Align acorn::IRGenerator::get_alignment(llvm::Type* ll_type) const {
    return llvm::Align(ll_module.getDataLayout().getABITypeAlign(ll_type));
}

uint64_t acorn::IRGenerator::sizeof_type_in_bytes(llvm::Type* ll_type) const {
    return ll_module.getDataLayout().getTypeAllocSize(ll_type);
}

llvm::Value* acorn::IRGenerator::gen_array_memory_access(llvm::Value* ll_address, Type* arr_type, Expr* index) {
    return gen_array_memory_access(ll_address, gen_type(arr_type), gen_rvalue(index));
}

llvm::Value* acorn::IRGenerator::gen_array_memory_access(llvm::Value* ll_address, llvm::Type* ll_arr_type, llvm::Value* ll_index) {
    // First index gives back the offset into the array pointer.
    // Second index gives back a pointer to the element.
    return builder.CreateInBoundsGEP(ll_arr_type, ll_address, { gen_isize(0), ll_index });
}

llvm::Function* acorn::IRGenerator::gen_void_function_decl(llvm::Twine ll_name) {

    auto ll_func_type = llvm::FunctionType::get(llvm::Type::getVoidTy(ll_context), {}, false);

    return llvm::Function::Create(
        ll_func_type,
        llvm::GlobalValue::InternalLinkage,
        ll_name,
        ll_module
    );
}

llvm::AllocaInst* acorn::IRGenerator::gen_unseen_alloca(Type* type, llvm::Twine ll_name) {
    return gen_unseen_alloca(gen_type(type), ll_name);
}

llvm::AllocaInst* acorn::IRGenerator::gen_unseen_alloca(llvm::Type* ll_type, llvm::Twine ll_name) {
    auto ll_backup_insert_block = builder.GetInsertBlock();
    auto ll_entry_block = &ll_cur_func->getEntryBlock();
    if (ll_entry_block->empty()) {
        builder.SetInsertPoint(ll_entry_block);
    } else {
        builder.SetInsertPoint(&ll_entry_block->front());
    }
    auto ll_address = gen_alloca(ll_type, ll_name);
    builder.SetInsertPoint(ll_backup_insert_block);
    return ll_address;
}

bool acorn::IRGenerator::is_pointer_lvalue(Expr* expr) {
    return expr->is_not(NodeKind::Cast) &&     // Casting call 'gen_rvalue' so there is no address to load.
           expr->is_not(NodeKind::FuncCall) && // There is no address to load.
           expr->is_not(NodeKind::This);       // The 'this' pointer has no address.
}

acorn::ImplicitFunc* acorn::IRGenerator::create_implicit_function(ImplicitFunc::ImplicitKind implicit_kind, Struct* structn) {
    auto implicit_func = context.get_allocator().alloc_type<ImplicitFunc>();
    new (implicit_func) ImplicitFunc();
    implicit_func->implicit_kind = implicit_kind;
    implicit_func->structn = structn;
    context.queue_gen_implicit_function(implicit_func);
    return implicit_func;
}

void acorn::IRGenerator::copy_struct_field_constructor(Var* field,
                                                       llvm::Value* ll_to_struct_address,
                                                       llvm::Value* ll_from_struct_address,
                                                       llvm::Type* ll_struct_type) {

    auto ll_to_field_addr   = builder.CreateStructGEP(ll_struct_type, ll_to_struct_address, field->field_idx);
    auto ll_from_field_addr = builder.CreateStructGEP(ll_struct_type, ll_from_struct_address, field->field_idx);

    if (field->type->is_struct()) {
        gen_copy_struct(ll_to_field_addr, ll_from_field_addr, static_cast<StructType*>(field->type));
    } else if (field->type->is_array()) {
        gen_copy_array(ll_to_field_addr, ll_from_field_addr, static_cast<ArrayType*>(field->type));
    } else {
        auto ll_value = builder.CreateLoad(gen_type(field->type), ll_from_field_addr);
        builder.CreateStore(ll_value, ll_to_field_addr);
    }
}

void acorn::IRGenerator::try_move_then_copy_struct_field_constructor(Var* field,
                                                                     llvm::Value* ll_to_struct_address,
                                                                     llvm::Value* ll_from_struct_address,
                                                                     llvm::Type* ll_struct_type) {

    auto ll_to_field_addr   = builder.CreateStructGEP(ll_struct_type, ll_to_struct_address, field->field_idx);
    auto ll_from_field_addr = builder.CreateStructGEP(ll_struct_type, ll_from_struct_address, field->field_idx);

    if (field->type->is_struct()) {
        auto field_struct_type = static_cast<StructType*>(field->type);
        auto structn = field_struct_type->get_struct();
        if (structn->needs_move_call) {
            gen_call_move_constructor(ll_to_field_addr, ll_from_field_addr, structn, field);
        } else {
            gen_copy_struct(ll_to_field_addr, ll_from_field_addr, static_cast<StructType*>(field->type));
        }
    } else if (field->type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(field->type);
        auto base_type = arr_type->get_base_type();
        if (base_type->is_struct()) {
            auto field_struct_type = static_cast<StructType*>(base_type);
            auto structn = field_struct_type->get_struct();
            if (structn->needs_move_call) {
                gen_call_array_move_constructors(ll_to_field_addr,
                                                 ll_from_field_addr,
                                                 arr_type,
                                                 structn);
            } else {
                gen_copy_array(ll_to_field_addr, ll_from_field_addr, static_cast<ArrayType*>(field->type));
            }
        }
    } else {
        auto ll_value = builder.CreateLoad(gen_type(field->type), ll_from_field_addr);
        builder.CreateStore(ll_value, ll_to_field_addr);
    }
}

void acorn::IRGenerator::gen_copy_struct(llvm::Value* ll_to_address,
                                         llvm::Value* ll_from_address,
                                         StructType* struct_type) {

    auto structn = struct_type->get_struct();
    if (structn->needs_copy_call) {
        gen_call_copy_constructor(ll_to_address, ll_from_address, structn);
    } else {
        auto ll_struct_type = gen_struct_type(struct_type);

        // TODO: Is the alignment correct?
        llvm::Align ll_alignment = get_alignment(ll_struct_type);

        builder.CreateMemCpy(
            ll_to_address, ll_alignment,
            ll_from_address, ll_alignment,
            sizeof_type_in_bytes(ll_struct_type)
        );
    }
}

void acorn::IRGenerator::gen_copy_array(llvm::Value* ll_to_address,
                                        llvm::Value* ll_from_address,
                                        ArrayType* arr_type,
                                        Node* lvalue) {

    auto base_type = arr_type->get_base_type();
    if (base_type->is_struct()) {
        auto struct_type = static_cast<StructType*>(base_type);
        auto structn = struct_type->get_struct();
        if (structn->needs_copy_call) {
            gen_call_array_copy_constructors(ll_to_address,
                                             ll_from_address,
                                             arr_type,
                                             structn,
                                             lvalue);
            return;
        }
    }

    auto ll_base_type = gen_type(arr_type->get_base_type());
    uint64_t total_linear_length = arr_type->get_total_linear_length();

    auto ll_elm_type = gen_type(arr_type);
    llvm::Align ll_alignment = get_alignment(ll_elm_type);
    builder.CreateMemCpy(
        ll_to_address, ll_alignment,
        ll_from_address, ll_alignment,
        total_linear_length * sizeof_type_in_bytes(ll_base_type)
    );
    if (should_emit_debug_info && lvalue) {
        di_emitter->emit_location(builder, lvalue->loc);
    }
}

void acorn::IRGenerator::gen_call_copy_constructor(llvm::Value* ll_to_address,
                                                   llvm::Value* ll_from_address,
                                                   Struct* structn,
                                                   Node* lvalue) {

    if (!structn->ll_copy_constructor) {

        auto ll_param_types = llvm::SmallVector<llvm::Type*>{ builder.getPtrTy(), builder.getPtrTy() };
        auto ll_func_type   = llvm::FunctionType::get(builder.getVoidTy(), ll_param_types, false);

        auto ll_name = llvm::Twine("copy.") + structn->name.to_string();
        auto ll_func = llvm::Function::Create(
            ll_func_type,
            llvm::GlobalValue::InternalLinkage,
            ll_name + (structn->copy_constructor ? ".acorn" : ".implicit.acorn"),
            ll_module
        );

        structn->ll_copy_constructor = ll_func;
        if (structn->copy_constructor) {
            structn->copy_constructor->ll_func = ll_func;
            context.queue_gen(structn->copy_constructor);
        } else if (!structn->has_requested_gen_implicits) {
            structn->has_requested_gen_implicits = true;
            auto implicit_func = create_implicit_function(ImplicitFunc::ImplicitKind::CopyConstructor, structn);
            context.queue_gen_implicit_function(implicit_func);
        }
    }

    builder.CreateCall(structn->ll_copy_constructor, { ll_to_address, ll_from_address });
    if (should_emit_debug_info) {
        if (lvalue) {
            di_emitter->emit_location(builder, lvalue->loc);
        } else {
            di_emitter->emit_location_at_last_statement(builder);
        }
    }
}

void acorn::IRGenerator::gen_call_array_move_constructors(llvm::Value* ll_to_address,
                                                          llvm::Value* ll_from_address,
                                                          ArrayType* arr_type,
                                                          Struct* structn,
                                                          Node* lvalue) {
    gen_abstract_double_array_loop(ll_from_address,
                                   ll_from_address,
                                   arr_type,
                                   structn,
                                   [this, structn, lvalue](auto ll_to_elm, auto ll_from_elm) {
        gen_call_move_constructor(ll_to_elm, ll_from_elm, structn, lvalue);
    });
}

void acorn::IRGenerator::gen_call_move_constructor(llvm::Value* ll_to_address,
                                                   llvm::Value* ll_from_address,
                                                   Struct* structn,
                                                   Node* lvalue) {
    if (!structn->ll_move_constructor) {

        auto ll_param_types = llvm::SmallVector<llvm::Type*>{ builder.getPtrTy(), builder.getPtrTy() };
        auto ll_func_type   = llvm::FunctionType::get(builder.getVoidTy(), ll_param_types, false);

        auto ll_name = llvm::Twine("move.") + structn->name.to_string();
        auto ll_func = llvm::Function::Create(
            ll_func_type,
            llvm::GlobalValue::InternalLinkage,
            ll_name + (structn->move_constructor ? ".acorn" : ".implicit.acorn"),
            ll_module
        );

        structn->ll_move_constructor = ll_func;
        if (structn->move_constructor) {
            structn->move_constructor->ll_func = ll_func;
            context.queue_gen(structn->move_constructor);
        } else if (!structn->has_requested_gen_implicits) {
            structn->has_requested_gen_implicits = true;
            auto implicit_func = create_implicit_function(ImplicitFunc::ImplicitKind::MoveConstructor, structn);
            context.queue_gen_implicit_function(implicit_func);
        }
    }

    builder.CreateCall(structn->ll_move_constructor, { ll_to_address, ll_from_address });
    if (should_emit_debug_info) {
        if (lvalue) {
            di_emitter->emit_location(builder, lvalue->loc);
        } else {
            di_emitter->emit_location_at_last_statement(builder);
        }
    }
}

void acorn::IRGenerator::gen_call_array_copy_constructors(llvm::Value* ll_to_address,
                                                          llvm::Value* ll_from_address,
                                                          ArrayType* arr_type,
                                                          Struct* structn,
                                                          Node* lvalue) {
    gen_abstract_double_array_loop(ll_from_address,
                                   ll_from_address,
                                   arr_type,
                                   structn,
                                   [this, structn, lvalue](auto ll_to_elm, auto ll_from_elm) {
        gen_call_copy_constructor(ll_to_elm, ll_from_elm, structn, lvalue);
    });
}

void acorn::IRGenerator::gen_abstract_double_array_loop(llvm::Value* ll_to_address,
                                                        llvm::Value* ll_from_address,
                                                        ArrayType* arr_type,
                                                        Struct* structn,
                                                        const std::function<void(llvm::Value*, llvm::Value*)>& gen_cb,
                                                        Node* lvalue) {
    uint64_t total_linear_length = arr_type->get_total_linear_length();

    auto ll_arr_type = gen_type(arr_type);

    llvm::SmallVector<llvm::Value*> ll_indexes(arr_type->get_depth() + 1, gen_isize(0));
    auto ll_to_arr_start_ptr   = builder.CreateInBoundsGEP(ll_arr_type, ll_to_address, ll_indexes);
    auto ll_from_arr_start_ptr = builder.CreateInBoundsGEP(ll_arr_type, ll_from_address, ll_indexes);

    auto ll_total_arr_length = gen_isize(total_linear_length);

    gen_abstract_double_array_loop(arr_type->get_base_type(),
                                   ll_to_arr_start_ptr,
                                   ll_from_arr_start_ptr,
                                   ll_total_arr_length,
                                   gen_cb);
}

llvm::Value* acorn::IRGenerator::gen_handle_returned_aggregate_obj(FuncCall* call, llvm::Value* ll_ret, const char* tmp_obj_name) {

    // Note: We do not need to allocate the object if the function call
    //       returns a parameter aggregate value because gen_function_call
    //       will create an allocation in that case for us.
    if (ll_ret->getType()->isIntegerTy()) {
        // Because the function returned an integer rather than the struct we
        // must create a temporary struct, and then cast the int into the struct
        // type.
        Type* aggr_type = call->type;
        auto ll_tmp_aggr = gen_unseen_alloca(aggr_type, tmp_obj_name);
        process_destructor_state(aggr_type, ll_tmp_aggr);
        builder.CreateStore(ll_ret, ll_tmp_aggr);
        return ll_tmp_aggr;
    }

    return ll_ret;
}

void acorn::IRGenerator::gen_nop() {
    auto ll_dummy = gen_unseen_alloca(llvm::Type::getInt32Ty(ll_context), "dummy.nop");
    builder.CreateStore(builder.getInt32(0), ll_dummy, true);
}
