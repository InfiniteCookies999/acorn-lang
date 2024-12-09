#include "IRGen.h"

#include "../Context.h"
#include "../Util.h"
#include "../Logger.h"

// Forward declaring static member fields.
int                                  acorn::IRGenerator::global_counter = 0;
llvm::SmallVector<acorn::Var*, 32>   acorn::IRGenerator::incomplete_global_variables;
llvm::SmallVector<acorn::Struct*, 4> acorn::IRGenerator::structs_needing_implicit_functions;
llvm::BasicBlock*                    acorn::IRGenerator::ll_global_init_call_bb;

acorn::IRGenerator::IRGenerator(Context& context)
    : context(context),
      ll_context(context.get_ll_context()),
      ll_module(context.get_ll_module()),
      builder(ll_context) {
}

void acorn::IRGenerator::clear_static_data() {
    global_counter = 0;
    incomplete_global_variables.clear();
    structs_needing_implicit_functions.clear();
    ll_global_init_call_bb = nullptr;
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

llvm::Value* acorn::IRGenerator::gen_node(Node* node) {
    switch (node->kind) {
    case NodeKind::BinOp:
        return gen_binary_op(as<BinOp*>(node));
    case NodeKind::UnaryOp:
        return gen_unary_op(as<UnaryOp*>(node));
    case NodeKind::Var:
        return gen_variable(as<Var*>(node));
    case NodeKind::Number:
        return gen_number(as<Number*>(node));
    case NodeKind::IdentRef:
        return gen_ident_reference(as<IdentRef*>(node));
    case NodeKind::ReturnStmt:
        return gen_return(as<ReturnStmt*>(node));
    case NodeKind::IfStmt:
        return gen_if(as<IfStmt*>(node));
    case NodeKind::ScopeStmt:
        return gen_scope(as<ScopeStmt*>(node));
    case NodeKind::FuncCall:
        return gen_function_call(as<FuncCall*>(node), nullptr);
    case NodeKind::Bool:
        return gen_bool(as<Bool*>(node));
    case NodeKind::String:
        return gen_string(as<String*>(node));
    case NodeKind::Null:
        return gen_null();
    case NodeKind::Cast:
        return gen_cast(as<Cast*>(node));
    case NodeKind::MemoryAccess:
        return gen_memory_access(as<MemoryAccess*>(node));
    case NodeKind::Array:
        return gen_array(as<Array*>(node), nullptr);
    case NodeKind::DotOperator:
        return gen_dot_operator(as<DotOperator*>(node));
    case NodeKind::PredicateLoopStmt:
        return gen_predicate_loop(as<PredicateLoopStmt*>(node));
    case NodeKind::RangeLoopStmt:
        return gen_range_loop(as<RangeLoopStmt*>(node));
    case NodeKind::IteratorLoopStmt:
        return gen_iterator_loop(as<IteratorLoopStmt*>(node));
    case NodeKind::BreakStmt:
    case NodeKind::ContinueStmt:
        return gen_loop_control(as<LoopControlStmt*>(node));
    case NodeKind::SwitchStmt:
        return gen_switch(as<SwitchStmt*>(node));
    case NodeKind::StructInitializer:
        return gen_struct_initializer(as<StructInitializer*>(node), nullptr);
    case NodeKind::This:
        return gen_this(as<This*>(node));
    case NodeKind::SizeOf:
        return gen_sizeof(as<SizeOf*>(node));
    default:
        acorn_fatal("gen_value: Missing case");
        return nullptr;
    }
}

llvm::Value* acorn::IRGenerator::gen_rvalue(Expr* node) {
    auto ll_value = gen_node(node);

    if (node->kind == NodeKind::IdentRef ||
        node->kind == NodeKind::DotOperator) {
        IdentRef* ref = as<IdentRef*>(node);
        if (ref->is_var_ref() &&
            !ref->is_foldable &&   // If the reference is foldable then no memory address is provided for storage to load.
            !ref->type->is_array() // Arrays are essentially always treated like lvalues.
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
        UnaryOp* unary_op = as<UnaryOp*>(node);
        if (unary_op->op == '*') {
            ll_value = builder.CreateLoad(gen_type(node->type), ll_value);
        }
    } else if (node->kind == NodeKind::MemoryAccess) {
        if (!node->type->is_array()) { // Arrays are always lvalues
            ll_value = builder.CreateLoad(gen_type(node->type), ll_value);
        }
    }

    if (node->cast_type) {
        ll_value = gen_cast(node->cast_type, node->type, ll_value);

        node->cast_type = nullptr;
    }

    return ll_value;
}

void acorn::IRGenerator::gen_function_decl(Func* func) {

    if (func->ll_func) {
        return; // Return early because the declaration has already been generated.
    }

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
    if (func->structn) {
        ll_param_types.push_back(builder.getPtrTy());
    }

    if (func->uses_aggr_param) {
        ll_param_types.push_back(builder.getPtrTy());
    }

    for (Var* param : func->params) {
        ll_param_types.push_back(gen_function_param_type(param));
    }

    auto ll_func_type = llvm::FunctionType::get(ll_ret_type, ll_param_types, false);

    auto get_name = [func, &contx = context, is_main] {
        bool dont_fix_name = is_main || func->has_modifier(Modifier::Native);
        if (!func->linkname.empty())
            return llvm::Twine(func->linkname);
        llvm::Twine ll_name = func->name.reduce();
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
        auto ll_param = ll_func->getArg(as<unsigned int>(param_idx));
        ll_param->setName(ll_name);
        ll_param->addAttr(llvm::Attribute::NoUndef);
        return ll_param;
    };

    size_t param_idx = 0;
    if (func->structn) {
        auto ll_param = assign_param_info(param_idx++, "this");
    }
    if (func->uses_aggr_param) {
        auto ll_param = assign_param_info(param_idx, "aggr.ret.addr");
        // Add noalias attribute which tells the compiler no other pointer
        // could possibly point to this pointer.
        ll_param->addAttr(llvm::Attribute::NoAlias);
        ++param_idx;
    }
    
    for (Var* param : func->params) {
        assign_param_info(param_idx++, llvm::Twine("in.") + param->name.reduce());
    }
}

llvm::Type* acorn::IRGenerator::gen_function_return_type(Func* func, bool is_main) {
    if (is_main) {
        return llvm::Type::getInt32Ty(ll_context);
    } else if (func->return_type->is_aggregate()) {
        auto ll_aggr_type = gen_type(func->return_type);
        uint64_t aggr_mem_size = sizeof_type_in_bytes(ll_aggr_type) * 8;
        if (aggr_mem_size <= ll_module.getDataLayout().getPointerSizeInBits()) {
            // The array can fit into an integer.

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

    auto ll_entry = gen_bblock("entry.block", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    // Creating the return block and address if they are needed.
    bool is_main = func == context.get_main_function();
    if (func->num_returns > 1) {
        ll_ret_block = gen_bblock("ret.block", ll_cur_func);
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
        ll_global_init_call_bb = ll_entry;
    }

    // Allocating and storing incoming variables.
    unsigned int param_idx = 0;
    if (func->structn) {
        ll_this = ll_cur_func->getArg(param_idx++);
    }
    if (func->uses_aggr_param) {
        ll_ret_addr = ll_cur_func->getArg(param_idx++);
    }

    for (Var* param : func->params) {
        if (!param->is_aggr_param) {
            auto ll_param_type = gen_type(param->type);
            gen_variable_address(param, ll_param_type);
            builder.CreateStore(ll_cur_func->getArg(param_idx++), param->ll_address);
        } else {
            // There is no reason to store the incoming parameter we can just
            // point to the incoming parameter value.
            param->ll_address = ll_cur_func->getArg(param_idx++);
        }
    }

    // Allocating memory for variables declared in the function.
    for (Var* var : func->vars_to_alloc) {
        if (func->uses_aggr_param && var == func->aggr_ret_var) {
            var->ll_address = ll_ret_addr;
        } else {
            gen_variable_address(var, gen_type(var->type));
        }
    }

    for (Node* node : *func->scope) {
        gen_node(node);
    }

    if (func->num_returns > 1) {
        // Have to check for termination because the last statement might
        // have been an if statement that already branched.
        gen_branch_if_not_term(ll_ret_block);
        builder.SetInsertPoint(ll_ret_block);
    
        if (!func->uses_aggr_param &&
            func->return_type->is_not(context.void_type) && !is_main) {
            // The return value returns to an address so need to load
            // the value.
            auto ll_load_type = is_main ? llvm::Type::getInt32Ty(ll_context) : gen_type(func->return_type);
            ll_ret_value = builder.CreateLoad(ll_load_type, ll_ret_addr, "ret.val");
        }
    }

    if (func->return_type->is(context.void_type) && !is_main) {
        builder.CreateRetVoid();
    } else if (is_main &&
               ((!func->scope->empty() && func->scope->back()->is_not(NodeKind::ReturnStmt))
               || func->scope->empty())) {
        // Implicit return for main function but since the main function always returns an
        // integer it is handled specially.
        builder.CreateRet(builder.getInt32(0));
    } else if (func->uses_aggr_param) {
        builder.CreateRetVoid();
    } else {
        builder.CreateRet(ll_ret_value);
    }
}

void acorn::IRGenerator::gen_variable_address(Var* var, llvm::Type* ll_alloc_type) {
    var->ll_address = gen_alloca(ll_alloc_type, var->name.reduce());
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
        return llvm::PointerType::get(ll_context, 0);
    } else if (param->type->is_struct_type()) {
        auto struct_type = as<StructType*>(param->type);
        
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
    }
    return gen_type(param->type);
}

void acorn::IRGenerator::gen_global_variable_decl(Var* var) {
    
    if (var->ll_address) {
        return; // Return early because the declaration has already been generated.
    }

    // TODO: const: !>_<
    auto ll_linkage = var->has_modifier(Modifier::Native) ? llvm::GlobalValue::ExternalLinkage
                                                          : llvm::GlobalValue::InternalLinkage;
    auto ll_name = var->linkname.empty() ? var->name.reduce() : var->linkname;
    auto ll_final_name = var->has_modifier(Modifier::Native) ? ll_name
                                                             : "global." + ll_name + "." + llvm::Twine(global_counter++);
    auto ll_address = gen_global_variable(ll_final_name,
                                          gen_type(var->type),
                                          false,
                                          nullptr,
                                          ll_linkage);
    ll_address->setAlignment(get_alignment(gen_type(var->type)));

    if (var->has_modifier(Modifier::DllImport)) {
        ll_address->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    }

    var->ll_address = ll_address;

}

void acorn::IRGenerator::gen_global_variable_body(Var* var) {
    if (var->has_modifier(Modifier::Native)) return;

    auto ll_global = llvm::cast<llvm::GlobalVariable>(var->ll_address);
    
    auto gen_constant_value = [this, var]() {
        if (var->assignment->type->is_array()) {
            return gen_constant_array_for_global(as<Array*>(var->assignment));
        } else {
            auto ll_value = gen_rvalue(var->assignment);
            return llvm::cast<llvm::Constant>(ll_value);
        }
    };

    if (!var->assignment) {
        if (var->type->is_struct_type()) {
            // Initialize as many fields as possible then post-pone the initialization
            // of the rest of the values in the initialize function.

            auto struct_type = as<StructType*>(var->type);
            
            llvm::Constant* ll_constant_struct;
            bool all_values_initialized = gen_constant_struct_for_global(struct_type, ll_constant_struct);

            ll_global->setInitializer(ll_constant_struct);
            if (!all_values_initialized) {
                incomplete_global_variables.push_back(var);
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
        incomplete_global_variables.push_back(var);
    }
}

bool acorn::IRGenerator::gen_constant_struct_for_global(StructType* struct_type, llvm::Constant*& ll_constant_struct) {

    bool all_values_initialized = true;

    auto structn        = struct_type->get_struct();
    auto ll_struct_type = gen_struct_type(struct_type);

    llvm::SmallVector<llvm::Constant*, 16> ll_field_values;
            
    for (Var* field : structn->fields) {
        if (field->type->is_struct_type()) {
            auto field_struct_type = as<StructType*>(field->type);
            llvm::Constant* ll_constant;
            all_values_initialized &= gen_constant_struct_for_global(field_struct_type, ll_constant);
            ll_field_values.push_back(ll_constant);
        } else if (field->type->is_array()) {
            if (!field->assignment) {
                ll_field_values.push_back(gen_zero(field->type));
                continue;
            }

            if (field->assignment->is_foldable) {
                auto arr = as<Array*>(field->assignment);
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

    ll_constant_struct = llvm::ConstantStruct::get(ll_struct_type, ll_field_values);

    return all_values_initialized;
}

llvm::Constant* acorn::IRGenerator::gen_constant_array_for_global(Array* arr) {
    // TODO: This is probably broken for assigning directly to a pointer.
    auto arr_type = as<ArrayType*>(arr->get_final_type());
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
        if (field->type->is_struct_type()) {
            auto field_struct_type = as<StructType*>(field->type);
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

void acorn::IRGenerator::finish_incomplete_global_variables() {
    if (incomplete_global_variables.empty()) {
        return;
    }

    auto ll_func = gen_void_function_decl("__acorn.global_init");

    // Move the insert point to the call location for the init globals function.
    builder.SetInsertPoint(ll_global_init_call_bb,
                           ll_global_init_call_bb->getFirstInsertionPt());
    builder.CreateCall(ll_func);

    auto ll_entry = gen_bblock("entry.block", ll_func);
    builder.SetInsertPoint(ll_entry);
    
    for (Var* var : incomplete_global_variables) {
        finish_incomplete_global_variable(var);
    }

    builder.CreateRetVoid();

    incomplete_global_variables.clear();
}

void acorn::IRGenerator::finish_incomplete_global_variable(Var* var) {
    if (!var->assignment && var->type->is_struct_type()) {
        auto struct_type = as<StructType*>(var->type);
        finish_incomplete_struct_type_global(var->ll_address, struct_type);
    } else {
        gen_assignment(var->ll_address, var->type, var->assignment);
    }
}

void acorn::IRGenerator::gen_implicit_structs_functions() {
    for (Struct* structn : structs_needing_implicit_functions) {
        if (structn->ll_default_constructor) {
            gen_default_constructor(structn);
        }
    }
}

void acorn::IRGenerator::gen_default_constructor(Struct* structn) {
    if (!structn->ll_default_constructor) {
        gen_default_constructor_decl(structn);
    }

    cur_struct = structn;

    ll_cur_func = structn->ll_default_constructor;

    auto ll_struct_type = gen_struct_type(structn->struct_type);

    auto ll_entry = gen_bblock("entry.block", ll_cur_func);
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

llvm::Value* acorn::IRGenerator::gen_return(ReturnStmt* ret) {
    bool not_void = cur_func->return_type->is_not(context.void_type);
    bool is_main = cur_func == context.get_main_function();

    if (cur_func->num_returns > 1) {
        if (not_void && !cur_func->aggr_ret_var) {
            // Not-void so store the return value into the return address
            // if it was not already stored due to an aggregate return variable.
            gen_assignment(ll_ret_addr, cur_func->return_type, ret->value);
        } else if (is_main) {
            // Special case for main because even if the user declares main as having
            // type void it still must return an integer.
            builder.CreateStore(builder.getInt32(0), ll_ret_addr);
        }
        // Jumping to the return block.
        builder.CreateBr(ll_ret_block);
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
                llvm::Value* ll_value = gen_node(ret->value);
                if (!ll_value->getType()->isIntegerTy()) {
                    ll_value = builder.CreateLoad(cur_func->ll_aggr_int_ret_type, ll_value);
                }
                ll_ret_value = ll_value;
            }
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

llvm::Value* acorn::IRGenerator::gen_if(IfStmt* ifs) {

    auto load_variable_cond = [this, ifs](Var* var) finline->llvm::Value* {
        
        if (!var->is_foldable) {
            gen_variable(var); // Generate assignment.
        }

        if (ifs->post_variable_cond) {
            return gen_condition(as<Expr*>(ifs->post_variable_cond));
        }

        auto ll_value = var->is_foldable ? gen_node(var->assignment)
                                         : builder.CreateLoad(gen_type(var->type), var->ll_address);
        if (var->type->is_real_pointer()) {
            return builder.CreateIsNotNull(ll_value);
        }
        return ll_value;
    };

    auto ll_then_bb = gen_bblock("if.then", ll_cur_func);
    auto ll_end_bb  = gen_bblock("if.end" , ll_cur_func);
    auto ll_else_bb = ifs->elseif ? gen_bblock("if.else", ll_cur_func) : ll_end_bb;

    // Jump to either the then or else block depending on the condition.
    if (ifs->cond->is(NodeKind::Var)) {
        auto ll_cond = load_variable_cond(as<Var*>(ifs->cond));
        builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);
    } else {
        gen_branch_on_condition(as<Expr*>(ifs->cond), ll_then_bb, ll_else_bb);
    }

    builder.SetInsertPoint(ll_then_bb);
    gen_scope(ifs->scope);

    // Jump to end after the else conidtion block.
    // 
    // Need to use gen_branch_if_not_term because our scope may
    // have ended in a return statement or some other form of jump.
    gen_branch_if_not_term(ll_end_bb);

    if (Node* elif = ifs->elseif) {
        builder.SetInsertPoint(ll_else_bb);
        gen_node(elif);
        gen_branch_if_not_term(ll_end_bb);
    }

    // Continue withe the end block after our if statement.
    builder.SetInsertPoint(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_predicate_loop(PredicateLoopStmt* loop) {
    
    auto ll_end_bb = gen_bblock("loop.end", ll_cur_func);
    auto ll_body_bb = gen_bblock("loop.body", ll_cur_func);
    auto ll_cond_bb = gen_bblock("loop.cond", ll_cur_func);

    loop_break_stack.push_back(ll_end_bb);
    loop_continue_stack.push_back(ll_cond_bb);
    
    builder.CreateBr(ll_cond_bb);
    builder.SetInsertPoint(ll_cond_bb);

    if (loop->cond) {
        gen_cond_branch_for_loop(loop->cond, ll_body_bb, ll_end_bb);
    } else {
        builder.CreateBr(ll_body_bb);
    }

    builder.SetInsertPoint(ll_body_bb);
    gen_scope(loop->scope);

    loop_break_stack.pop_back();
    loop_continue_stack.pop_back();

    // End of loop scope, jump back to the condition.
    gen_branch_if_not_term(ll_cond_bb);

    // Continue generating code after the the loop.
    builder.SetInsertPoint(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_range_loop(RangeLoopStmt* loop) {

    auto ll_end_bb = gen_bblock("loop.end", ll_cur_func);
    auto ll_body_bb = gen_bblock("loop.body", ll_cur_func);
    auto ll_inc_bb = loop->inc ? gen_bblock("loop.inc", ll_cur_func) : nullptr;
    auto ll_cond_bb = gen_bblock("loop.cond", ll_cur_func);
    auto ll_continue_bb = ll_inc_bb ? ll_inc_bb : ll_cond_bb;

    loop_break_stack.push_back(ll_end_bb);
    loop_continue_stack.push_back(ll_continue_bb);

    if (loop->init_node) {
        gen_node(loop->init_node);
    }

    builder.CreateBr(ll_cond_bb);
    builder.SetInsertPoint(ll_cond_bb);

    gen_cond_branch_for_loop(loop->cond, ll_body_bb, ll_end_bb);

    builder.SetInsertPoint(ll_body_bb);
    gen_scope(loop->scope);

    loop_break_stack.pop_back();
    loop_continue_stack.pop_back();

    // End of loop scope, jump back to condition or increment.
    gen_branch_if_not_term(ll_continue_bb);

    if (loop->inc) {
        builder.SetInsertPoint(ll_inc_bb);
        gen_node(loop->inc);
        builder.CreateBr(ll_cond_bb);
    }

    // Continue generating code after the the loop.
    builder.SetInsertPoint(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_iterator_loop(IteratorLoopStmt* loop) {
    
    auto ll_end_bb = gen_bblock("loop.end", ll_cur_func);
    auto ll_body_bb = gen_bblock("loop.body", ll_cur_func);
    auto ll_cond_bb = gen_bblock("loop.cond", ll_cur_func);
    auto ll_inc_bb = gen_bblock("loop.inc", ll_cur_func);

    loop_break_stack.push_back(ll_end_bb);
    loop_continue_stack.push_back(ll_inc_bb);

    if (loop->container->type->is_array()) {

        // Calculate beginning and end of the array for determining
        // the stop condition later.
        // 
        auto ll_ptr_type = builder.getPtrTy();
        auto arr_type = as<ArrayType*>(loop->container->type);
        auto elm_type = arr_type->get_elm_type();
        auto ll_elm_type = gen_type(elm_type);
        auto ll_arr_itr_ptr = gen_unseen_alloca(ll_ptr_type, "arr.itr.ptr");

        bool is_decayed = is_decayed_array(loop->container);
        auto ll_arr_length = gen_isize(arr_type->get_length());
        auto ll_arr_type = gen_type(arr_type);

        auto ll_arr_beg = gen_node(loop->container);
        if (is_decayed) {
            ll_arr_beg = builder.CreateLoad(ll_ptr_type, ll_arr_beg);
        }

        auto ll_arr_end = is_decayed ? builder.CreateInBoundsGEP(ll_elm_type, ll_arr_beg, ll_arr_length)
                                     : gen_array_memory_access(ll_arr_beg, gen_type(arr_type), ll_arr_length);

        builder.CreateStore(ll_arr_beg, ll_arr_itr_ptr);

        // Branch into the condition block and determine when to stop
        // iterating.
        //
        builder.CreateBr(ll_cond_bb);
        builder.SetInsertPoint(ll_cond_bb);

        auto ll_ptr_index1 = builder.CreateLoad(ll_ptr_type, ll_arr_itr_ptr);
        auto ll_cond = builder.CreateICmpNE(ll_ptr_index1, ll_arr_end);

        builder.CreateCondBr(ll_cond, ll_body_bb, ll_end_bb);

        // Generate code for incrementing the array pointer.
        //
        builder.SetInsertPoint(ll_inc_bb);

        auto ll_ptr_index2 = builder.CreateLoad(ll_ptr_type, ll_arr_itr_ptr);
        auto ll_ptr_next = builder.CreateInBoundsGEP(ll_elm_type, ll_ptr_index2, gen_isize(1));
        builder.CreateStore(ll_ptr_next, ll_arr_itr_ptr);

        builder.CreateBr(ll_cond_bb);

        builder.SetInsertPoint(ll_body_bb);
        // Store the pointer value into the variable.
        auto ll_ptr_index3 = builder.CreateLoad(ll_ptr_type, ll_arr_itr_ptr);
        auto ll_index_value = ll_ptr_index3;
        if (!loop->references_memory) {
            ll_index_value = builder.CreateLoad(ll_elm_type, ll_ptr_index3);
        }

        builder.CreateStore(ll_index_value, loop->var->ll_address);

    } else if (loop->container->type->is_range()) {

        auto range_type = as<RangeType*>(loop->container->type);
        auto value_type = range_type->get_value_type();
        auto range = as<BinOp*>(loop->container);

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

        // Generate code for incrementing the index.
        //
        builder.SetInsertPoint(ll_inc_bb);

        ll_index = builder.CreateLoad(ll_index_type, loop->var->ll_address);
        ll_index = builder.CreateNSWAdd(ll_index, ll_one);
        builder.CreateStore(ll_index, loop->var->ll_address);

        builder.CreateBr(ll_cond_bb);

        builder.SetInsertPoint(ll_body_bb);

    } else {
        acorn_fatal("unreachable iteration type");
    }

    gen_scope(loop->scope);

    loop_break_stack.pop_back();
    loop_continue_stack.pop_back();

    // End of loop scope, jump back to increment.
    gen_branch_if_not_term(ll_inc_bb);

    // Continue generating code after the the loop.
    builder.SetInsertPoint(ll_end_bb);
    
    return nullptr;
}

void acorn::IRGenerator::gen_cond_branch_for_loop(Expr* cond, llvm::BasicBlock* ll_body_bb, llvm::BasicBlock* ll_end_bb) {
    auto ll_cond = cond ? gen_condition(cond) : builder.getTrue();
    builder.CreateCondBr(ll_cond, ll_body_bb, ll_end_bb);
}

llvm::Value* acorn::IRGenerator::gen_loop_control(LoopControlStmt* loop_control) {
    auto& target_stack = loop_control->is(NodeKind::BreakStmt) ? loop_break_stack : loop_continue_stack;

    size_t index = target_stack.size() - loop_control->loop_count;

    llvm::BasicBlock* target_bb = target_stack[index];
    builder.CreateBr(target_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_switch(SwitchStmt* switchn) {
    if (switchn->all_conds_foldable) {
        return gen_switch_foldable(switchn);
    }
    return gen_switch_non_foldable(switchn);
}

llvm::Value* acorn::IRGenerator::gen_switch_non_foldable(SwitchStmt* switchn) {

    auto ll_end_bb = gen_bblock("sw.end", ll_cur_func);

    size_t count = 0;
    for (SwitchCase scase : switchn->cases) {
        bool is_last = count == switchn->cases.size() - 1;

        auto ll_then_bb = gen_bblock("sw.then", ll_cur_func);
        auto ll_else_bb = (!is_last || switchn->default_scope) ? gen_bblock("sw.else", ll_cur_func) : ll_end_bb;

        // Jump to either the then or else block depending on the condition.
        if (scase.cond->type->is_ignore_const(context.bool_type)) {
            // It is an operation that results in a boolean so branch on that condition.
            gen_branch_on_condition(as<Expr*>(scase.cond), ll_then_bb, ll_else_bb);
        } else {
            auto ll_cond = gen_equal(gen_rvalue(switchn->on), gen_rvalue(scase.cond));
            builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);
        }

        builder.SetInsertPoint(ll_then_bb);
        gen_scope(scase.scope);

        // Jump to end after the else conidtion block.
        // 
        // Need to use gen_branch_if_not_term because our scope may
        // have ended in a return statement or some other form of jump.
        gen_branch_if_not_term(ll_end_bb);

        if (!is_last) {
            builder.SetInsertPoint(ll_else_bb);
        } else if (switchn->default_scope) {
            builder.SetInsertPoint(ll_else_bb);
            gen_scope(switchn->default_scope);
            gen_branch_if_not_term(ll_end_bb);
        }

        ++count;

    }

    builder.SetInsertPoint(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_switch_foldable(SwitchStmt* switchn) {

    auto ll_end_bb = gen_bblock("sw.end", ll_cur_func);

    size_t num_cases = switchn->cases.size() + switchn->default_scope ? 1 : 0;
    auto ll_default_bb = switchn->default_scope ? gen_bblock("sw.default", ll_cur_func) : ll_end_bb;
    auto ll_switch = builder.CreateSwitch(gen_rvalue(switchn->on), 
                                          ll_default_bb, 
                                          static_cast<unsigned int>(num_cases));
    
    auto gen_case_scope = [this, ll_end_bb](llvm::BasicBlock* ll_case_bb, ScopeStmt* scope) finline {
        builder.SetInsertPoint(ll_case_bb);
        gen_scope(scope);
        gen_branch_if_not_term(ll_end_bb);
    };

    for (SwitchCase scase : switchn->cases) {

        auto ll_case_bb = gen_bblock("sw.case", ll_cur_func);

        auto ll_case_value = llvm::cast< llvm::ConstantInt>(gen_rvalue(scase.cond));
        ll_switch->addCase(ll_case_value, ll_case_bb);

        gen_case_scope(ll_case_bb, scase.scope);

    }

    if (switchn->default_scope) {
        gen_case_scope(ll_default_bb, switchn->default_scope);
    }

    // Need to check for terminator here because it is possible
    // all the branches terminate.
    gen_branch_if_not_term(ll_end_bb);
    
    builder.SetInsertPoint(ll_end_bb);

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_struct_initializer(StructInitializer* initializer, llvm::Value* ll_dest_addr) {
    
    auto ll_struct_type = gen_type(initializer->type);
    if (!ll_dest_addr) {
        ll_dest_addr = gen_unseen_alloca(ll_struct_type, "tmp.struct");
    }
    
    if (initializer->values.empty()) {
        gen_default_value(ll_dest_addr, initializer->type);
        return ll_dest_addr;
    }

    auto structn = initializer->structn;

    llvm::SmallVector<bool, 32> fields_set_list(structn->fields.size(), false);

    unsigned field_idx = 0;
    for (Expr* value : initializer->values) {
        if (value->is(NodeKind::NamedValue)) {
            auto named_val = as<NamedValue*>(value);
            Var* field = structn->fields[named_val->mapped_idx];
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_dest_addr, named_val->mapped_idx);
            gen_assignment(ll_field_addr, field->type, named_val->assignment);
            
            fields_set_list[named_val->mapped_idx] = true;
        } else {
            Var* field = structn->fields[field_idx];
            auto ll_field_addr = builder.CreateStructGEP(ll_struct_type, ll_dest_addr, field_idx);
            gen_assignment(ll_field_addr, field->type, value);
        
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
                gen_assignment(ll_field_addr, field->type, field->assignment);
            } else {
                gen_default_value(ll_field_addr, field->type);
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

llvm::Value* acorn::IRGenerator::gen_scope(ScopeStmt* scope) {
    for (Node* stmt : *scope) {
        gen_node(stmt);
    }
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_variable(Var* var) {
    if (var->is_foldable) return nullptr; // Nothing to generate since the variable doesn't have an address.

    if (var->assignment) {
        gen_assignment(var->ll_address, var->type, var->assignment);
    } else {
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
    }

    acorn_fatal("unreachable: gen_ident_reference()");
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_function_call(FuncCall* call, llvm::Value* ll_dest_addr) {
    
    bool call_func_type = call->site->type->is_function_type();

    bool uses_aggr_param;
    if (!call_func_type) {
        Func* called_func = call->called_func;
        gen_function_decl(called_func);

        if (called_func->ll_intrinsic_id != llvm::Intrinsic::not_intrinsic) {
            auto ll_ret = gen_intrinsic_call(call);
            if (ll_dest_addr) {
                builder.CreateStore(ll_ret, ll_dest_addr);
            }
            return ll_ret;
        }
        
        uses_aggr_param = called_func->uses_aggr_param;
    } else {
        auto func_type = as<FunctionType*>(call->site->type);
        uses_aggr_param = func_type->get_return_type()->is_aggregate();
    }

    auto gen_function_call_arg = [this](Expr* arg) finline -> llvm::Value* {
        if (arg->is(NodeKind::IdentRef)) {
            auto ref = as<IdentRef*>(arg);
            if (ref->is_var_ref() &&
                ref->var_ref->is_param() &&
                ref->var_ref->type->is_array()) {
                // The argument references an already decayed array
                // from a parameter so it must be loaded.
                return builder.CreateLoad(builder.getPtrTy(), gen_node(arg));
            }
        }
        
        if (arg->type->is_struct_type()) {
            
            auto ll_aggr_type = gen_type(arg->type);
            uint64_t aggr_mem_size_bytes = sizeof_type_in_bytes(ll_aggr_type);
            uint64_t aggr_mem_size_bits = aggr_mem_size_bytes * 8;

            bool uses_optimized_int_passing = aggr_mem_size_bits <= ll_module.getDataLayout().getPointerSizeInBits();

            auto finish_struct_arg = [this, uses_optimized_int_passing, aggr_mem_size_bits]
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

                return finish_struct_arg(ll_tmp_arg);
            }
            
            llvm::Align ll_alignment = get_alignment(ll_aggr_type);

            auto ll_copy_from_addr = gen_node(arg);
            auto ll_tmp_arg = gen_unseen_alloca(ll_aggr_type, "aggr.arg");
            builder.CreateMemCpy(
                ll_tmp_arg, ll_alignment,
                ll_copy_from_addr, ll_alignment,
                aggr_mem_size_bytes
            );

            return finish_struct_arg(ll_tmp_arg);
        }

        return gen_rvalue(arg);
    };

    llvm::SmallVector<llvm::Value*> ll_args;
    bool is_member_func = call->called_func && call->called_func->structn;
    size_t ll_num_args = call->called_func ? call->called_func->params.size()
                                           : call->args.size();
    size_t arg_offset = 0;
    
    if (is_member_func) {
        ++ll_num_args;
    }
    if (uses_aggr_param) {
        ++ll_num_args;
    }
    ll_args.resize(ll_num_args);

    // Pass the address of the struct for the member function.
    if (is_member_func) {
        // When a member function is called from a variable the
        // function call has the dot operator as its child. Otherwise
        // the member function must be a call from the current function
        // where the current function is a member function of the same
        // struct.
        //
        llvm::Value* ll_in_this;
        if (call->site->is(NodeKind::DotOperator)) {
            auto dot_operator = as<DotOperator*>(call->site);
            ll_in_this = gen_node(dot_operator->site);
            if (dot_operator->site->type->is_pointer()) {
                // The function call auto-dereferences the pointer.
                ll_in_this = builder.CreateLoad(builder.getPtrTy(), ll_in_this);
            }
        } else {
            ll_in_this = ll_this;
        }

        ll_args[arg_offset++] = ll_in_this;
    }

    // Pass the return address as an argument.
    if (uses_aggr_param) {

        if (!ll_dest_addr) {
            // The user decided to ignore the return or the value
            // is a temporary value so we must create a temporary 
            // value to place the return value into.
            auto ll_ret_type = gen_type(call->type);
            ll_dest_addr = gen_unseen_alloca(ll_ret_type, "tmp.aggr.ret");
        }

        ll_args[arg_offset++] = ll_dest_addr;
    }

    bool uses_default_param_values = call->called_func && call->called_func->default_params_offset != -1;
    if (uses_default_param_values) {
        // Zero initializing the arguments after the start of the default parameter values
        // then filling them in later if they were not filled by the named parameter.
        size_t default_params_offset = call->called_func->default_params_offset + arg_offset;
        std::fill(ll_args.begin() + default_params_offset, ll_args.end(), nullptr);
    }

    for (size_t i = 0; i < call->args.size(); ++i) {
        Expr* arg = call->args[i];
        if (arg->is(NodeKind::NamedValue)) {
            auto named_arg = as<NamedValue*>(arg);
            size_t arg_idx = arg_offset + named_arg->mapped_idx;
            ll_args[arg_idx] = gen_function_call_arg(named_arg->assignment);
        } else {
            ll_args[arg_offset + i] = gen_function_call_arg(arg);
        }
    }

    // Fill in slots with default parameter values.
    if (uses_default_param_values) {
        auto& params = call->called_func->params;
        size_t default_params_offset = call->called_func->default_params_offset;
        size_t start = default_params_offset;
        if (uses_aggr_param) {
            ++start;
        }
        if (is_member_func) {
            ++start;
        }

        size_t param_idx = call->called_func->default_params_offset;
        for (size_t i = start; i < ll_num_args; i++) {
            if (ll_args[i] == nullptr) {
                Expr* arg = params[param_idx]->assignment;
                ll_args[i] = gen_function_call_arg(arg);
            }
            ++param_idx;
        }
    }

    llvm::Value* ll_ret;
    if (!call_func_type) {

        // -- Debug
        // Func* called_func = call->called_func;
        // std::string debug_info = "Calling function with name: " + called_func->name.reduce().str() + "\n";
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

        Func* called_func = call->called_func;
        ll_ret = builder.CreateCall(called_func->ll_func, ll_args);
    } else {

        auto ll_site = gen_rvalue(call->site);
        
        auto func_type    = as<FunctionType*>(call->site->type);
        auto& param_types = func_type->get_param_types();

        auto ll_ret_type = !uses_aggr_param ? gen_type(func_type->get_return_type())
                                            : llvm::Type::getVoidTy(ll_context);
        
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
        ll_ret = builder.CreateCall(ll_func_type, ll_site, ll_args);
    }
    if (!ll_ret->getType()->isVoidTy()) {
        ll_ret->setName("call.ret");
    }

    if (ll_dest_addr && !uses_aggr_param) {
        builder.CreateStore(ll_ret, ll_dest_addr);
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

llvm::Value* acorn::IRGenerator::gen_intrinsic_call(FuncCall* call) {

    auto get_arg = [call](size_t idx) finline -> Expr* {
        if (idx < call->non_named_args_offset) {
            return call->args[idx];
        }

        for (size_t i = call->non_named_args_offset; i < call->args.size(); i++) {
            Expr* arg = call->args[i];
            if (!arg->is(NodeKind::NamedValue)) continue;
        
            auto named_arg = as<NamedValue*>(arg);
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
        
        auto container_type = as<ContainerType*>(arg0->get_final_type());
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
    case llvm::Intrinsic::memset: {
        // We know the first argument is a pointer/array type since
        // it is cast to a void* so to get the type that we are
        // aligning with we need to get the element type
        // of that pointer/array.

        Expr* arg0 = get_arg(0);
        Expr* arg1 = get_arg(1);
        Expr* arg2 = get_arg(2);

        auto container_type = as<ContainerType*>(arg0->get_final_type());
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

llvm::Value* acorn::IRGenerator::gen_cast(Cast* cast) {
    return gen_cast(cast->explicit_cast_type, cast->value->type, gen_rvalue(cast->value));
}

llvm::Value* acorn::IRGenerator::gen_array(Array* arr, llvm::Value* ll_dest_addr) {
    if (!ll_dest_addr) {
        ll_dest_addr = gen_unseen_alloca(arr->type, "tmp.arr");
    }

    auto to_type = arr->get_final_type();;

    auto arr_type = as<ArrayType*>(to_type);
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
            gen_assignment(ll_elm_addr, arr_type->get_elm_type(), elm);
        } else {
            builder.CreateStore(gen_zero(arr_type->get_elm_type()), ll_elm_addr);
        }
    }
    // Zero fill the rest.
    for (uint64_t i = arr->elms.size(); i < ll_arr_type->getNumElements(); ++i) {
        auto ll_elm_addr = gen_gep_index(i);
        builder.CreateStore(gen_zero(arr_type->get_elm_type()), ll_elm_addr);
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
            auto elm_arr = as<Array*>(elm);
            auto elm_arr_type = as<ArrayType*>(elm_arr->get_final_type());
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

    if (is_decayed_array(mem_access->site)) {
        // We have to load again because gen_rvalue says to always treat
        // nodes with array types as lvalues but it was decayed so it
        // was stored into a variable that now needs loaded.
        //
        // Conceptually this makes sense to do here since gen_array_memory_access
        // works under the assumption that the memory given is the lvalue of the
        // array.
        ll_memory = builder.CreateLoad(builder.getPtrTy(), ll_memory);
        mem_access_ptr = true;
    }
    Type* type = mem_access->site->type;

    if (mem_access->site->is(NodeKind::FuncCall)) {
        auto call = as<FuncCall*>(mem_access->site);
        bool uses_aggr_int_ret = false;
        if (!call->site->type->is_function_type()) {
            uses_aggr_int_ret = call->called_func->ll_aggr_int_ret_type;
        }

        if (uses_aggr_int_ret) {
            // Because the function returned an integer rather than the struct we
            // must create a temporary struct, cast the int into the struct, then
            // access its fields.
            Type* array_type = call->called_func->return_type;
            auto ll_tmp_array = gen_unseen_alloca(array_type, "tmp.arr");
            builder.CreateStore(ll_memory, ll_tmp_array);
            ll_memory = ll_tmp_array;
        }
    }

    if (mem_access_ptr) {
        auto ctr_type = as<ContainerType*>(type);
        auto ll_load_type = gen_type(ctr_type->get_elm_type());
        return builder.CreateInBoundsGEP(ll_load_type, ll_memory, {gen_rvalue(mem_access->index)});
    } else {
        // Should be an array type otherwise.
        return gen_array_memory_access(ll_memory, type, mem_access->index);
    }
}

llvm::Value* acorn::IRGenerator::gen_dot_operator(DotOperator* dot) {

    auto gen_struct_type_access = [this, dot](StructType* struct_type) finline{
        auto site = dot->site;

        auto ll_struct_type = gen_type(struct_type);
        auto ll_struct_address = gen_node(site);
        
        if (site->is(NodeKind::FuncCall)) {
            auto call = as<FuncCall*>(site);
            bool uses_aggr_int_ret = false;
            if (!call->site->type->is_function_type()) {
                uses_aggr_int_ret = call->called_func->ll_aggr_int_ret_type;
            }

            // Note: We do not need to allocate the object if the function call
            //       returns a parameter aggregate value because gen_function_call
            //       will create an allocation in that case for us.
            if (uses_aggr_int_ret) {
                // Because the function returned an integer rather than the struct we
                // must create a temporary struct, cast the int into the struct, then
                // access its fields.
                Type* struct_type = call->called_func->return_type;
                auto ll_tmp_struct = gen_unseen_alloca(struct_type, "tmp.struct");
                builder.CreateStore(ll_struct_address, ll_tmp_struct);
                ll_struct_address = ll_tmp_struct;
            }
        }

        // Automatically dereferencing pointers.
        if (site->type->is_pointer() &&
            site->is_not(NodeKind::Cast) &&     // Casting call 'gen_rvalue' so there is no address to load.
            site->is_not(NodeKind::FuncCall) && // There is no address to load.
            site->is_not(NodeKind::This)        // The 'this' pointer has no address.
            ) {
            ll_struct_address = builder.CreateLoad(builder.getPtrTy(), ll_struct_address);
        }

        auto field = dot->var_ref;
        return builder.CreateStructGEP(ll_struct_type, ll_struct_address, field->field_idx);
    };

    if (dot->is_array_length) {
        auto arr_type = as<ArrayType*>(dot->site->type);
        return builder.getInt32(arr_type->get_length());
    } else if (dot->site->type->is_struct_type()) {
        return gen_struct_type_access(as<StructType*>(dot->site->type));
    } else if (dot->site->type->is_pointer()) {
        auto ptr_type = as<PointerType*>(dot->site->type);
        auto elm_type = ptr_type->get_elm_type();
        if (elm_type->is_struct_type()) {
            return gen_struct_type_access(as<StructType*>(elm_type));
        } else {
            return gen_ident_reference(dot);
        }
    } else {
        return gen_ident_reference(dot);
    }
}

void acorn::IRGenerator::gen_assignment(llvm::Value* ll_address, Type* to_type, Expr* value) {
    if (value->is(NodeKind::Array)) {
        gen_array(as<Array*>(value), ll_address);
    } else if (value->is(NodeKind::FuncCall)) {
        gen_function_call(as<FuncCall*>(value), ll_address);
    } else if (value->type->is_array() && to_type->is_array()) {
        // Assigning one array to another so using memory copy for performance
        // sake.
        auto ll_array = gen_node(value);

        auto arr_type = as<ArrayType*>(value->type);
        auto ll_base_type = gen_type(arr_type->get_base_type());
        uint64_t total_linear_length = arr_type->get_total_linear_length();

        auto ll_elm_type = gen_type(arr_type);
        llvm::Align ll_alignment = get_alignment(ll_elm_type);
        builder.CreateMemCpy(
            ll_address, ll_alignment,
            ll_array, ll_alignment,
            total_linear_length * sizeof_type_in_bytes(ll_base_type)
        );
    } else if (value->is(NodeKind::StructInitializer)) {
        auto initializer = as<StructInitializer*>(value);
        gen_struct_initializer(initializer, ll_address);
    } else {
        auto ll_assignment = gen_rvalue(value);
        builder.CreateStore(ll_assignment, ll_address);
    }
}

void acorn::IRGenerator::gen_default_value(llvm::Value* ll_address, Type* type) {
    
    auto type_kind = type->get_kind();
    if (type_kind == TypeKind::Array) {
        auto arr_type = as<ArrayType*>(type);
        auto base_type = arr_type->get_base_type();

        uint64_t total_linear_length = arr_type->get_total_linear_length();

        if (base_type->get_kind() == TypeKind::Struct) {
            auto struct_type = as<StructType*>(base_type);
            auto structn = struct_type->get_struct();

            if (structn->fields_have_assignments) {
                auto ll_arr_type = gen_type(type);

                llvm::SmallVector<llvm::Value*> ll_indexes(arr_type->get_depth() + 1, gen_isize(0));
                auto ll_arr_start_ptr    = builder.CreateInBoundsGEP(ll_arr_type, ll_address, ll_indexes);
                auto ll_total_arr_length = gen_isize(total_linear_length);
                gen_abstract_array_loop(base_type, 
                                        ll_arr_start_ptr, 
                                        ll_total_arr_length, 
                                        [this, structn](auto ll_elm) {
                    gen_default_constructor_call(ll_elm, structn);
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

        auto struct_type = as<StructType*>(type);
        auto structn = struct_type->get_struct();

        auto ll_struct_type = gen_type(type);
        auto ll_alignment = get_alignment(ll_struct_type);

        if (!structn->fields_have_assignments) {
            builder.CreateMemSet(
                ll_address,
                builder.getInt8(0),
                sizeof_type_in_bytes(ll_struct_type),
                ll_alignment
            );
        } else {
            gen_default_constructor_call(ll_address, structn);
        }
    } else {
        builder.CreateStore(gen_zero(type), ll_address);
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

llvm::Value* acorn::IRGenerator::gen_cast(Type* to_type, Type* from_type, llvm::Value* ll_value) {
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
        if (from_type->get_kind() == TypeKind::FuncsRef) {
            return ll_value;
        } else if (from_type->is_pointer()) {
            return ll_value;
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

void acorn::IRGenerator::gen_default_constructor_decl(Struct* structn) {
    auto ll_param_types = llvm::SmallVector<llvm::Type*>{ builder.getPtrTy() };
    auto ll_func_type   = llvm::FunctionType::get(builder.getVoidTy(), ll_param_types, false);

    auto name = structn->name.reduce();
    auto ll_func = llvm::Function::Create(
        ll_func_type,
        llvm::GlobalValue::InternalLinkage,
        llvm::Twine(name) + "." + llvm::Twine(name),
        ll_module
    );

    structn->ll_default_constructor = ll_func;
}

void acorn::IRGenerator::gen_default_constructor_call(llvm::Value* ll_address, Struct* structn) {
    if (!structn->ll_default_constructor) {
        gen_default_constructor_decl(structn);
        structs_needing_implicit_functions.push_back(structn);
    }
    builder.CreateCall(structn->ll_default_constructor, ll_address);
}

void acorn::IRGenerator::struct_array_call_default_constructors() {
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

llvm::Value* acorn::IRGenerator::gen_isize(uint64_t v) {
    auto ll_type = gen_ptrsize_int_type();
    return llvm::ConstantInt::get(ll_type, v, true);
}

void acorn::IRGenerator::gen_branch_on_condition(Expr* cond, llvm::BasicBlock* ll_true_bb, llvm::BasicBlock* ll_false_bb) {
    // See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CodeGenFunction.cpp
    // EmitBranchOnBoolExpr

    if (cond->is(NodeKind::BinOp)) {
        auto bin_op = as<BinOp*>(cond);

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
}

void acorn::IRGenerator::gen_branch_if_not_term(llvm::BasicBlock* ll_bb) {
    auto ll_cur_bb = builder.GetInsertBlock();
    if (!ll_cur_bb->getTerminator()) {
        builder.CreateBr(ll_bb);
    }
}

llvm::Twine acorn::IRGenerator::get_global_name(const char* name) {
    return llvm::Twine(name) + llvm::Twine(global_counter++);
}

llvm::GlobalVariable* acorn::IRGenerator::gen_const_global_variable(const llvm::Twine& name,
                                                                    llvm::Type* ll_type,
                                                                    llvm::Constant* ll_initial_value, 
                                                                    llvm::GlobalValue::LinkageTypes linkage) {
    return gen_global_variable(name, ll_type, true, ll_initial_value, linkage);
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

bool acorn::IRGenerator::is_decayed_array(Expr* arr) {
    if (arr->is_not(NodeKind::IdentRef)) return false;
    auto ref = as<IdentRef*>(arr);
    return ref->is_var_ref() && ref->var_ref->is_param() && ref->type->is_array();
}
