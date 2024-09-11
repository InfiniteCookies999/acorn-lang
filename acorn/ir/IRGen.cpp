#include "IRGen.h"

#include "../Context.h"
#include "../Util.h"
#include "../Logger.h"

int acorn::IRGenerator::global_counter = 0;

acorn::IRGenerator::IRGenerator(Context& context)
    : context(context),
      ll_context(context.get_ll_context()),
      ll_module(context.get_ll_module()),
      builder(ll_context) {
}

void acorn::IRGenerator::gen_function(Func* func) {

    // Logger::debug("generating function: %s", func->name);

    gen_function_decl(func);
    gen_function_body(func);

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
    case NodeKind::ComptimeIfStmt:
        return gen_comptime_if(as<ComptimeIfStmt*>(node));
    case NodeKind::ScopeStmt:
        return gen_scope(as<ScopeStmt*>(node));
    case NodeKind::FuncCall:
        return gen_function_call(as<FuncCall*>(node));
    case NodeKind::Bool:
        return gen_bool(as<Bool*>(node));
    case NodeKind::String:
        return gen_string(as<String*>(node));
    case NodeKind::Null:
        return gen_null();
    case NodeKind::Cast:
        return gen_cast(as<Cast*>(node));
    default:
        acorn_fatal("gen_value: Missing case");
        return nullptr;
    }
}

llvm::Value* acorn::IRGenerator::gen_rvalue(Expr* node) {
    auto ll_value = gen_node(node);

    if (node->kind == NodeKind::IdentRef) {
        IdentRef* ref = as<IdentRef*>(node);
        if (ref->is_var_ref()) {
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

    // Creating the parameter types.
    llvm::SmallVector<llvm::Type*> ll_param_types;
    ll_param_types.reserve(func->params.size());

    for (const Var* param : func->params) {
        ll_param_types.push_back(gen_type(param->type));
    }

    bool is_main = func == context.get_main_function();
    auto ll_func_type = llvm::FunctionType::get(
        is_main ? llvm::Type::getInt32Ty(ll_context) : gen_type(func->return_type),
        ll_param_types,
        false
    );

    auto get_name = [func, &contx = context, is_main] {
        bool dont_fix_name = is_main || func->has_modifier(Modifier::Native);
        llvm::Twine ll_name = func->name.reduce();
        return dont_fix_name ? ll_name : ll_name.concat(".acorn");
    };

    auto ll_func = llvm::Function::Create(
        ll_func_type,
        llvm::Function::ExternalLinkage,
        get_name(),
        ll_module
    );
    func->ll_func = ll_func;

    if (func->has_modifier(Modifier::Native)) {
#ifdef _WIN32
        ll_func->setCallingConv(llvm::CallingConv::X86_StdCall);
#endif
    } else if (!func->has_modifier(Modifier::DllImport)) {
        // Will resolve the symbol within the same compilation unit.
        ll_func->setDSOLocal(true);
        // Tell LLVM that the return value cannot be poisioned or undefined
        // allowing for better optimization.
        if (!func->return_type->is(context.void_type)) {
            ll_func->addRetAttr(llvm::Attribute::NoUndef);
        }
    }

    if (func->has_modifier(Modifier::DllImport)) {
        ll_func->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
    }

    // Assigning names to parameters.
    for (size_t idx = 0; idx < func->params.size(); ++idx) {
        const Var* param = func->params[idx];
        ll_func->getArg(as<unsigned int>(idx))->setName(llvm::Twine("in.") + param->name.reduce());   
    }

    for (size_t idx = 0; idx < func->params.size(); ++idx) {
        // Allows for better optimization by assuming it is not a undefined or poisioned value.
        if (!func->has_modifier(Modifier::DllImport)) {
            ll_func->addParamAttr(as<unsigned int>(idx), llvm::Attribute::NoUndef);
        }
    }
}

void acorn::IRGenerator::gen_function_body(Func* func) {
    if (func->has_modifier(Modifier::Native)) return;

    cur_func    = func;
    ll_cur_func = func->ll_func;

    auto ll_entry = gen_bblock("entry.block", ll_cur_func);
    builder.SetInsertPoint(ll_entry);

    // Creating the return block and address if they are needed.
    bool is_main = func == context.get_main_function();
    if (func->num_returns > 1) {
        ll_ret_block = gen_bblock("ret.block");
        if (func->return_type->is_not(context.void_type)) {
            ll_ret_addr = builder.CreateAlloca(gen_type(func->return_type), nullptr, "ret.addr");
        } else if (is_main) {
            ll_ret_addr = builder.CreateAlloca(llvm::Type::getInt32Ty(ll_context), nullptr, "ret.addr");
        }
    }

    // Allocating and storing incoming variables.
    for (size_t idx = 0; idx < func->params.size(); ++idx) {
        Var* param = func->params[idx];
        gen_variable_address(param);
        builder.CreateStore(ll_cur_func->getArg(idx), param->ll_address);
    }

    // Allocating memory for variables declared in the function.
    for (Var* var : func->vars_to_alloc) {
        gen_variable_address(var);
    }

    for (Node* node : *func->scope) {
        gen_node(node);
    }

    if (func->num_returns > 1) {
        builder.CreateBr(ll_ret_block);
        builder.SetInsertPoint(ll_ret_block);
    
        // The return value returns to an address so need to load
        // the value.
        auto load_type = is_main ? llvm::Type::getInt32Ty(ll_context) : gen_type(func->return_type);
        ll_ret_addr = builder.CreateLoad(load_type, ll_ret_addr, "ret.val");
    }

    if (func->return_type->is(context.void_type) && !is_main) {
        builder.CreateRetVoid();
    } else if (func->num_returns == 0 && is_main) {
        builder.CreateRet(builder.getInt32(0));
    } else {
        builder.CreateRet(ll_ret_value);
    }
}

void acorn::IRGenerator::gen_variable_address(Var* var) {
    var->ll_address = builder.CreateAlloca(gen_type(var->type),
                                           nullptr,
                                           llvm::Twine(var->name.reduce()));
}

llvm::Value* acorn::IRGenerator::gen_return(ReturnStmt* ret) {
    bool not_void = cur_func->return_type->is_not(context.void_type);
    bool is_main = cur_func == context.get_main_function();

    if (cur_func->num_returns > 1) {
        if (not_void) {
            builder.CreateStore(gen_rvalue(ret->value), ll_ret_addr);
        } else if (is_main) {
            builder.CreateStore(builder.getInt32(0), ll_ret_addr);
        }
        // Jumping to the end of the function.
        builder.CreateBr(ll_ret_block);
    } else if (not_void) {
        ll_ret_value = gen_rvalue(ret->value);
    } else if (is_main) {
        ll_ret_value = builder.getInt32(0);
    }

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_if(IfStmt* ifs) {

    llvm::Value* ll_cond = gen_rvalue(ifs->cond);

    auto ll_then_bb = gen_bblock("if.then", ll_cur_func);
    auto ll_end_bb  = gen_bblock("if.end" , ll_cur_func);
    auto ll_else_bb = ifs->elseif ? gen_bblock("if.else", ll_cur_func) : ll_end_bb;

    // Jump to either the then or else block depending on the condition.
    builder.CreateCondBr(ll_cond, ll_then_bb, ll_else_bb);

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

llvm::Value* acorn::IRGenerator::gen_comptime_if(ComptimeIfStmt* ifs) {
    if (ifs->takes_path) {
        for (Node* stmt : *ifs->scope) {
            gen_node(stmt);
        }
    } else if (ifs->elseif && ifs->elseif->is(NodeKind::ComptimeIfStmt)) {
        gen_comptime_if(as<ComptimeIfStmt*>(ifs->elseif));
    } else if (ifs->elseif && ifs->elseif->is(NodeKind::ScopeStmt)) {
        auto else_scope = as<ScopeStmt*>(ifs->elseif);
        for (Node* stmt : *else_scope) {
            gen_node(stmt);
        }
    }
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_scope(ScopeStmt* scope) {
    for (Node* stmt : *scope) {
        gen_node(stmt);
    }
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_variable(Var* var) {

    if (var->assignment) {
        gen_assignment(var->ll_address, var->assignment);
    } else {
        gen_default_value(var->ll_address, var->type);
    }

    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_number(Number* number) {
    return llvm::ConstantInt::get(ll_context, llvm::APInt(32, number->value_u64, true));
}

llvm::Value* acorn::IRGenerator::gen_ident_reference(IdentRef* ref) {
    if (ref->is_var_ref()) {
        return ref->var_ref->ll_address;
    } else if (ref->is_universal_ref()) {
        return gen_rvalue(ref->universal_ref);
    }

    acorn_fatal("unreachable: gen_ident_reference()");
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_function_call(FuncCall* call) {
    
    Func* called_func = call->called_func;
    gen_function_decl(called_func);

    llvm::SmallVector<llvm::Value*> ll_args;
    ll_args.reserve(call->args.size());
    for (Expr* arg : call->args) {
        ll_args.push_back(gen_rvalue(arg));
    }

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
    // for (size_t count = 0; llvm::Argument & ll_arg : called_func->ll_func->args()) {
    //     debug_info += to_string(ll_arg.getType());
    //     if (count + 1 != called_func->ll_func->arg_size()) {
    //         debug_info += ", ";
    //     }
    //     ++count;
    // }
    // debug_info += "]\n";
    // Logger::debug(debug_info.c_str());

    auto ll_ret = builder.CreateCall(called_func->ll_func, ll_args);
    if (!ll_ret->getType()->isVoidTy()) {
        ll_ret->setName("call.ret");
    }
    return ll_ret;
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
        auto global = gen_const_global_variable(get_global_name("global.string"), 
                                                const_array->getType(),
                                                const_array);
        global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        global->setAlignment(llvm::Align(alignment));
        return global;
    };

    if (string->bit_type == String::Str8Bit) {
        if (string->cast_type->is(context.const_char16_ptr_type)) {
            return text_to_global_array(string->text8bit, llvm::Type::getInt16Ty(ll_context), 2);
        } else if (string->cast_type->is(context.const_char32_ptr_type)) {
            return text_to_global_array(string->text8bit, llvm::Type::getInt32Ty(ll_context), 4);
        } else {
            return builder.CreateGlobalStringPtr(string->text8bit, get_global_name("global.string"));
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
    return llvm::Constant::getNullValue(llvm::PointerType::get(ll_context, 0));
}

llvm::Value* acorn::IRGenerator::gen_cast(Cast* cast) {
    return gen_cast(cast->explicit_cast_type, cast->value->type, gen_rvalue(cast->value));
}

void acorn::IRGenerator::gen_assignment(llvm::Value* ll_address, Expr* value) {
    auto ll_assignment = gen_rvalue(value);
    builder.CreateStore(ll_assignment, ll_address);
}

void acorn::IRGenerator::gen_default_value(llvm::Value* ll_address, Type* type) {
    builder.CreateStore(gen_zero(type), ll_address);
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
    case TypeKind::Bool:
        return builder.getInt1(0);
    case TypeKind::ISize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), 0, true);
    case TypeKind::USize:
        return llvm::ConstantInt::get(gen_ptrsize_int_type(), 0, false);
    case TypeKind::Pointer:
        return llvm::Constant::getNullValue(llvm::PointerType::get(ll_context, 0));
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
        if (from_type->is_integer() || from_type->is(context.bool_type)) {
            return builder.CreatePtrToInt(ll_value, llvm::PointerType::get(ll_context, 0), "cast");
        } else if (from_type->is_pointer() || from_type->is(context.null_type)) {
            // Pointer to pointer doesn't need casting because of opaque pointers.
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
        if (from_type->is_integer() || from_type->is(context.bool_type)) {
            return builder.CreateIntCast(ll_value, gen_type(to_type), to_type->is_signed(), "cast");
        } else if (from_type->is_pointer()) {
            return builder.CreateIntToPtr(ll_value, gen_type(to_type), "cast");
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

llvm::BasicBlock* acorn::IRGenerator::gen_bblock(const char* name, llvm::Function* ll_func) {
    return llvm::BasicBlock::Create(ll_context, name, ll_func);
}

llvm::Value* acorn::IRGenerator::gen_isize(uint64_t v) {
    auto ll_type = gen_ptrsize_int_type();
    return llvm::ConstantInt::get(ll_type, v, true);
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
