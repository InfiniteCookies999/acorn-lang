#include "Sema.h"

#include <ranges>

#include "Util.h"
#include "Context.h"
#include "Type.h"
#include "SourceExpansion.h"
#include "ir/IRGen.h"
#include "Module.h"
#include "SourceFile.h"

/* Utility for returning from check functions if an error occures */
#define nvalid(n) !(n->type)
#define yield_if(n) if (nvalid(n)) return;
#define check_and_verify_type(n) check_node(n); yield_if(n)

acorn::Sema::Sema(Context& context, SourceFile* file, Logger& logger)
    : context(context), 
      modl(file->modl), 
      file(file), 
      nspace(file->get_namespace()),
      logger(logger),
      type_table(context.type_table) {
}

bool acorn::Sema::is_potential_main_function(Context& context, const Func* canidate) {
    if (canidate->params.empty()) {
        return true;
    }

    if (canidate->params.size() == 2) {
        Type* param_type1 = canidate->params[0]->type;
        Type* param_type2 = canidate->params[1]->type;
        bool valid_param1 = param_type1->is(context.int_type) ||
                            param_type1->is(context.const_int_type);
        bool valid_param2 = param_type2->is(context.const_char_ptr_ptr_type);
        if (valid_param1 && valid_param2) {
            return true;
        }
    }

    return false;
}

bool acorn::Sema::find_main_function(Context& context) {
    auto& canidates = context.get_canidate_main_funcs();
    for (Func* canidate : canidates) {
        if (!is_potential_main_function(context, canidate)) {
            continue;
        }

        auto& logger = canidate->get_logger();

        if (Func* prev_main = context.get_main_function()) {
            logger.begin_error(canidate->loc, "Duplicate main (entry point) function")
                              .add_line([prev_main](Logger& l) { prev_main->show_prev_declared_msg(l); })
                              .end_error(ErrCode::ParseDuplicateMainFunc);
        } else {
            context.set_main_function(canidate);
        }

        if (canidate->return_type->is_not(context.int_type) &&
            canidate->return_type->is_not(context.void_type)) {
            logger.begin_error(canidate->loc, "main function must have return type of 'int' or 'void'")
                  .end_error(ErrCode::SemaMainBadReturnType);
        }
        if (canidate->modifiers & Modifier::Native) {
            logger.begin_error(canidate->loc, "main function cannot have native modifier")
                .end_error(ErrCode::SemaMainCannotHaveModifier);
        }
        if (canidate->modifiers & Modifier::DllImport) {
            logger.begin_error(canidate->loc, "main function cannot have dllimport modifier")
                  .end_error(ErrCode::SemaMainCannotHaveModifier);
        }
    }
    return false;
}

acorn::Type* acorn::Sema::fixup_type(Type* type) {
    if (type->get_kind() == TypeKind::UnresolvedArrayType) {
        return fixup_unresolved_arr_type(type);
    } else if (type->get_kind() == TypeKind::UnresolvedStructType) {
        return fixup_unresolved_struct_type(type);
    } else if (type->get_kind() == TypeKind::Function) {
        return fixup_function_type(type);
    }
    auto type_kind = type->get_kind();
    if (type_kind == TypeKind::Pointer || type_kind == TypeKind::Array) {
        auto container_type = static_cast<ContainerType*>(type);
        auto base_type = container_type->get_base_type();
        auto fixed_base_type = fixup_type(base_type);
        if (!fixed_base_type) {
            return nullptr;
        }

        if (fixed_base_type != base_type) {
            if (type_kind == TypeKind::Pointer) {
                Type* fixed_ptr_type = type_table.get_ptr_type(fixed_base_type);
                Type* elm_type = container_type->get_elm_type();
                while (elm_type->is_pointer()) {
                    fixed_ptr_type = type_table.get_ptr_type(fixed_ptr_type);
                    container_type = static_cast<ContainerType*>(elm_type);
                    elm_type = container_type->get_elm_type();
                }
                return fixed_ptr_type;
            } else {
                llvm::SmallVector<uint32_t, 8> arr_lengths;
                auto arr_type = static_cast<ArrayType*>(container_type);
                arr_lengths.push_back(arr_type->get_length());

                Type* elm_type = arr_type->get_elm_type();
                while (elm_type->is_array()) {
                    container_type = static_cast<ArrayType*>(elm_type);

                    arr_type = static_cast<ArrayType*>(elm_type);
                    arr_lengths.push_back(arr_type->get_length());
                    elm_type = container_type->get_elm_type();
                }

                auto fixed_arr_type = type_table.get_arr_type(fixed_base_type, arr_lengths.back());
                for (auto itr = arr_lengths.rbegin() + 1; itr != arr_lengths.rend(); ++itr) {
                    uint32_t length = *itr;
                    fixed_arr_type = type_table.get_arr_type(fixed_arr_type, length);
                }
                return fixed_arr_type;
            }
        } else {
            return type;
        }
    }
    return type;
}

acorn::Type* acorn::Sema::fixup_unresolved_arr_type(Type* type) {
    auto unarr_type = static_cast<UnresolvedArrayType*>(type);
    Expr* length_expr = unarr_type->get_length_expr();

    check_node(length_expr);
    if (!length_expr->type) {
        // Failed to check the length expression so returning early.
        return nullptr;
    }
        
    if (!length_expr->type->is_integer()) {
        error(expand(length_expr), "Array length must be an integer type")
            .end_error(ErrCode::SemaArrayLengthNotInteger);
        return nullptr;
    }

    if (length_expr->type->get_number_of_bits() > 32) {
        error(expand(length_expr), "Array length must be less than or equal a 32 bit integer")
            .end_error(ErrCode::SemaArrayLengthTooLargeType);
        return nullptr;
    }

    if (!length_expr->is_foldable) {
        error(expand(length_expr), "Array length must be able to be determined at compile time")
            .end_error(ErrCode::SemaArrayLengthNotComptime);
        return nullptr;
    }

    if (auto ll_length = gen_constant(expand(length_expr), length_expr)) {
        auto ll_int_length = llvm::cast<llvm::ConstantInt>(ll_length);
        uint32_t length = static_cast<uint32_t>(ll_int_length->getZExtValue());

        if (length == 0) {
            error(expand(length_expr), "Array length cannot be zero")
                .end_error(ErrCode::SemaArrayLengthZero);
            return nullptr;
        }

        if (length_expr->type->is_signed() && static_cast<int32_t>(length) < 0) {
            error(expand(length_expr), "Array length cannot be negative")
                .end_error(ErrCode::SemaArrayLengthNegative);
            return nullptr;
        }

        Type* fixed_elm_type = fixup_type(unarr_type->get_elm_type());
        if (!fixed_elm_type) {
            return nullptr;
        }

        // Everything is okay we can create a new array out of it!
        return type_table.get_arr_type(fixed_elm_type, length);
    }

    return nullptr;
}

acorn::Type* acorn::Sema::fixup_assign_det_arr_type(Type* type, Var* var) {

    auto from_type = var->assignment->type;
    auto assign_det_arr_type = static_cast<AssignDeterminedArrayType*>(type);

    if (from_type->get_kind() != TypeKind::Array) {
        error(expand(var), "Expected array type for '%s', but found '%s'", 
              type, from_type)
            .end_error(ErrCode::SemaAssignDetArrTypeReqsArrAssignment);
        return nullptr;
    }

    auto arr_type = static_cast<ArrayType*>(from_type);

    auto elm_type = assign_det_arr_type->get_elm_type();
    auto from_elm_type = arr_type->get_elm_type();

    llvm::SmallVector<uint32_t, 8> arr_lengths;
    arr_lengths.push_back(arr_type->get_length());

    auto report_dimensions_error = [this, var, type, from_type]() finline {
        error(expand(var), "Wrong array dimensions for '%s', but found '%s'",
              type, from_type)
           .end_error(ErrCode::SemaAssignDetArrWrongDimensions);
    };

    while (elm_type->get_kind() == TypeKind::AssignDeterminedArray) {
        if (from_elm_type->get_kind() != TypeKind::Array) {
            report_dimensions_error();
            return nullptr;
        }

        assign_det_arr_type = static_cast<AssignDeterminedArrayType*>(elm_type);
        elm_type = assign_det_arr_type->get_elm_type();
        
        arr_type = static_cast<ArrayType*>(from_elm_type);
        from_elm_type = arr_type->get_elm_type();

        arr_lengths.push_back(arr_type->get_length());
    }

    if (from_elm_type->get_kind() == TypeKind::Array) {
        report_dimensions_error();
        return nullptr;
    }

    Type* fixed_type = fixup_type(elm_type);
    if (!fixed_type) {
        return nullptr;
    }
    
    for (auto itr = arr_lengths.rbegin(); itr != arr_lengths.rend(); ++itr) {
        uint32_t arr_length = *itr;
        fixed_type = type_table.get_arr_type(fixed_type, arr_length);
    }

   return fixed_type;
}

acorn::Type* acorn::Sema::fixup_unresolved_struct_type(Type* type) {
    
    auto unresolved_struct_type = static_cast<UnresolvedStructType*>(type);

    auto name = unresolved_struct_type->get_struct_name();
    auto found_struct = find_struct(name);

    if (!found_struct) {
        error(unresolved_struct_type->get_error_location(),
              "Could not find struct type '%s'", name)
            .end_error(ErrCode::SemaCouldNotFindStructType);
        return nullptr;
    }

    if (!ensure_struct_checked(unresolved_struct_type->get_error_location(), found_struct)) {
        return nullptr;
    }

    if (found_struct->fields_have_errors) {
        return nullptr;
    }

    Type* struct_type = found_struct->struct_type;
    if (unresolved_struct_type->is_const()) {
        struct_type = type_table.get_const_type(struct_type);
    }

    return struct_type;
}

acorn::Type* acorn::Sema::fixup_function_type(Type* type) {
    auto func_type = static_cast<FunctionType*>(type);

    llvm::SmallVector<Type*, 8> fixed_param_types;
    bool type_needed_fixing = false;

    auto ret_type = func_type->get_return_type();
    auto fixed_ret_type = fixup_type(ret_type);
    if (fixed_ret_type != ret_type) {
        type_needed_fixing = true;
    }

    for (auto param_type : func_type->get_param_types()) {
        auto fixed_param_type = fixup_type(param_type);
        fixed_param_types.push_back(fixed_param_type);
        if (fixed_param_type != param_type) {
            type_needed_fixing = true;
        }
    }

    if (type_needed_fixing) {
        return type_table.get_function_type(fixed_ret_type, std::move(fixed_param_types));
    }

    return type;
}

// Statement checking
//--------------------------------------

void acorn::Sema::check_for_duplicate_functions(Namespace* nspace, Context& context) {
    nspace->set_duplicates_checked();

    for (const auto& [_, funcs] : nspace->get_functions()) {
        for (auto itr = funcs.begin(); itr != funcs.end(); ++itr) {
            Func* func = *itr;
            Sema analyzer(context, func->file, func->get_logger());
            analyzer.check_function_decl(func);
        }
    }

    for (const auto& [_, funcs] : nspace->get_functions()) {
        for (auto itr = funcs.begin(); itr != funcs.end(); ++itr) {
            for (auto itr2 = itr+1; itr2 != funcs.end(); ++itr2) {
                if (check_for_duplicate_match(*itr, *itr2)) {
                    break;
                }
            }
        }
    }

    for (auto [_, structn] : nspace->get_structs()) {
        check_for_duplicate_functions(structn->nspace, context);
    }
}

void acorn::Sema::check_all_other_duplicates(Module& modl, Context& context) {
    // Reporting all other duplcates.
    auto get_duplicate_kind_str = [](Decl* decl) finline {
        if (decl->is(NodeKind::Var)) {
            return "variable";
        } else if (decl->is(NodeKind::Struct)) {
            return "struct";
        } else {
            acorn_fatal("unreachable");
            return "";
        }
    };
    auto get_duplcate_err_code = [](Decl* decl) finline {
        if (decl->is(NodeKind::Var)) {
            return ErrCode::SemaDuplicateGlobalVar;
        } else if (decl->is(NodeKind::Struct)) {
            return ErrCode::SemaDuplicateGlobalStruct;
        } else {
            acorn_fatal("unreachable");
            return ErrCode::SemaDuplicateGlobalVar;
        }
    };
    for (auto& [location, decl1, decl2] : modl.get_declaration_duplicates()) {
        report_redeclaration(decl1, 
                             decl2, 
                             get_duplicate_kind_str(decl1),
                             get_duplcate_err_code(decl1));
    }
}

bool acorn::Sema::check_for_duplicate_match(const Func* func1, const Func* func2) {
    auto get_param_count = [](const Func* func) finline {
        if (func->default_params_offset == -1) {
            return func->params.size();
        }
        return func->default_params_offset;
    };

    size_t param_count = get_param_count(func1);
    if (param_count != get_param_count(func2)) {
        return false;
    }

    for (size_t i = 0; i < param_count; i++) {
        Var* param1 = func1->params[i];
        Var* param2 = func2->params[i];
        if (param1->type->is_not(param2->type)) {
            return false;
        }
    }
    report_redeclaration(func1, func2, "function", ErrCode::SemaDuplicateGlobalFunc);
    return true;
}

void acorn::Sema::report_redeclaration(const Decl* decl1, const Decl* decl2, const char* node_kind_str, ErrCode error_code) {
    // Make sure that we report the declaration that comes second within a given file.
    if (decl1->loc.ptr < decl2->loc.ptr) {
        std::swap(decl1, decl2);
    }
    decl1->get_logger().begin_error(decl1->loc, "Duplicate declaration of %s '%s'", node_kind_str, decl1->name)
                       .add_line([decl2](Logger& l) { decl2->show_prev_declared_msg(l); })
                       .end_error(error_code);
}

void acorn::Sema::check_nodes_wrong_scopes(Module& modl) {

    auto report = []<typename T>(Logger& logger,
                                 T loc,
                                 ScopeLocation location,
                                 auto expr_or_stmt_str) finline {
        const char* scope_str;
        if (location == ScopeLocation::Global) {
            scope_str = "global";
        } else if (location == ScopeLocation::Struct) {
            scope_str = "struct";
        } else {
            acorn_fatal("unreachable");
        }
        logger.begin_error(loc, "%s does not belong at %s scope",
                           expr_or_stmt_str, scope_str)
            .end_error(ErrCode::SemaNodeAtWrongScope);
    };

    for (auto [location, node, logger] : modl.get_bad_scope_nodes()) {
        if (node->is_expression()) {
            Expr* expr = static_cast<Expr*>(node);
            report(logger, expand(expr), location, "Expression");
        } else {
            report(logger, node->loc, location, "Statement");
        }
    }
}

void acorn::Sema::resolve_imports(Context& context, SourceFile* file) {
    for (auto& entry : file->get_imports()) {
        resolve_import(context, entry.second);
    }
}

void acorn::Sema::resolve_import(Context& context, ImportStmt* importn) {
    auto& key = importn->key;
    auto& logger = importn->file->logger;

    auto add_namespace = [importn](Namespace* nspace) finline {
        if (importn->is_static) {
            importn->file->add_static_import(nspace);
        } else {
            importn->set_imported_namespace(nspace);
        }
    };

    auto add_struct = [&logger, importn](Struct* structn) finline {
        if (importn->is_static) {
            logger.begin_error(importn->loc, "Cannot static import a struct")
                .end_error(ErrCode::SemaCannotStaticImportStruct);
        } else {
            importn->set_imported_struct(structn);
        }
    };
    
    auto report_could_not_find = [&logger, importn](ImportStmt::KeyPart& key_part) finline {
        logger.begin_error(key_part.error_loc, "Could not find '%s' for import", key_part.name)
                .end_error(ErrCode::SemaCouldNotResolveImport);
    };

    auto report_invalid_import = [&logger, importn]() finline {
        // TODO: better error description.
        logger.begin_error(importn->loc, "Invalid import")
            .end_error(ErrCode::SemaInvalidImport);
    };

    auto report_expect_module_identifier = [&logger](ImportStmt::KeyPart& key_part) finline {
        logger.begin_error(key_part.error_loc, "Importing parent module must always name import as 'module' but found '%s'",
                           key_part.name)
                           .end_error(ErrCode::SemaParentModuleImportExpectModuleIdent);
    };

    if (key.size() == 1 && importn->within_same_modl) {
        auto& modl = importn->file->modl;
        Identifier ident = key[0].name;
        if (auto nspace = modl.find_namespace(ident)) {
            add_namespace(nspace);
        } else if (auto structn = modl.find_struct(ident)) {
            add_struct(structn);
        } else {
            report_could_not_find(key[0]);
        }
        return;
    }

    if (key.size() == 1 && importn->within_parent_modl) {
        // import the module.
        auto& modl = importn->file->modl;
        if (key[0].name != context.module_identifier) {
            report_expect_module_identifier(key[0]);
            return;
        }
        add_namespace(&modl);
        return;
    }
    
    if (key.size() == 1) {
        if (auto modl = context.find_module(key[0].name)) {
            add_namespace(modl);
        } else {
            logger.begin_error(key[0].error_loc, "Could not find module '%s'", key[0].name)
                .end_error(ErrCode::SemaCouldNotResolveImport);
        }
        return;
    }

    if (key.size() == 2 && importn->within_same_modl) {
        auto& modl = importn->file->modl;
        if (auto nspace = modl.find_namespace(key[0].name)) {
            if (auto structn = nspace->find_struct(key[1].name)) {
                add_struct(structn);
            } else {
                logger.begin_error(key[1].error_loc, "Could not find struct '%s'", key[1].name)
                    .end_error(ErrCode::SemaCouldNotResolveImport);
            }
        } else {
            logger.begin_error(key[0].error_loc, "Could not find mamespace '%s'", key[0].name)
                .end_error(ErrCode::SemaCouldNotResolveImport);
        }
        return;
    }

    if (key.size() == 2 && importn->within_parent_modl) {
        auto& modl = importn->file->modl;

        if (key[0].name != context.module_identifier) {
            report_expect_module_identifier(key[0]);
            return;
        }

        if (auto structn = modl.find_struct(key[1].name)) {
            add_struct(structn);
        } else {
            logger.begin_error(key[1].error_loc, "Could not find struct '%s'", key[1].name)
                .end_error(ErrCode::SemaCouldNotResolveImport);
        }
        return;
    }

    if (importn->within_same_modl || importn->within_parent_modl) {
        report_invalid_import();
        return;
    }

    if (key.size() == 2) {
        if (auto modl = context.find_module(key[0].name)) {
            if (auto nspace = modl->find_namespace(key[1].name)) {
                add_namespace(nspace);
            } else if (auto structn = modl->find_struct(key[1].name)) {
                add_struct(structn);
            } else {
                report_could_not_find(key[1]);
            }
        } else {
            report_could_not_find(key[0]);
        }
        return;
    }

    if (key.size() == 3) {
        if (auto modl = context.find_module(key[0].name)) {
            if (auto nspace = modl->find_namespace(key[1].name)) {
                if (auto structn = modl->find_struct(key[2].name)) {
                    add_struct(structn);
                } else {
                    report_could_not_find(key[2]);
                } 
            } else {
                report_could_not_find(key[1]);
            }
        } else {
            report_could_not_find(key[0]);
        }
        return;
    }

    report_invalid_import();
}

bool acorn::Sema::check_function_decl(Func* func) {

    // TODO: This does not take into account that it is possible a
    // parameter with a default value calls another function and that
    // other function's declaration has not been fullfilled yet.
    func->has_checked_declaration = true;
    func->is_checking_declaration = true;

    // -- debug
    // Logger::debug("checking function declaration: %s", func->name.reduce());

    check_modifier_incompatibilities(func);

    if (func->return_type == context.auto_type ||
        func->return_type == context.auto_ptr_type ||
        func->return_type == context.const_auto_type) {
        error(func, "Functions cannot have a return type of '%s'",
              func->return_type)
            .end_error(ErrCode::SemaFuncsCannotReturnAuto);
        return false;
    }

    if (func->return_type->get_kind() == TypeKind::AssignDeterminedArray) {
        error(func, "Functions cannot have return type '%s'", func->return_type)
            .end_error(ErrCode::SemaFuncsCannotHaveAssignDetArrType);
        func->is_checking_declaration = false;
        return false;
    }

    func->return_type = fixup_type(func->return_type);
    if (!func->return_type) {
        // Failed to fixup return type so returning early.
        func->is_checking_declaration = false;
        return false;
    }

    // If we ever decide to allow nesting functions for some reason then this
    // will possibly be a problem because it will have overriden the current scope.
    size_t pcount = 0;
    bool encountered_param_default_value = false;
    Var* last_default_param;
    size_t num_default_params = 0;
    for (Var* param : func->params) {
        // Check for duplicate parameter names.
        //
        // Only have to search parameters that were declared before
        // this variable.
        for (size_t i = 0; i < pcount; i++) {
            if (param->name == func->params[i]->name) {
                error(param, "Duplicate declaration of parameter '%s'", param->name)
                    .end_error(ErrCode::SemaDuplicateParamVariableDecl);
                func->is_checking_declaration = false;
                return false;
            }
        }
        
        check_variable(param);
        if (!param->type) {
            func->is_checking_declaration = false;
            return false;
        }

        if (encountered_param_default_value && !param->assignment) {
            error(expand(last_default_param), "Parameters with default values must come last")
                .end_error(ErrCode::SemaDefaultParamValueMustComeLast);
            func->is_checking_declaration = false;
            return false;
        }

        if (param->assignment) {
            ++num_default_params;
            last_default_param = param;
            encountered_param_default_value = true;
        }

        ++pcount;
    }
    if (num_default_params != 0) {
        func->default_params_offset = func->params.size() - num_default_params;
    }


    if (func->has_modifier(Modifier::Native)) {
        Identifier link_name = func->linkname.empty() ? func->name
                                                      : Identifier::get(func->linkname);
        auto itr = context.ll_intrinsics_table.find(link_name);
        if (itr != context.ll_intrinsics_table.end()) {
            func->ll_intrinsic_id = itr->second;
            
            // Validating that the parameters are correct.

            for (Var* param : func->params) {
                if (param->assignment) {
                    error(expand(param), "Parameters of intrinsic functions cannot have a default value")
                        .end_error(ErrCode::SemaIntrinsicFuncParamDefValue);
                }
            }

            bool found_valid_intrinsic = false;
            for (const auto& intrinsic_def : context.ll_valid_intrinsic_defs) {
                if (intrinsic_def.name != link_name) {
                    continue;
                }
                if (intrinsic_def.return_type->is_not(func->return_type)) {
                    continue;
                }
                if (intrinsic_def.param_types.size() != func->params.size()) {
                    continue;
                }
                bool params_match = true;
                for (size_t i = 0; i < intrinsic_def.param_types.size(); i++) {
                    if (intrinsic_def.param_types[i]->is_not(func->params[i]->type)) {
                        params_match = false;
                        break;
                    }
                }
                if (!params_match) {
                    continue;
                }
                found_valid_intrinsic = true;
                break;
            }

            if (!found_valid_intrinsic) {
                llvm::SmallVector<Context::LLVMIntrinsicDefinition> options;
                for (const auto& intrinsic_def : context.ll_valid_intrinsic_defs) {
                    if (intrinsic_def.name == link_name) {
                        options.push_back(intrinsic_def);
                    }
                }
                if (options.empty()) {
                    acorn_fatal("Placed an intrinsic function in the table but not in the definitions list");
                }

                logger.begin_error(func->loc, "Invalid parameter types or return type for intrinsic functon declaration");
                logger.add_line("Valid intrinsics declarations for '%s':", func->name).remove_period();
                for (const auto& intrinsic_def : options) {
                    std::string func_decl = intrinsic_def.return_type->to_string();
                    func_decl += " ";
                    func_decl += intrinsic_def.name.reduce();
                    func_decl += "(";
                    for (size_t i = 0; i < intrinsic_def.param_types.size(); i++) {
                        func_decl += intrinsic_def.param_types[i]->to_string();
                        if (i + 1 != intrinsic_def.param_types.size()) {
                            func_decl += ", ";
                        }
                    }
                    func_decl += ")";

                    logger.add_line("    - '%s'", func_decl);
                }
                logger.end_error(ErrCode::SemaInvalidIntrinsicDecl);
            }
        }
    }

    func->is_checking_declaration = false;
    return true;
}

void acorn::Sema::check_node(Node* node) {
    switch (node->kind) {
    case NodeKind::Var:
        return check_variable(static_cast<Var*>(node));
    case NodeKind::IdentRef:
        return check_ident_ref(static_cast<IdentRef*>(node), nspace, false);
    case NodeKind::DotOperator:
        return check_dot_operator(static_cast<DotOperator*>(node), false);
    case NodeKind::ReturnStmt:
        return check_return(static_cast<ReturnStmt*>(node));
    case NodeKind::IfStmt: {
        bool _;
        return check_if(static_cast<IfStmt*>(node), _);
    }
    case NodeKind::BinOp:
        return check_binary_op(static_cast<BinOp*>(node));
    case NodeKind::UnaryOp:
        return check_unary_op(static_cast<UnaryOp*>(node));
    case NodeKind::FuncCall:
        return check_function_call(static_cast<FuncCall*>(node));
    case NodeKind::Cast:
        return check_cast(static_cast<Cast*>(node));
    case NodeKind::NamedValue:
        return check_named_value(static_cast<NamedValue*>(node));
    case NodeKind::Number:
    case NodeKind::Bool:
    case NodeKind::String:
    case NodeKind::Null:
        break;
    case NodeKind::ScopeStmt:
        return check_scope(static_cast<ScopeStmt*>(node));
    case NodeKind::Array:
        return check_array(static_cast<Array*>(node), nullptr);
    case NodeKind::MemoryAccess:
        return check_memory_access(static_cast<MemoryAccess*>(node));
    case NodeKind::PredicateLoopStmt:
        return check_predicate_loop(static_cast<PredicateLoopStmt*>(node));
    case NodeKind::RangeLoopStmt:
        return check_range_loop(static_cast<RangeLoopStmt*>(node));
    case NodeKind::IteratorLoopStmt:
        return check_iterator_loop(static_cast<IteratorLoopStmt*>(node));
    case NodeKind::BreakStmt:
    case NodeKind::ContinueStmt:
        return check_loop_control(static_cast<LoopControlStmt*>(node));
    case NodeKind::SwitchStmt:
        return check_switch(static_cast<SwitchStmt*>(node));
    case NodeKind::StructInitializer:
        return check_struct_initializer(static_cast<StructInitializer*>(node));
    case NodeKind::This:
        return check_this(static_cast<This*>(node));
    case NodeKind::SizeOf:
        return check_sizeof(static_cast<SizeOf*>(node));
    default:
        acorn_fatal("check_node(): missing case");
    }
}

bool acorn::Sema::check_comptime_cond(Expr* cond) {
    is_comptime_if_cond = true;
    check_node(cond);
    if (!cond->type) {
        return false;
    }

    if (!cond->is_foldable) {
        error(expand(cond), "Comptime if expects the condition to be determined at compile time")
            .end_error(ErrCode::SemaNotComptimeCompute);
        return false;
    }

    if (!is_condition(cond->type)) {
        return false;
    }

    auto ll_cond = llvm::cast<llvm::ConstantInt>(gen_constant(expand(cond), cond));
    return !ll_cond->isZero();
}

void acorn::Sema::check_function(Func* func) {

    if (func->structn) {
        // The function is a member function so need to check the fields first
        // in case they are referenced.
        if (!ensure_struct_checked(func->loc, func->structn)) {
            return;
        }

        if (func->has_modifier(Modifier::Native)) {
            error(func->get_modifier_location(Modifier::Native), "Member function cannot have native modifier")
                .end_error(ErrCode::SemaMemberFuncHasNativeModifier);
        }
        if (func->has_modifier(Modifier::DllImport)) {
            error(func->get_modifier_location(Modifier::DllImport), "Member function cannot have dllimport modifier")
                .end_error(ErrCode::SemaMemberFuncHasDllimportModifier);
        }
    }

    if (func->has_modifier(Modifier::Native)) {
        if (func->return_type->is_array()) {
            error(func, "Native functions cannot return arrays")
                .end_error(ErrCode::SemaNativeFunctionsCannotReturnArrays);
        } else if (func->return_type->is_struct_type()) {
            error(func, "Native functions cannot return structs")
                .end_error(ErrCode::SemaNativeFunctionsCannotReturnStructs);
        }
        return;
    }

    Struct* prev_struct;
    if (func->structn) {
        prev_struct = cur_struct;
        cur_struct = func->structn;
    }

    // -- debug
    // Logger::debug("checking function: %s", func->name);

    cur_func = func;

    SemScope sem_scope = push_scope();
    for (Var* param : func->params) {
        cur_scope->variables.push_back(param);
    }

    check_scope(func->scope, &sem_scope);
    pop_scope();
    if (!sem_scope.all_paths_return && func->return_type->is_not(context.void_type)) {
        error(func, "Not all function paths return")
            .end_error(ErrCode::SemaNotAllFuncPathReturn);
    }

    bool uses_implicit_return = func->scope->empty();
    if (func->return_type->is(context.void_type) && !uses_implicit_return) {
        auto last_stmt = func->scope->back();
        if (last_stmt->is_not(NodeKind::ReturnStmt)) {
            uses_implicit_return = true;
        }
    }

    if (uses_implicit_return) {
        ++func->num_returns;
    }

    if (func->structn) {
        cur_struct = prev_struct;
    }
}

void acorn::Sema::check_variable(Var* var) {

    auto cleanup = [this, var]() finline {
        var->is_being_checked = false;
        cur_global_var = nullptr;
    };

    check_modifier_incompatibilities(var);

    // Reporting errors if the variable is local to a function and has
    // modifiers.
    if (!var->is_global && !var->is_field() && var->modifiers) {
        for (uint32_t modifier = Modifier::Start;
                      modifier != Modifier::End; modifier *= 2) {
            if (var->modifiers & modifier) {
                error(var->get_modifier_location(modifier), "Modifier cannot apply to local variable")
                    .end_error(ErrCode::SemaLocalVarHasModifiers);
            }
        }
    }

    if (var->is_global) {
        cur_global_var = var;
        if (!var->is_foldable) {
            context.queue_gen(var);
        }
    }
    var->is_being_checked = true;
    var->has_been_checked = true; // Set early to prevent circular checking.

    if (var->assignment) {
        if (var->assignment->is(NodeKind::Array) && var->parsed_type->is_array()) {
            auto arr_type = static_cast<ArrayType*>(var->parsed_type);
            // TODO: We fix up the element twice since we need to fix the element
            //       type up first in order to resolve the assignment and then
            //       again below when calling fixup_assign_det_arr_type.
            //
            //       But it is nessessary to do this first since fixup_assign_det_arr_type
            //       assumes that the assignment has already been checked.
            //
            auto fixed_up_elm_type = fixup_type(arr_type->get_elm_type());
            if (!fixed_up_elm_type) {
                return cleanup();
            }

            check_array(static_cast<Array*>(var->assignment), fixed_up_elm_type);
        } else {
            check_node(var->assignment);
        }
        if (!var->assignment->type) {
            return cleanup();
        }
    }

    if (var->parsed_type->get_kind() == TypeKind::AssignDeterminedArray && !var->assignment) {
        error(var, "Must have assignment to determine array type's length")
            .end_error(ErrCode::SemaVarRequiresAssignForDetArrType);
        return cleanup();
    }

    auto check_for_incomplete_type = [this, var]() finline {
        switch (var->assignment->type->get_kind()) {
        case TypeKind::Void:
        case TypeKind::NamespaceRef:
        case TypeKind::EmptyArray:
        case TypeKind::Null:
        case TypeKind::Range:
            error(expand(var), "Cannot assign incomplete type '%s' to variable declared auto",
                  var->assignment->type)
                .end_error(ErrCode::SemaCannotAssignIncompleteTypeToAuto);
            return false;
        default:
            return true;
        }
    };

    auto report_error_auto_must_have_assignment = [this, var](Type* auto_type) finline {
        error(var, "Must have assignment to determine auto %s", auto_type)
                .end_error(ErrCode::SemaMustHaveAssignmentToDetAuto);
    };

    auto construct_function_type_from_func_ref = [this](Expr* expr) finline {
        // TODO: In the future we will want to allow for selecting for
        // overloaded functions somehow.
        auto ref = static_cast<IdentRef*>(expr);
        auto func = (*ref->funcs_ref)[0];

        llvm::SmallVector<Type*> param_types;
        for (Var* param : func->params) {
            param_types.push_back(param->type);
        }
        return type_table.get_function_type(func->return_type, param_types);
    };

    if (var->parsed_type == context.auto_type || var->parsed_type == context.const_auto_type) {
        if (!var->assignment) {
            report_error_auto_must_have_assignment(var->parsed_type);
            return cleanup();
        }

        if (!check_for_incomplete_type()) {
            return cleanup();
        }
        
        if (var->assignment->type == context.funcs_ref_type) {
            var->type = construct_function_type_from_func_ref(var->assignment);
        } else {
            var->type = var->assignment->type;
        }
        if (var->parsed_type == context.const_auto_type) {
            var->type = type_table.get_const_type(var->type);
        }
    } else if (var->parsed_type == context.auto_ptr_type) {
        if (!var->assignment) {
            report_error_auto_must_have_assignment(context.auto_ptr_type);
            return cleanup();
        }

        if (!check_for_incomplete_type()) {
            return cleanup();
        }

        if (var->assignment->type == context.funcs_ref_type) {
            var->type = construct_function_type_from_func_ref(var->assignment);
        } else {
            var->type = var->assignment->type;
        }

        if (!var->type->is_pointer()) {
            error(expand(var), "Variable with 'auto*' type expects assignment to be a pointer type but found '%s'",
                  var->type)
                .end_error(ErrCode::SemaAutoPtrExpectsPtrType);
            var->type = nullptr;
            return cleanup();
        }
    } else if (var->parsed_type->get_kind() == TypeKind::AssignDeterminedArray) {
        // We have to handle this case as if it is speciial because
        // in no other instance other than variable assignments is the
        // applicable.
        var->type = fixup_assign_det_arr_type(var->parsed_type, var);
    } else {
        var->type = fixup_type(var->parsed_type);
    }

    if (!var->type) {
        // Failed to fixup the type so returning early.
        return cleanup();
    }

    var->is_foldable = var->type->is_number() &&
                       var->type->is_const() &&
                       var->assignment && var->assignment->is_foldable;

    // This must go after checking is the variable is foldable because if
    // it is not then it will not request allocation.
    if (cur_scope && !var->is_param() && !var->is_field()) {
        add_variable_to_local_scope(var);
    }

    if (var->type->is_struct_type()) {
        auto struct_type = static_cast<StructType*>(var->type);
        auto structn = struct_type->get_struct();
        if (structn->default_constructor) {
            context.queue_gen(structn->default_constructor);
        }
    } else if (var->type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(var->type);
        auto base_type = arr_type->get_base_type();
        if (base_type->is_struct_type()) {
            auto struct_type = static_cast<StructType*>(base_type);
            auto structn = struct_type->get_struct();
            if (structn->default_constructor) {
                context.queue_gen(structn->default_constructor);
            }
        }
    }

    if (var->type->is(context.void_type)) {
        error(var, "Variables cannot have type 'void'")
            .end_error(ErrCode::SemaVariableCannotHaveVoidType);
        return cleanup();
    }

    if (!var->assignment && var->type->is_const() &&
        !var->has_modifier(Modifier::Native) && !var->is_param()) {
        error(var, "Variables declared const must be assigned a value")
            .end_error(ErrCode::SemaVariableConstNoValue);
        return cleanup();
    }

    if (var->assignment && !is_assignable_to(var->type, var->assignment)) {
        error(expand(var), get_type_mismatch_error(var->type, var->assignment).c_str())
            .end_error(ErrCode::SemaVariableTypeMismatch);
    } else if (var->assignment) {
        create_cast(var->assignment, var->type);

        if (var->type->is_pointer() && var->assignment->is(NodeKind::Array)) {
            error(expand(var), "Cannot assign an array directly to a pointer")
                .end_error(ErrCode::SemaCannotAssignArrayDirectlyToPtr);
        }
    }

    return cleanup();
}

void acorn::Sema::check_struct(Struct* structn) {
    
    // -- Debug
    // Logger::debug("checking struct: %s", structn->name);

    auto prev_struct = cur_struct;
    cur_struct = structn;

    structn->is_being_checked = true;
    structn->has_been_checked = true; // Set early to prevent circular checking.

    auto process_field_struct_type_state = [this,structn](StructType* field_struct_type) finline {
        auto field_struct = field_struct_type->get_struct();
                
        structn->needs_destruction |= field_struct->needs_destruction;
        structn->fields_need_destruction |= field_struct->needs_destruction;

        structn->needs_copy_call |= field_struct->needs_copy_call;
        structn->fields_need_copy_call |= field_struct->needs_copy_call;

        structn->needs_default_call |= field_struct->needs_default_call;
    };

    uint32_t field_count = 0;
    for (Var* field : structn->fields) {

        field->field_idx = field_count++;

        if (field->has_modifier(Modifier::Native)) {
            error(field->get_modifier_location(Modifier::Native), "Field cannot have native modifier")
                .end_error(ErrCode::SemaFieldHasNativeModifier);
        }
        if (field->has_modifier(Modifier::DllImport)) {
            error(field->get_modifier_location(Modifier::DllImport), "Field cannot have dllimport modifier")
                .end_error(ErrCode::SemaFieldHasDllimportModifier);
        }

        check_variable(field);

        if (!field->type) {
            structn->fields_have_errors = true;
        } else {
            if (field->type->is_struct_type()) {
                process_field_struct_type_state(static_cast<StructType*>(field->type));
            } else if (field->type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(field->type);
                auto base_type = arr_type->get_base_type();
                if (base_type->is_struct_type()) {
                    process_field_struct_type_state(static_cast<StructType*>(base_type));
                }
            }
        }
        if (field->assignment) {
            structn->fields_have_assignments = true;
        }
    }

    structn->needs_default_call |= structn->fields_have_assignments || structn->default_constructor;

    structn->is_being_checked = false;

    // Note: We need to check these after is_being_checked has been set to false because
    //       otherwise the function declaration checking will think there is a circular
    //       dependency with the struct type since the constructor's can reference the
    //       struct type.

    if (structn->destructor) {
        if (structn->destructor->has_checked_declaration || check_function_decl(structn->destructor)) {
            if (!structn->destructor->params.empty()) {
                error(structn->destructor, "Destructors are expected to not have any parameters")
                    .end_error(ErrCode::SemaDestructorsCannotHaveParams);
            }
            context.queue_gen(structn->destructor);
        }
    }
    if (structn->copy_constructor) {
        if (structn->copy_constructor->has_checked_declaration || check_function_decl(structn->copy_constructor)) {
            auto struct_ptr_type = type_table.get_ptr_type(structn->struct_type);
            if (structn->copy_constructor->params.size() != 1) {
                error(structn->copy_constructor,
                      "Copy constructor expected to have expactly one parameter of type '%s'",
                      struct_ptr_type)
                    .end_error(ErrCode::SemaCopyConstructorExpectsOneParam);
            } else {
                auto param1 = structn->copy_constructor->params[0];
                if (param1->type->is_not(struct_ptr_type)) {
                    error(param1,
                          "Copy constructor parameter expected to be of type '%s'",
                          struct_ptr_type)
                        .end_error(ErrCode::SemaCopyConstructorExpectedStructPtrType);
                }
            }
            // TODO: May want to make this only generate if needed.
            context.queue_gen(structn->copy_constructor);
        }
    }

    cur_struct = prev_struct;

}

void acorn::Sema::check_return(ReturnStmt* ret) {
    cur_scope->all_paths_return = true;
    cur_scope->found_terminal = true;

    ++cur_func->num_returns;

    bool is_assignable;
    if (ret->value) {
        if (ret->value->is(NodeKind::Array) && cur_func->return_type->is_array()) {
            auto dest_array_type = static_cast<ArrayType*>(cur_func->return_type);
            check_array(static_cast<Array*>(ret->value), dest_array_type->get_elm_type());
            if (!ret->value->type) {
                return;
            }
        } else {
            check_and_verify_type(ret->value);
        }
        is_assignable = is_assignable_to(cur_func->return_type, ret->value);

        if (cur_func->return_type->is_aggregate() && ret->value->is(NodeKind::IdentRef)) {
            auto ref = static_cast<IdentRef*>(ret->value);
            if (ref->is_var_ref()) {
                // Have to check if it is global because obviously
                // global variables are not local to the function
                // and we do not want to mess up the global variable's
                // memory.
                //
                // Additionally have to check that it is not a parameter because
                // the parameter may be passed in as an aggregate parameter and
                // but the caller thinks a seperate parameter is responsible for
                // handling the aggregate returning. Additionally for certain memory
                // handling cases it is important that these two are distinct or it
                // leads to issues of shared memory.
                // 
                // TODO: We also include a check for if it is a field but this is probably
                // not needed if it is a field of a local variable. Although there would
                // then be extra IRGen complications in referencing the memory. For now it
                // is not allowed.
                //
                if (!ref->var_ref->is_param() && !ref->var_ref->is_global && !ref->var_ref->is_field()) {
                    if (cur_func->aggr_ret_var &&
                        cur_func->aggr_ret_var != ref->var_ref) {
                        // Returning multiple different variable references
                        // so we cannot treat the variable as the return address.
                        cur_func->cannot_use_aggr_ret_var = true;
                        cur_func->aggr_ret_var = nullptr;
                    } else if (!cur_func->cannot_use_aggr_ret_var) {
                        // May be the only variable returned in which case the
                        // variable may be used as the return address.
                        cur_func->aggr_ret_var = ref->var_ref;
                    }
                }
            }
        } else {
            cur_func->cannot_use_aggr_ret_var = true;
        }

    } else {
        is_assignable = cur_func->return_type->is(context.void_type);
    }

    if (!is_assignable) {
        auto show_error = [ret, this]<typename T>(T value) {
            error(ret, get_type_mismatch_error(cur_func->return_type, value).c_str())
                .end_error(ErrCode::SemaFuncReturnTypeMismatch);
        };

        if (ret->value) {
            show_error(ret->value);
        } else {
            show_error(context.void_type);
        }
    }
}

void acorn::Sema::check_if(IfStmt* ifs, bool& all_paths_return) {

    // Must create the scope early so that the scope of
    // the variable is the body of the if statement.
    SemScope sem_scope = push_scope();
    if (ifs->cond->is(NodeKind::Var)) {
            
        Var* var = static_cast<Var*>(ifs->cond);
        check_variable(var);
        
        if (!var->assignment) {
            error(var, "Must assign a value")
                .end_error(ErrCode::SemaExpectedAssignmentIfStmt);
        }
        
        if (var->type) {
            if (!ifs->post_variable_cond && !is_condition(var->type)) {
                error(expand(var), "Variable expected to have a conditional type")
                    .end_error(ErrCode::SemaExpectedCondition);
            } else if (ifs->post_variable_cond) {
                check_node(ifs->post_variable_cond);
                if (ifs->post_variable_cond->type) {
                    check_condition(ifs->post_variable_cond);
                }
            }
        }
        
    } else {
        check_node(ifs->cond);
        Expr* cond = static_cast<Expr*>(ifs->cond);
        if (cond->type) {
            check_condition(cond);
        }
    }
    
    check_scope(ifs->scope, &sem_scope);
    all_paths_return = sem_scope.all_paths_return;
    pop_scope();

    if (ifs->elseif && ifs->elseif->is(NodeKind::IfStmt)) {
        bool all_paths_return2;
        check_if(static_cast<IfStmt*>(ifs->elseif), all_paths_return2);
        all_paths_return &= all_paths_return2;
    } else if (ifs->elseif) {
        check_scope(static_cast<ScopeStmt*>(ifs->elseif));
    } else {
        // If an else does not exist then not all paths return.
        all_paths_return = false;
    }

    cur_scope->all_paths_return = all_paths_return;
    cur_scope->found_terminal = all_paths_return;
}

void acorn::Sema::check_predicate_loop(PredicateLoopStmt* loop) {
    if (loop->cond) {
        check_node(loop->cond);
        if (loop->cond->type) {
            check_condition(loop->cond);
        }
    }

    SemScope sem_scope = push_scope();
    check_loop_scope(loop->scope, &sem_scope);
    pop_scope();
}

void acorn::Sema::check_range_loop(RangeLoopStmt* loop) {
    
    SemScope sem_scope = push_scope();
    if (loop->init_node) {
        check_node(loop->init_node);
    }

    if (loop->cond) {
        check_node(loop->cond);
    }

    if (loop->inc) {
        check_node(loop->inc);
    }

    check_loop_scope(loop->scope, &sem_scope);
    pop_scope();
}

void acorn::Sema::check_iterator_loop(IteratorLoopStmt* loop) {
    
    SemScope sem_scope = push_scope();
    loop->var->type = fixup_type(loop->var->parsed_type);
    auto var_type = loop->var->type;
    if (!var_type) {
        return;
    }

    loop->var->is_foldable = false;
    add_variable_to_local_scope(loop->var);

    check_node(loop->container);

    if (loop->container->type) {
        if (loop->container->type->is_array()) {
            auto arr_type = static_cast<ArrayType*>(loop->container->type);
            auto elm_type = arr_type->get_elm_type();

            if (var_type == context.auto_type) {
                loop->var->type = elm_type;
            } else if (var_type == context.auto_ptr_type) {
                loop->var->type = type_table.get_ptr_type(elm_type);
                loop->references_memory = true;
            } else if (var_type == context.const_auto_type) {
                loop->var->type = type_table.get_const_type(elm_type);
            } else {
                bool types_match = has_valid_constness(var_type, elm_type) && var_type->is_ignore_const(elm_type);
                if (!types_match) {
                    auto ptr_type = type_table.get_ptr_type(elm_type);
                    types_match = has_valid_constness(var_type, ptr_type) && var_type->is_ignore_const(ptr_type);
                    loop->references_memory = true;
                }

                if (!types_match) {
                    error(loop->container, "Cannot assign type '%s' to variable type '%s'",
                          elm_type, var_type)
                        .end_error(ErrCode::SemaCannotAssignIteratorElmTypeToVar);
                }
            }
        } else if (loop->container->type->is_range()) {
            // TODO: deal with auto and auto*

            auto range_type = static_cast<RangeType*>(loop->container->type);
            if (loop->var->type->is_not(range_type->get_value_type())) {
                error(loop->var, "Expected type '%s' for variable", range_type->get_value_type())
                    .end_error(ErrCode::SemaCannotAssignIteratorElmTypeToVar);
            }
        } else {
            error(loop->container, "Cannot iterate over type '%s'", loop->container->type)
                .end_error(ErrCode::SemaCannotIteratorOverType);
        }
    }

    check_loop_scope(loop->scope, &sem_scope);
    pop_scope();
}

void acorn::Sema::check_loop_control(LoopControlStmt* loop_control) {
    cur_scope->found_terminal = true;

    if (loop_depth == 0) {
        if (loop_control->is(NodeKind::BreakStmt)) {
            error(loop_control, "break statements may only be used in loops")
                .end_error(ErrCode::SemaLoopControlOnlyInLoops);
        } else {
            error(loop_control, "continue statements may only be used in loops")
                .end_error(ErrCode::SemaLoopControlOnlyInLoops);
        }
        return;
    }

    if (loop_control->loop_count > loop_depth) {
        Node* error_node = loop_control->loop_count_expr ? static_cast<Node*>(loop_control->loop_count_expr)
                                                         : static_cast<Node*>(loop_control);
        if (loop_control->is(NodeKind::BreakStmt)) {
            error(error_node, "number of requested breaks exceeds the loop nesting depth")
                .end_error(ErrCode::SemaLoopControlLoopCountExceedsLoopDepth);
        } else {
            error(error_node, "number of requested continues exceeds the loop nesting depth")
                .end_error(ErrCode::SemaLoopControlLoopCountExceedsLoopDepth);
        }
    }
}

void acorn::Sema::check_loop_scope(ScopeStmt* scope, SemScope* sem_scope) {
    ++loop_depth;
    check_scope(scope, sem_scope);
    --loop_depth;
    cur_scope->all_paths_return = sem_scope->all_paths_return;
    cur_scope->found_terminal = sem_scope->found_terminal;
}

void acorn::Sema::check_switch(SwitchStmt* switchn) {
    check_and_verify_type(switchn->on);

    if (!switchn->on->type->is_comparable()) {
        error(expand(switchn->on), "Type '%s' cannot be used in a switch", switchn->on->type)
            .end_error(ErrCode::SemaInvalidTypeForSwitch);
        return;
    }

    if (!is_lvalue(switchn->on)) {
        error(expand(switchn->on), "Value switched on is expected to have an address")
            .end_error(ErrCode::SemaSwitchOnExpectedLValue);
    }

    bool all_paths_return = true;
    auto check_case_scope = [this, switchn, &all_paths_return](ScopeStmt* scope) finline {
        SemScope sem_scope = push_scope();
        check_scope(scope, &sem_scope);
        all_paths_return &= sem_scope.all_paths_return;
        pop_scope();
    };

    if (switchn->default_scope) {
        check_case_scope(switchn->default_scope);
    } else {
        all_paths_return = false;
    }

    for (SwitchCase scase : switchn->cases) {
        check_node(scase.cond);
        if (scase.cond->type) {
            bool is_bool_type = scase.cond->type->is_ignore_const(context.bool_type);
            if (!is_bool_type &&
                !switchn->on->type->is_ignore_const(scase.cond->type)) {
                error(expand(scase.cond), "Cannot compare case type '%s' to type '%s'",
                      scase.cond->type, switchn->on->type)
                    .end_error(ErrCode::SemaCannotCompareCaseType);
            }

            if (scase.cond->is(NodeKind::Bool)) {
                auto bool_cond = static_cast<Bool*>(scase.cond);
                error(scase.cond, "Cannot compare to %s", bool_cond->value ? "true" : "false")
                    .end_error(ErrCode::SemaCaseCannotBeBoolLiteral);
            }
        
            if (!scase.cond->is_foldable || is_bool_type) {
                switchn->all_conds_foldable = false;
            }
        }
        check_case_scope(scase.scope);
    }

    cur_scope->all_paths_return = all_paths_return;
    cur_scope->found_terminal = all_paths_return;
}

void acorn::Sema::check_struct_initializer(StructInitializer* initializer) {

    auto name = initializer->ref->ident;
    auto structn = find_struct(name);

    if (!structn) {
        error(initializer->ref, "Failed to find struct type '%s'", 
              name)
            .end_error(ErrCode::SemaStructInitFailedToFindStruct);
        return;
    }

    if (!ensure_struct_checked(initializer->ref->loc, structn)) {
        return;
    }

    if (structn->fields_have_errors) {
        return;
    }

    if (!structn->constructors.empty()) {
        // Need to check the arguments before trying to select a constructor.
        bool args_have_errors = false;
        for (auto arg : initializer->values) {
            check_node(arg);
            if (!arg->type) args_have_errors = true;
        }
        if (args_have_errors) return;

        Func* found_constructor = check_function_decl_call(initializer,
                                                           initializer->values,
                                                           initializer->non_named_vals_offset,
                                                           structn->constructors);
        if (!found_constructor) {
            return;
        }

        initializer->is_foldable = false;
        initializer->called_constructor = found_constructor;
        initializer->type = structn->struct_type;
        context.queue_gen(found_constructor);

        return;
    }
    
    // Check for duplicate values.
    if (initializer->non_named_vals_offset != -1) {
        bool dup_named_vals = false;
        for (size_t i = initializer->non_named_vals_offset; i < initializer->values.size(); i++) {
            auto val = initializer->values[i];
            if (!val->is(NodeKind::NamedValue)) {
                continue;
            }

            auto named_val = static_cast<NamedValue*>(val);
            for (size_t j = i + 1; j < initializer->values.size(); j++) {
                auto other_val = initializer->values[j];
                if (!other_val->is(NodeKind::NamedValue)) {
                    continue;
                }
                
                auto other_named_val = static_cast<NamedValue*>(other_val);
                if (named_val->name == other_named_val->name) {
                    error(other_named_val, "Duplicated named value '%s'", named_val->name)
                        .end_error(ErrCode::SemaDuplicatedNamedStructInitVal);
                    dup_named_vals = true;
                    break;
                }
            }
        }
    
        if (dup_named_vals) {
            return;
        }
    }
    
    auto& values = initializer->values;
    if (values.size() > structn->fields.size()) {
        error(initializer, "More values than struct fields")
            .end_error(ErrCode::SemaStructTooManyFields);
        return;
    }

    initializer->structn = structn;

    bool named_values_out_of_order = false;
    uint32_t named_value_high_idx = 0;
    for (size_t i = 0; i < values.size(); i++) {
        Expr* value = values[i];
        check_and_verify_type(value);

        Var* field;
        if (value->is(NodeKind::NamedValue)) {
            // Handle named arguments by finding the corresponding parameter

            auto named_value = static_cast<NamedValue*>(value);
            value = named_value->assignment;

            field = structn->nspace->find_variable(named_value->name);

            // Check to make sure we found the field by the given name.
            if (!field) {
                error(expand(value), "Could not find field '%s' for named value", named_value->name)
                    .end_error(ErrCode::SemaStructInitCouldNotFindField);
                return;
            }

            if (field->field_idx != i) {
                named_values_out_of_order = true;
            }
            named_value_high_idx = std::max(field->field_idx, named_value_high_idx);
            named_value->mapped_idx = field->field_idx;
        } else {
            
            field = structn->fields[i];

            // Cannot determine the order of the values if the
            // non-named values come after the named values
            // and the named values are not in order.
            if (named_values_out_of_order || named_value_high_idx > i) {
                error(expand(value), "Value %s causes the values to be out of order", i + 1)
                    .end_error(ErrCode::SemaStructInitValuesOutOfOrder);
                return;
            }
        }
    
        if (!is_assignable_to(field->type, value)) {
            error(expand(value), "Field '%s'. %s", 
                  field->name,
                  get_type_mismatch_error(field->type, value).c_str())
                .end_error(ErrCode::SemaFieldInitTypeMismatch);
        }

        create_cast(value, field->type);
    }

    initializer->is_foldable = false;
    initializer->type = structn->struct_type;
}

void acorn::Sema::check_this(This* thisn) {
    if (!cur_struct) {
        error(thisn, "Cannot use 'this' pointer outside a struct")
            .end_error(ErrCode::SemaThisNotInStruct);
        return;
    }
    
    thisn->type = type_table.get_ptr_type(cur_struct->struct_type);
    thisn->is_foldable = false;
}

void acorn::Sema::check_sizeof(SizeOf* sof) {
    
    sof->type = context.int_type;
}

acorn::Sema::SemScope acorn::Sema::push_scope() {
    SemScope sem_scope;
    sem_scope.parent = cur_scope;
    cur_scope = &sem_scope;
    return sem_scope;
}

void acorn::Sema::pop_scope() {
    cur_scope = cur_scope->parent;
}

void acorn::Sema::check_scope(ScopeStmt* scope) {
    SemScope sem_scope = push_scope();
    check_scope(scope, &sem_scope);
    pop_scope();
}


void acorn::Sema::check_scope(ScopeStmt* scope, SemScope* sem_scope) {
    
    for (Node* stmt : *scope) {
        if (sem_scope && sem_scope->found_terminal) {
            error(stmt, "Unreachable code")
                .end_error(ErrCode::SemaUnreachableStmt);
            break;
        }

        switch (stmt->kind) {
        case NodeKind::ReturnStmt:
        case NodeKind::Var:
        case NodeKind::Func:
        case NodeKind::Struct:
        case NodeKind::FuncCall:
        case NodeKind::IfStmt:
        case NodeKind::ScopeStmt:
        case NodeKind::PredicateLoopStmt:
        case NodeKind::RangeLoopStmt:
        case NodeKind::IteratorLoopStmt:
        case NodeKind::BreakStmt:
        case NodeKind::ContinueStmt:
        case NodeKind::SwitchStmt:
            break;
        case NodeKind::BinOp: {
            BinOp* bin_op = static_cast<BinOp*>(stmt);

            switch (bin_op->op) {
            case '=':
            case Token::AddEq:
            case Token::SubEq:
            case Token::MulEq:
            case Token::DivEq:
            case Token::ModEq:
            case Token::AndEq:
            case Token::OrEq:
            case Token::CaretEq:
            case Token::TildeEq:
            case Token::LtLtEq:
            case Token::GtGtEq:
                goto ContinueToCheckNodeLab;
            default:
                break;
            }

            goto IncompleteStatementLab;
        }
        case NodeKind::UnaryOp: {
            UnaryOp* unary_op = static_cast<UnaryOp*>(stmt);

            if (unary_op->op == Token::AddAdd || unary_op->op == Token::SubSub ||
                unary_op->op == Token::PostAddAdd || unary_op->op == Token::PostSubSub) {
                goto ContinueToCheckNodeLab;
            }

            goto IncompleteStatementLab;
        }
        case NodeKind::VarList: {
            acorn_fatal("Variable list used as scope node");
            break;
        }
        default:
        IncompleteStatementLab:
            error(stmt, "Incomplete statement")
                .end_error(ErrCode::SemaIncompleteStmt);
            continue;
        }

    ContinueToCheckNodeLab:
        
        if (stmt->is(NodeKind::Func)) {
            error(stmt, "Functions cannot be declared within another function")
                .end_error(ErrCode::SemaNoLocalFuncs);
        } else if (stmt->is(NodeKind::Struct)) {
            error(stmt, "Structs cannot be declared within a function")
                .end_error(ErrCode::SemaNoLocalStructs);
        } else {
            check_node(stmt);
        }
    }
}

void acorn::Sema::add_variable_to_local_scope(Var* var) {
    if (Var* prev_var = cur_scope->find_variable(var->name)) {
        logger.begin_error(var->loc, "Duplicate declaration of variable '%s'", var->name)
                .add_line([prev_var](Logger& l) { prev_var->show_prev_declared_msg(l); })
                .end_error(ErrCode::SemaDuplicateLocVariableDecl);
    } else {
        cur_scope->variables.push_back(var);
        if (!var->is_foldable) {
            cur_func->vars_to_alloc.push_back(var);
        }
    }
}

// Expression checking
//--------------------------------------

void acorn::Sema::check_binary_op(BinOp* bin_op) {
    
    Expr* lhs = bin_op->lhs;
    Expr* rhs = bin_op->rhs;

    auto error_cannot_apply = [this, bin_op, lhs, rhs](Expr* expr) finline {
        auto op_str = token_kind_to_string(context, bin_op->op);
        error(expand(expr), "Operator %s cannot apply to type '%s'.   ('%s' %s '%s')",
                op_str, expr->type, lhs->type, op_str, rhs->type)
            .end_error(ErrCode::SemaBinOpTypeCannotApply);
    };

    auto error_mismatched = [this, bin_op, lhs, rhs]() finline {
        error(expand(bin_op), "Invalid operation. Mismatched types ('%s' %s '%s')",
              lhs->type, token_kind_to_string(context, bin_op->op), rhs->type)
              .end_error(ErrCode::SemaBinOpTypeMismatch);
    };

    auto get_integer_type = [=, this](bool enforce_lhs) finline -> Type* {
        auto rhs_type = rhs->type;
        auto lhs_type = lhs->type;
        if (rhs_type->is_ignore_const(lhs_type)) {
            return lhs_type->remove_all_const();
        }

        // Allow numeric operations on integer types when one of them is
        // an integer literal but without an explicit type.
        if (lhs_type->is_integer() && rhs_type->is_integer()) {
            if (rhs->is(NodeKind::Number) &&
                (rhs_type->is(context.int_type) || rhs_type->is(context.char_type))) {
                return lhs_type->remove_all_const();
            }
            if (!enforce_lhs && lhs->is(NodeKind::Number) &&
                (lhs_type->is(context.int_type) || lhs_type->is(context.char_type))) {
                return rhs_type->remove_all_const();
            }
        }

        return nullptr;
    };

    auto get_number_type = [=](bool enforce_lhs) finline -> Type* {
        // TODO: If one of the types is a float and the other an integer 
        //       we may want the bitwidth to be less than or equal to the
        //       size of the float for the integer.
        
        if (lhs->type->is_float()) {
            if (rhs->type->is_float()) {
                uint32_t lbits = rhs->type->get_number_of_bits();
                uint32_t rbits = lhs->type->get_number_of_bits();
                return lbits > rbits ? lhs->type : rhs->type;
            } else if (rhs->type->is_integer()) {
                return lhs->type->remove_all_const();
            }
            return nullptr;
        } else if (rhs->type->is_float()) {
            if (lhs->type->is_float()) {
                uint32_t lbits = rhs->type->get_number_of_bits();
                uint32_t rbits = lhs->type->get_number_of_bits();
                return lbits > rbits ? lhs->type : rhs->type;
            } else if (lhs->type->is_integer()) {
                return rhs->type->remove_all_const();
            }
            return nullptr;
        }
        return get_integer_type(enforce_lhs);
    };

    //lhs, rhs, error_cannot_apply, error_mismatched, valid_number_compare
    auto get_add_sub_mul_type = [=, this](bool enforce_lhs) finline->Type* {
        // valid pointer arithmetic cases:
        // 
        // ptr + int
        // int + ptr
        // 
        // ptr - int
        // ptr - ptr
        
        if (lhs->type->is_pointer() || lhs->type->is_array()) {
            // Pointer arithmetic
            if (bin_op->op == '+' && !rhs->type->is_integer()) {
                error_mismatched();
                return nullptr;
            } else if (!(rhs->type->is_integer() || rhs->type->is(lhs->type))) {
                error_mismatched();
                return nullptr;
            }

            if (lhs->type->is_pointer() && rhs->type->is_pointer()) {
                return context.isize_type;
            }
            if (lhs->type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(lhs->type);
                return type_table.get_ptr_type(arr_type->get_elm_type());
            }
            return lhs->type;
        } else if (!enforce_lhs && rhs->type->is_pointer() || rhs->type->is_array()) {
            // Pointer arithmetic
            if (bin_op->op == '+' && !lhs->type->is_integer()) {
                error_mismatched();
                return nullptr;
            } else if (bin_op->op == '-') {
                // ptr - ptr   case would have already been handled.
                error_mismatched();
                return nullptr;
            }

            if (rhs->type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(rhs->type);
                return type_table.get_ptr_type(arr_type->get_elm_type());
            }
            return rhs->type;
        }

        if (!lhs->type->is_number()) {
            error_cannot_apply(lhs);
            return nullptr;
        }
        if (!rhs->type->is_number()) {
            error_cannot_apply(rhs);
            return nullptr;
        }
        if (Type* result_type = get_number_type(enforce_lhs)) {
            return result_type;
        }

        error_mismatched();
        return nullptr;
    };

    auto get_div_mod_type = [=, this](bool enforce_lhs) finline -> Type* {
        if (!lhs->type->is_number()) {
            error_cannot_apply(lhs);
            return nullptr;
        }   
        if (!rhs->type->is_number()) {
            error_cannot_apply(rhs);
            return nullptr;
        }
            
        check_division_by_zero(expand(bin_op), rhs);
        
        if (Type* result_type = get_number_type(enforce_lhs)) {
            return result_type;
        }
        
        error_mismatched();
        return nullptr;
    };

    auto get_logical_bitwise_type = [=, this](bool enforce_lhs) finline -> Type* {
        if (!(lhs->type->is_integer() || lhs->type->is_bool())) {
            error_cannot_apply(lhs);
            return nullptr;
        }
        if (!(rhs->type->is_integer() || rhs->type->is_bool())) {
            error_cannot_apply(rhs);
            return nullptr;
        }
        if (Type* result_type = get_integer_type(enforce_lhs)) {
            return result_type;
        }

        error_mismatched();
        return nullptr;
    };

    auto get_shifts_type = [=, this](bool enforce_lhs) finline -> Type* {
        if (!lhs->type->is_integer()) {
            error_cannot_apply(lhs);
            return nullptr;
        }
        if (!rhs->type->is_integer()) {
            error_cannot_apply(rhs);
            return nullptr;
        }
        if (Type* result_type = get_integer_type(enforce_lhs)) {
            return result_type;
        }
        
        error_mismatched();
        return nullptr;
    };

    check_and_verify_type(lhs);
    check_and_verify_type(rhs);

    if (!lhs->is_foldable || !rhs->is_foldable) {
        bin_op->is_foldable = false;
    }

    switch (bin_op->op) {
    case '=':
    case Token::AddEq:
    case Token::SubEq:
    case Token::MulEq:
    case Token::DivEq:
    case Token::ModEq:
    case Token::AndEq:
    case Token::OrEq:
    case Token::CaretEq:
    case Token::TildeEq:
    case Token::LtLtEq:
    case Token::GtGtEq: {
        
        check_modifiable(bin_op->lhs);

        switch (bin_op->op) {
        case Token::AddEq: case Token::SubEq: case Token::MulEq: {
            if (!get_add_sub_mul_type(true)) return;
            break;
        }
        case Token::DivEq: case Token::ModEq: {
            if (!get_div_mod_type(true)) return;
            break;
        }
        case Token::AndEq: case Token::OrEq: case Token::CaretEq: {
            if (!get_logical_bitwise_type(true)) return;
            break;
        }
        case Token::TildeEq: {
            if (!lhs->type->is_integer()) {
                error_cannot_apply(lhs);
                return;
            }
            if (!rhs->type->is_integer()) {
                error_cannot_apply(rhs);
                return;
            }
            if (!get_integer_type(true)) {
                error_mismatched();
                return;
            }
            break;
        case Token::LtLtEq: case Token::GtGtEq: {
            if (!get_shifts_type(true)) return;
            break;
        }
        }
        }

        if (!bin_op->lhs->type->is_pointer()) {
            create_cast(bin_op->rhs, bin_op->lhs->type);
        }
        bin_op->type = bin_op->lhs->type;
        break;
    }
    case '+': case '-': case '*': {
        auto result_type = get_add_sub_mul_type(true);
        if (!result_type) return;

        // Create needed casts if not pointer arithmetic.
        if (!lhs->type->is_pointer() && !rhs->type->is_pointer() &&
            !lhs->type->is_array() && !rhs->type->is_array()) {
            create_cast(lhs, result_type);
            create_cast(rhs, result_type);
        }

        bin_op->type = result_type;

        break;
    }
    case '/': case '%': {
        auto result_type = get_div_mod_type(true);
        if (!result_type) return;
        bin_op->type = result_type;
        create_cast(lhs, result_type);
        create_cast(rhs, result_type);
        break;
    }
    case '^': case '&': case '|': {
        auto result_type = get_logical_bitwise_type(true);
        if (!result_type) return;
        bin_op->type = result_type;
        create_cast(lhs, result_type);
        create_cast(rhs, result_type);
        break;
    }
    case Token::LtLt: case Token::GtGt: {
        auto result_type = get_shifts_type(true);
        if (!result_type) return;
        bin_op->type = result_type;
        create_cast(lhs, result_type);
        create_cast(rhs, result_type);
        break;
    }
    case '<': case '>':
    case Token::GtEq: case Token::LtEq:
    case Token::EqEq: case Token::ExEq: {
        if (!lhs->type->is_comparable()) {
            error_cannot_apply(lhs);
            return;
        }
        if (!rhs->type->is_comparable()) {
            error_cannot_apply(rhs);
            return;
        }

        auto result_type = get_integer_type(false);
        if (!(result_type ||
             (rhs->type->is_pointer() && lhs->type->get_kind() == TypeKind::Null) ||
             (lhs->type->is_pointer() && rhs->type->get_kind() == TypeKind::Null)
              )) {
            error_mismatched();
            return;
        } else if (result_type) {
            create_cast(lhs, result_type);
            create_cast(rhs, result_type);
        }
        bin_op->type = context.bool_type;
        break;
    }
    case Token::AndAnd: case Token::OrOr: {
        if (!is_condition(lhs->type)) {
            error_cannot_apply(lhs);
            return;
        }
        if (!is_condition(rhs->type)) {
            error_cannot_apply(rhs);
            return;
        }
        
        bin_op->type = context.bool_type;
        break;
    }
    case Token::RangeEq: case Token::RangeLt: {
        if (!lhs->type->is_integer()) {
            error_cannot_apply(lhs);
            return;
        }
        if (!rhs->type->is_integer()) {
            error_cannot_apply(rhs);
            return;
        }
        
        if (!lhs->type->is_ignore_const(rhs->type)) {
            error_mismatched();
            return;
        }

        bin_op->type = type_table.get_range_type(lhs->type);
        break;
    }
    default:
        acorn_fatal("check_binary_op(): Failed to implement case");
        break;
    }
}

void acorn::Sema::check_unary_op(UnaryOp* unary_op) {
    Expr* expr = unary_op->expr;
    check_and_verify_type(expr);

    auto error_no_applies = [this, unary_op, expr]() finline -> void {
        error(expand(unary_op), "Operator %s cannot apply to type '%s'",
              token_kind_to_string(context, unary_op->op), expr->type)
            .end_error(ErrCode::SemaUnaryOpTypeCannotApply);
    };

    switch (unary_op->op) {
    case '-': case '+': {
        if (!expr->type->is_number()) {
            error_no_applies();
            return;
        }
        unary_op->type = expr->type;
        unary_op->is_foldable = expr->is_foldable;
        break;
    }
    case '~': {
        if (!expr->type->is_integer()) {
            error_no_applies();
            return;
        }
        unary_op->type = expr->type;
        unary_op->is_foldable = expr->is_foldable;
        break;
    }
    case '!': {
        if (!is_condition(expr->type)) {
            error_no_applies();
            return;
        }
        unary_op->type = context.bool_type;
        unary_op->is_foldable = expr->is_foldable;
        break;
    }
    case '&': {
        if (!is_lvalue(expr)) {
            error(expand(unary_op), "Operator & expects the value to have an address")
                .end_error(ErrCode::SemaUnaryOpAddressAccessNotLvalue);
            return;
        }
        unary_op->type = type_table.get_ptr_type(expr->type);
        unary_op->is_foldable = false;
        break;
    }
    case '*': {
        if (!expr->type->is_pointer()) {
            error_no_applies();
            return;
        }

        if (expr->is(NodeKind::This)) {
            error(expand(unary_op), "Cannot dereference 'this' ptr")
                .end_error(ErrCode::SemaCannotDereferenceThisPtr);
            return;
        }

        unary_op->type = static_cast<PointerType*>(expr->type)->get_elm_type();
        unary_op->is_foldable = false;
        break;
    }
    case Token::AddAdd: case Token::SubSub:
    case Token::PostAddAdd: case Token::PostSubSub: {
        if (!is_lvalue(expr)) {
            error(expand(unary_op), "Operator %s expects the value to have an address",
                  token_kind_to_string(context, unary_op->op))
                .end_error(ErrCode::SemaUnaryOpIncDecNotLValue);
            return;
        }
        if (!(expr->type->is_integer() || expr->type->is_pointer())) {
            error_no_applies();
            return;
        }
        unary_op->type = expr->type;
        unary_op->is_foldable = false;
        break;
    }
    default:
        acorn_fatal("check_unary_op(): unimplemented case");
        break;
    }
}

void acorn::Sema::check_ident_ref(IdentRef* ref, Namespace* search_nspace, bool is_for_call) {
    
    if (is_comptime_if_cond) {
        if (is_for_call) {
            error(expand(ref), "Cannot make calls in comptime if conditions")
                .end_error(ErrCode::SemaCannotMakeCallsInComptimeIf);
            return;
        }
        
        if (auto* universal = context.get_universal_constant(ref->ident)) {
            ref->set_universal_ref(universal);
            ref->type = ref->universal_ref->type;
            return;
        }
        
        error(expand(ref), "Could not find identifier '%s'", ref->ident)
            .end_error(ErrCode::SemaNoFindIdentRef);
        return;
    }

    bool same_nspace = search_nspace == nspace;

    auto find_function = [=, this]() finline {
        if (same_nspace) {
            if (cur_struct) {
                if (auto* funcs = cur_struct->nspace->find_functions(ref->ident)) {
                    ref->set_funcs_ref(funcs);
                    return;
                }
            }

            if (auto* funcs = file->find_functions(ref->ident)) {
                ref->set_funcs_ref(funcs);
                return;
            }

            if (auto* funcs = file->find_static_import_functions(ref->ident)) {
                ref->set_funcs_ref(funcs);
                return;
            }
        }

        if (auto* funcs = search_nspace->find_functions(ref->ident)) {
            ref->set_funcs_ref(funcs);
        }
    };

    auto find_variable = [=, this]() finline {
        if (same_nspace) {
            if (cur_scope) {
                if (auto* var = cur_scope->find_variable(ref->ident)) {
                    ref->set_var_ref(var);
                    return;
                }
            }

            if (cur_struct) {
                if (auto* var = cur_struct->nspace->find_variable(ref->ident)) {
                    ref->set_var_ref(var);
                    return;
                }
            }

            if (auto* var = file->find_variable(ref->ident)) {
                ref->set_var_ref(var);
                return;
            }

            if (auto* var = file->find_static_import_variable(ref->ident)) {
                ref->set_var_ref(var);
                return;
            }
        }

        if (auto* var = search_nspace->find_variable(ref->ident)) {
            ref->set_var_ref(var);
            return;
        }
        if (auto* universal = context.get_universal_constant(ref->ident)) {
            ref->set_universal_ref(universal);
        }
    };

    if (is_for_call) {
        find_function();
        if (!ref->found_ref()) {
            find_variable();
        }
    } else {
        find_variable();
        if (!ref->found_ref()) {
            find_function();
        }
    }

    // If still not found let us try and search for an imported module.
    if (!ref->found_ref() && same_nspace) {
        if (auto importn = file->find_import(ref->ident)) {
            ref->set_import_ref(importn);
        }
    }
    
    switch (ref->found_kind) {
    case IdentRef::VarKind: {
        
        Var* var_ref = ref->var_ref;
        if (var_ref->is_global) {
            ensure_global_variable_checked(ref->loc, ref->var_ref);
        }

        if (!var_ref->type) {
            break;
        }
        
        ref->is_foldable = var_ref->is_foldable;
        ref->type = var_ref->type;
        break;
    }
    case IdentRef::FuncsKind: {
        ref->type = context.funcs_ref_type;
        break;
    }
    case IdentRef::UniversalKind: {
        ref->type = ref->universal_ref->type;
        break;
    }
    case IdentRef::ImportKind : {
        if (ref->import_ref->is_static) {
            error(expand(ref), "Cannot reference a static import")
                .end_error(ErrCode::SemaCannotRefStaticImport);
        } else {
            ref->type = context.namespace_ref_type;
        }
        break;
    }
    case IdentRef::NoneKind: {
        error(expand(ref), "Could not find %s '%s'", is_for_call ? "function" : "identifier", ref->ident)
            .end_error(!is_for_call ? ErrCode::SemaNoFindIdentRef : ErrCode::SemaNoFindFuncIdentRef);
        break;
    }
    }
}

void acorn::Sema::check_dot_operator(DotOperator* dot, bool is_for_call) {
    if (dot->site->is(NodeKind::IdentRef)) {
        IdentRef* site = static_cast<IdentRef*>(dot->site);
        check_ident_ref(site, nspace, false);
        dot->is_foldable = site->is_foldable;

        yield_if(dot->site);

        if (site->type == context.namespace_ref_type) {
            auto importn = site->import_ref;
            // Special case in which we search in a given module.
            check_ident_ref(dot, importn->imported_nspace, is_for_call);
            return;
        } else if (!site->type) {
            return;
        }
    } else {
        check_and_verify_type(dot->site);
        dot->is_foldable = dot->site->is_foldable;
    }

    auto report_error_cannot_access_field = [this, dot]() finline {
        error(expand(dot), "Cannot access field '%s' of type '%s'", dot->ident, dot->site->type)
            .end_error(ErrCode::SemaDotOperatorCannotAccessType);
    };

    auto check_struct_ident_ref = [this, dot, is_for_call](StructType* struct_type) finline {
        auto structn = struct_type->get_struct();
        check_ident_ref(dot, structn->nspace, is_for_call);

        if (!dot->type) {
            return;
        }

        if (dot->site->type->is_const() && !dot->type->is_const()) {
            // The constness must be passed onto the field to prevent modification of fields.
            dot->type = type_table.get_const_type(dot->type);
        }
    };

    if (dot->ident == context.length_identifier && dot->site->type->is_array()) {
        dot->is_array_length = true;
        dot->type = context.int_type;
    } else if (dot->site->type->is_struct_type()) {
        auto struct_type = static_cast<StructType*>(dot->site->type);
        check_struct_ident_ref(struct_type);
    } else if (dot->site->type->is_pointer()) {
        auto ptr_type = static_cast<PointerType*>(dot->site->type);
        auto elm_type = ptr_type->get_elm_type();
        if (elm_type->is_struct_type()) {
            auto struct_type = static_cast<StructType*>(elm_type);
            check_struct_ident_ref(struct_type);
        } else {
            report_error_cannot_access_field();
        }
    } else {
        report_error_cannot_access_field();
    }
}

void acorn::Sema::check_function_call(FuncCall* call) {
    
    bool args_have_errors = false;
    for (Expr* arg : call->args) {
        check_node(arg);
        if (!arg->type) args_have_errors = true;
    }
    if (args_have_errors) return;

    bool is_funcs_ref = true;
    if (call->site->is(NodeKind::IdentRef)) {
        
        IdentRef* ref = static_cast<IdentRef*>(call->site);
        check_ident_ref(ref, nspace, true);
        yield_if(ref);

        if (ref->found_kind != IdentRef::FuncsKind) {
            is_funcs_ref = false;
        }
    } else if (call->site->is(NodeKind::DotOperator)) {
        // This is very similar to check to IdentRef but it is too much
        // small differences to abstract in any meaningfull way as far
        // as I can tell.

        DotOperator* ref = static_cast<DotOperator*>(call->site);
        check_dot_operator(ref, true);
        yield_if(ref);

        if (ref->found_kind != IdentRef::FuncsKind) {
            is_funcs_ref = false;
        }
    } else {
        check_and_verify_type(call->site);
        
        is_funcs_ref = false;
    }

    if (call->site->type->is_function_type()) {
        check_function_type_call(call, static_cast<FunctionType*>(call->site->type));
        return;
    }
    
    if (!is_funcs_ref) {
        error(expand(call->site), "Type '%s' is not callable", call->site->type)
            .end_error(ErrCode::SemaTypeNoCallable);
        return;
    }

    IdentRef* ref = static_cast<IdentRef*>(call->site);
    auto called_func = check_function_decl_call(call, 
                                                call->args, 
                                                call->non_named_args_offset, 
                                                *ref->funcs_ref);
    if (!called_func) {
        return;
    }

    call->is_foldable = false;
    call->called_func = called_func;
    call->type = called_func->return_type;
    context.queue_gen(call->called_func);

}

void acorn::Sema::check_function_type_call(FuncCall* call, FunctionType* func_type) {
    auto& param_types = func_type->get_param_types();

    auto display_error = [this, call, func_type]() finline {
        logger.begin_error(expand(call), "Invalid call to function type: %s", func_type);
        logger.add_empty_line();
        display_call_mismatch_info(func_type, call->args, false);
        logger.end_error(ErrCode::SemaInvalidFuncCallSingle);
    };

    if (call->args.size() != param_types.size()) {
        display_error();
        return;
    }

    for (size_t i = 0; i < call->args.size(); i++) {
        Expr* arg        = call->args[i];
        Type* param_type = param_types[i];

        if (arg->is(NodeKind::NamedValue)) {
            display_error();
            return;
        }

        if (!is_assignable_to(param_type, arg)) {
            display_error();
            return;
        }

        create_cast(arg, param_type);
    }

    call->is_foldable = false;
    call->type = func_type->get_return_type();
}

acorn::Func* acorn::Sema::check_function_decl_call(Expr* call_node,
                                                   llvm::SmallVector<Expr*>& args,
                                                   size_t non_named_args_offset,
                                                   FuncList& candidates) {

    for (Func* canidate : candidates) {
        if (canidate->is_checking_declaration) {
            logger.begin_error(call_node->loc,
                               "Circular dependency while checking function declaration '%s'",
                               canidate->name);
            
            const char* func_end_ptr = canidate->loc.ptr;
            if (call_node->is(NodeKind::FuncCall)) {
                go_until(func_end_ptr, '(', ')');
            } else {
                go_until(func_end_ptr, '{', '}');
            }
            
            bool within_func_params = call_node->loc.ptr >= canidate->loc.ptr && call_node->loc.ptr <= func_end_ptr;
            if (!within_func_params) {
                logger.add_line([canidate](Logger& logger) {
                    logger.print("Function declared at: ");
                    canidate->show_location_msg(logger);
                });
            }
            logger.end_error(ErrCode::SemaCircularFuncDeclDependency);
            return nullptr;
        }
        if (!canidate->has_checked_declaration) {
            if (!check_function_decl(canidate)) {
                return nullptr;
            }
        }
    }

    // Check for duplicated named arguments.
    if (non_named_args_offset != -1) {
        bool dup_named_args = false;
        for (size_t i = non_named_args_offset; i < args.size(); i++) {
            auto arg = args[i];
            if (!arg->is(NodeKind::NamedValue)) {
                continue;
            }

            auto named_arg = static_cast<NamedValue*>(arg);
            for (size_t j = i + 1; j < args.size(); j++) {
                auto other_arg = args[j];
                if (!other_arg->is(NodeKind::NamedValue)) {
                    continue;
                }
                
                auto other_named_arg = static_cast<NamedValue*>(other_arg);
                if (named_arg->name == other_named_arg->name) {
                    error(other_named_arg, "Duplicated named argument '%s'", named_arg->name)
                        .end_error(ErrCode::SemaDuplicatedNamedCallArg);
                    dup_named_args = true;
                    break;
                }
            }
        }
        if (dup_named_args) {
            return nullptr;
        }
    }

    auto called_func = find_best_call_candidate(candidates, args);
    if (!called_func) {
        display_call_mismatch_info(expand(call_node), candidates, args);
        return nullptr;
    }

    // Creating casts from the argument to the parameter.
    for (size_t i = 0; i < args.size(); i++) {
        auto arg_value = args[i];
        Var* param;
        if (arg_value->is(NodeKind::NamedValue)) {
            auto named_arg = static_cast<NamedValue*>(arg_value);
            param = called_func->find_parameter(named_arg->name);
            named_arg->mapped_idx = param->param_idx;
            
            arg_value = named_arg->assignment;
        } else {
            param = called_func->params[i];
        }
        create_cast(arg_value, param->type);
    }

    return called_func;
}

uint32_t acorn::Sema::get_function_call_score(const Func* candidate, const llvm::SmallVector<Expr*>& args) const {
    // We just want to define enough to definitely place the next score
    // value above any of the mismatched types.
    const uint32_t MAX_ARGS_SCORE_CAP          = MAX_FUNC_PARAMS * 4;
    const uint32_t MAX_NOT_ASSIGNABLE_CAP      = MAX_ARGS_SCORE_CAP *2;

    const uint32_t INCORRECT_PARAM_NAME_OR_ORD = MAX_NOT_ASSIGNABLE_CAP * 2;
    const uint32_t INCORRECT_NUM_ARGS_CAP      = MAX_NOT_ASSIGNABLE_CAP * 2;

    uint32_t score = 0;
    
    uint32_t mimatched_types = 0;
    uint32_t not_assignable_types = 0;
    auto status = compare_as_call_candidate<true>(candidate, args, mimatched_types, not_assignable_types);
    switch (status) {
    case CallCompareStatus::INCORRECT_ARGS:
        return INCORRECT_NUM_ARGS_CAP;
    case CallCompareStatus::INCORRECT_PARAM_BY_NAME_NOT_FOUND:
    case CallCompareStatus::OUT_OF_ORDER_PARAMS:
        return INCORRECT_PARAM_NAME_OR_ORD;
    default:
        break;
    }
    score += mimatched_types;
    if (not_assignable_types != 0) {
        score += MAX_ARGS_SCORE_CAP;
        score += not_assignable_types;
    }
    
    return score;
}

acorn::Func* acorn::Sema::find_best_call_candidate(FuncList& candidates,
                                                   llvm::SmallVector<Expr*>& args) {
    
    Func* selected = nullptr;
    uint32_t best_mimatched_types = 0;


    // TODO: select if has fewer default values?

    auto select = [&selected, &best_mimatched_types](Func* canidate,
                                                     uint32_t mimatched_types) finline{
        selected = canidate;
        best_mimatched_types = mimatched_types;
    };
    for (Func* canidate : candidates) {

        uint32_t mimatched_types = 0;
        uint32_t not_assignable_types = 0;
        auto status = compare_as_call_candidate<false>(canidate, args, mimatched_types, not_assignable_types);
        if (status != CallCompareStatus::SUCCESS) {
            continue;
        }

        if (!selected) {
            select(canidate, mimatched_types);
            continue;
        }

        if (best_mimatched_types > mimatched_types) {
            select(canidate, mimatched_types);
        }
    }
    return selected;
}

template<bool for_score_gathering>
acorn::Sema::CallCompareStatus acorn::Sema::compare_as_call_candidate(const Func* candidate,
                                                                      const llvm::SmallVector<Expr*>& args,
                                                                      uint32_t& mimatched_types,
                                                                      uint32_t& not_assignable_types) const {
    if (!has_correct_number_of_args(candidate, args)) {
        return CallCompareStatus::INCORRECT_ARGS;
    }

    bool named_args_out_of_order = false;
    uint32_t named_arg_high_idx = 0;
    for (size_t i = 0; i < args.size(); i++) {
        Expr* arg_value = args[i];
        Var* param;

        if (arg_value->is(NodeKind::NamedValue)) {
            // Handle named arguments by finding the corresponding parameter

            auto named_arg = static_cast<NamedValue*>(arg_value);
            arg_value = named_arg->assignment;
            param = candidate->find_parameter(named_arg->name);
            
            // Check to make sure we found the parameter by the given name.
            if (!param) {
                return CallCompareStatus::INCORRECT_PARAM_BY_NAME_NOT_FOUND;
            }

            if (param->param_idx != i) {
                named_args_out_of_order = true;
            }
            named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
        } else {
            param = candidate->params[i];

            // Cannot determine the order of the arguments if the
            // non-named arguments come after the named arguments
            // and the named arguments are not in order.
            if (named_args_out_of_order || named_arg_high_idx > i) {
                return CallCompareStatus::OUT_OF_ORDER_PARAMS;
            }
        }
        
        if (!is_assignable_to(param->type, arg_value)) {
            if constexpr (for_score_gathering) {
                ++not_assignable_types;
                continue;
            } else {
                return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
            }
        }

        if (param->type->is_not(arg_value->type)) {
            ++mimatched_types;
        }
    }

    return CallCompareStatus::SUCCESS;
}

bool acorn::Sema::has_correct_number_of_args(const Func* candidate, const 
                                             llvm::SmallVector<Expr*>& args) const {
    if (candidate->default_params_offset == -1) {
        if (candidate->params.size() != args.size()) {
            return false;
        }
    } else {
        size_t num_params = candidate->params.size();
        if (!(args.size() >= candidate->default_params_offset
             && args.size() <= num_params)) {
            return false;
        }
    }
    return true;
}

void acorn::Sema::display_call_mismatch_info(PointSourceLoc error_loc, 
                                             const FuncList& candidates, 
                                             const llvm::SmallVector<Expr*>& args) const {
    
    auto function_decl_to_string = [](const Func* canidate) {
        std::string str = canidate->name.reduce().str();
        str += "(";
        size_t count = 0;
        for (Var* param : canidate->params) {
            str += param->type->to_string();
            if (count + 1 != canidate->params.size()) {
                str += ", ";
            }
            ++count;
        }
        str += ")";
        return str;
    };

    const char* func_type_str = candidates[0]->is_constructor ? "constructor" : "function";

    if (candidates.size() == 1) {
        
        Func* canidate = candidates[0];

        logger.begin_error(error_loc, "Invalid call to %s: %s", func_type_str, function_decl_to_string(canidate));
        logger.add_empty_line();
        display_call_mismatch_info(canidate, args, false);
        logger.end_error(ErrCode::SemaInvalidFuncCallSingle);

    } else {

        llvm::SmallVector<std::pair<uint32_t, const Func*>> candidates_and_scores;
        for (const Func* candidate : candidates) {
            uint32_t score = get_function_call_score(candidate, args);
            candidates_and_scores.push_back(std::make_pair(score, candidate));
        }
        std::ranges::sort(candidates_and_scores, [](const auto& lhs, const auto& rhs) {
            return lhs.first < rhs.first;
        });

        logger.begin_error(error_loc, "Could not find a valid overloaded %s to call", func_type_str);
        for (auto [_, candidate] : candidates_and_scores | std::views::take(context.get_max_call_err_funcs())) {
            logger.add_empty_line();
            logger.add_line("Could not match: %s", function_decl_to_string(candidate));
            display_call_mismatch_info(candidate, args, true);
        }
        int64_t remaining = static_cast<int64_t>(candidates.size()) - static_cast<int64_t>(context.get_max_call_err_funcs());
        if (remaining > 0) {
            logger.add_empty_line()
                  .add_line("... and %s more not shown", remaining);
        }
        logger.end_error(ErrCode::SemaInvalidFuncCallOverloaded);

    }
}

template<typename F>
void acorn::Sema::display_call_mismatch_info(const F* candidate,
                                             const llvm::SmallVector<Expr*>& args,
                                             bool indent) const {

#define err_line(fmt, ...) logger.add_line(("%s- " fmt), indent ? "  " : "", ##__VA_ARGS__)

    constexpr bool is_func_decl = std::is_same_v<std::remove_const_t<F>, Func>;
        
    auto get_num_params = [candidate]() constexpr -> size_t {
        if constexpr (is_func_decl) {
            return candidate->params.size();
        } else {
            return candidate->get_param_types().size();
        }
    };

    size_t num_params = get_num_params();

    auto check_correct_number_of_args = [this, candidate, args, num_params]() finline {
        if constexpr (is_func_decl) {
            return has_correct_number_of_args(candidate, args);
        } else {
            return num_params == args.size();
        }  
    };

    if (!check_correct_number_of_args()) {
        if constexpr (is_func_decl) {
            if (candidate->default_params_offset != -1) {
                size_t min_params = candidate->default_params_offset;
                err_line("Incorrect number of args. Expected between %s-%s but found %s",
                         min_params, num_params, args.size());
                return;
            }
        }

        err_line("Incorrect number of args. Expected %s but found %s",
                 num_params, args.size());
        return;
    }

    auto get_param_type = [candidate](Var* param, size_t arg_idx) constexpr -> Type* {
        if constexpr (is_func_decl) {
            return param->type;
        } else {
            return candidate->get_param_types()[arg_idx];
        }
    };

    bool named_args_out_of_order = false;
    uint32_t named_arg_high_idx = 0;
    for (size_t i = 0; i < args.size(); i++) {
        
        Expr* arg_value = args[i];
        Var* param = nullptr;

        if (arg_value->is(NodeKind::NamedValue)) {
            
            if constexpr (is_func_decl) {
                auto named_arg = static_cast<NamedValue*>(arg_value);
                arg_value = named_arg->assignment;
                param = candidate->find_parameter(named_arg->name);
            
                if (!param) {
                    err_line("Could not find param '%s' for named arg", named_arg->name);
                    return;
                }

                named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
                if (param->param_idx != i) {
                    named_args_out_of_order = true;
                }
            } else {
                err_line("Cannot use named arguments when calling based on type");
                return;
            }
        } else {
            if constexpr (is_func_decl) {
                param = candidate->params[i];

                if (named_args_out_of_order || named_arg_high_idx > i) {
                    // Do not continue reporting errors because the arguments
                    // being disordered would mean the error messages would not
                    // make any sense.
                    err_line("Arg %s causes the arguments to be out of order", i + 1);
                    logger.add_line("  Order named arguments or place before named arguments");
                    return;
                }
            }
        }

        auto param_type = get_param_type(param, i);
        if (!is_assignable_to(param_type, arg_value)) {
            err_line("Wrong type for arg %s. Expected '%s' but found '%s'",
                     i + 1, param_type, arg_value->type);
        }
    }

#undef err_line
}

void acorn::Sema::check_cast(Cast* cast) {
    auto fixed_cast_type = fixup_type(cast->explicit_cast_type);
    if (!fixed_cast_type) {
        return;
    }

    cast->type = fixed_cast_type;

    check_and_verify_type(cast->value);
    if (!is_castable_to(cast->explicit_cast_type, cast->value)) {
        error(expand(cast), "Cannot cast from '%s' to '%s'",
              cast->value->type, cast->explicit_cast_type)
            .end_error(ErrCode::SemaInvalidCast);
    }
}

void acorn::Sema::check_named_value(NamedValue* named_value) {
    check_node(named_value->assignment);
    named_value->type = named_value->assignment->type;
    named_value->is_foldable = named_value->assignment->is_foldable;
}

void acorn::Sema::check_array(Array* arr, Type* dest_elm_type) {
    if (arr->elms.empty()) {
        arr->type = context.empty_array_type;
        return;
    }

    Type* elm_type = nullptr;
    Expr* value_for_elm_type;
    bool values_have_errors = false;
    for (Expr* elm : arr->elms) {
        if (!elm) continue;

        if (elm->is(NodeKind::Array) && dest_elm_type && dest_elm_type->is_array()) {
            auto next_dest_elm_type = static_cast<ArrayType*>(dest_elm_type)->get_elm_type();
            check_array(static_cast<Array*>(elm), next_dest_elm_type);
        } else {
            check_node(elm);
        }

        if (!elm->type) {
            values_have_errors = true;
            continue;
        }

        if (!elm->is_foldable) {
            arr->is_foldable = false;
        }

        if (dest_elm_type) {
            if (!is_assignable_to(dest_elm_type, elm)) {
                error(expand(elm), "Expected element type '%s' but found '%s'",
                      dest_elm_type, elm->type)
                    .end_error(ErrCode::SemaIncompatibleArrayElmTypes);
            }
        } else if (!elm_type) {
            elm_type = elm->type;
            value_for_elm_type = elm;
        } else if (elm_type->is_not(elm->type)) {
            if (!is_assignable_to(elm_type, elm)) {
                // Check the reverse case.
                if (!is_assignable_to(elm->type, value_for_elm_type)) {
                    error(expand(elm), "Incompatible element types. First found '%s' but now '%s'",
                          elm_type, elm->type)
                        .end_error(ErrCode::SemaIncompatibleArrayElmTypes);
                } else {
                    elm_type = elm->type;
                    value_for_elm_type = elm;
                }
            }
        }
    }

    if (values_have_errors) {
        return;
    }

    if (!elm_type) {
        elm_type = dest_elm_type;
    }
    
    for (Expr* value : arr->elms) {
        if (value) {
            create_cast(value, elm_type);
        }
    }

    arr->type = type_table.get_arr_type(elm_type, arr->elms.size());
}

void acorn::Sema::check_memory_access(MemoryAccess* mem_access) {
    check_and_verify_type(mem_access->site);

    mem_access->is_foldable = false;

    Type* access_type = mem_access->site->type;
    if (!(access_type->is_array() || access_type->is_pointer())) {
        error(mem_access, "Cannot index memory of type '%s'", access_type)
            .end_error(ErrCode::SemaMemoryAccessBadType);
    } else {
        auto ctr_type = static_cast<ContainerType*>(access_type);
        mem_access->type = ctr_type->get_elm_type();
    }

    check_and_verify_type(mem_access->index);

    if (!mem_access->index->type->is_integer()) {
        error(expand(mem_access->index), "Expected index of memory access to be an integer")
            .end_error(ErrCode::SemaMemoryIndexNotInteger);
    } else {
        create_cast(mem_access->index, context.usize_type);
    }
}

void acorn::Sema::ensure_global_variable_checked(SourceLoc error_loc, Var* var) {
    // TODO: Does this need to just check if var->is_being_checked is true? It may be possible
    // that it somehow circularly depends on itself through a different means.
    if (cur_global_var) {
        cur_global_var->dependency = var;
        
        if (var->is_being_checked) {
            display_circular_dep_error(error_loc, 
                                       cur_global_var,
                                       "Global variables form a circular dependency", 
                                       ErrCode::SemaGlobalCircularDependency);
        }
    }
    
    if (!var->has_been_checked) {
        Sema sema(context, var->file, var->get_logger());
        sema.check_variable(var);
    }
}

bool acorn::Sema::ensure_struct_checked(SourceLoc error_loc, Struct* structn) {
    if (structn->is_being_checked) {
        logger.begin_error(error_loc,
                           "Circular dependency while checking struct declaration '%s'",
                           structn->name);
        logger.add_line([structn](Logger& logger) {
            logger.print("Struct declared at: ");
            structn->show_location_msg(logger);
        });
        logger.end_error(ErrCode::SemaCircularStructDeclDependency);
        return false;
    }
    if (!structn->has_been_checked) {
        Sema sema(context, structn->file, structn->get_logger());
        sema.check_struct(structn);
    }
    return true;
}

// Utility functions
//--------------------------------------

bool acorn::Sema::is_assignable_to(Type* to_type, Expr* expr) const {

    Type* from_type = expr->type;
    if (!try_remove_const_for_compare(to_type, from_type, expr)) {
        return false;
    }

    switch (to_type->get_kind()) {
    case TypeKind::Int:
    case TypeKind::Int8: case TypeKind::UInt8:
    case TypeKind::Int16: case TypeKind::UInt16:
    case TypeKind::Int32: case TypeKind::UInt32:
    case TypeKind::Int64: case TypeKind::UInt64:
    case TypeKind::USize: case TypeKind::ISize:
    case TypeKind::Char: case TypeKind::Char16:
    case TypeKind::Char32: {
        if (to_type->is(from_type)) {
            return true;
        }

        if (expr->is_foldable && expr->is(NodeKind::Number)) {

            Number* number = static_cast<Number*>(expr);

            // TODO: We want to allow for certain floats to be assignable such
            //       as 1e+9 because those are even numbers.
            if (from_type->is_float()) {
                return false;
            }

            auto does_fit_range = [to_type]<typename T>(T value) finline {
                switch (to_type->get_kind()) {
                // Signed cases
                case TypeKind::Int8:  return fits_in_range<int8_t>(value);
                case TypeKind::Int16: return fits_in_range<int16_t>(value);
                case TypeKind::Int:
                case TypeKind::Int32:
                case TypeKind::ISize:
                    return fits_in_range<int32_t>(value);
                case TypeKind::Int64: return fits_in_range<int64_t>(value);
                // Unsigned cases
                case TypeKind::UInt8: case TypeKind::Char:
                    return fits_in_range<uint8_t>(value);
                case TypeKind::UInt16: case TypeKind::Char16:
                    return fits_in_range<uint16_t>(value);
                case TypeKind::UInt32: case TypeKind::USize: case TypeKind::Char32:
                    return fits_in_range<uint32_t>(value);
                case TypeKind::UInt64: return fits_in_range<uint64_t>(value);

                default:
                    acorn_fatal("unreachable signed integer type");
                    return false;
                }
            };

            if (from_type->is_signed()) {
                return does_fit_range(number->value_s64);
            } else {
                return does_fit_range(number->value_u64);
            }
        }

        return to_type->is(from_type);
    }
    case TypeKind::Float32: case TypeKind::Float64: {
        if (expr->is(NodeKind::Number) && from_type == context.int_type) {
            return true;
        }
        return to_type->is(from_type);
    }
    case TypeKind::Pointer: {
        if (expr->is(NodeKind::String)) {
            if (expr->type->is(context.const_char_ptr_type) &&
                (to_type->is(context.const_char16_ptr_type) || to_type->is(context.const_char32_ptr_type))) {
                return true;
            } else if (expr->type->is(context.const_char16_ptr_type) &&
                       to_type->is(context.const_char32_ptr_type)) {
                return true;
            }

            return to_type->is(from_type) || to_type->is(context.void_ptr_type);
        } else if (from_type->is_array()) {
            auto to_arr_Type = static_cast<ArrayType*>(to_type);
            auto from_ptr_type = static_cast<PointerType*>(from_type);

            auto to_elm_type = to_arr_Type->get_elm_type();
            auto from_elm_type = from_ptr_type->get_elm_type();

            return to_elm_type->is(from_elm_type) || to_elm_type->is(context.void_type);
        } else if (from_type->is_pointer()) {
            return to_type->is(from_type) || to_type->is(context.void_ptr_type);
        } else if (expr->is(NodeKind::Null)) {
            return true;
        } else {
            return false;
        }
    }
    case TypeKind::Array: {
        if (from_type->get_kind() == TypeKind::EmptyArray) {
            return true;
        } else if (expr->is(NodeKind::Array)) {
            auto to_arr_type = static_cast<ArrayType*>(to_type);
            auto from_arr_type = static_cast<ArrayType*>(from_type);

            while (to_arr_type->get_elm_type()->is_array()) {
                if (!from_arr_type->get_elm_type()->is_array()) {
                    // Not the same dimensionality.
                    return false;
                }

                // Allow for zero filling the remainder.
                if (from_arr_type->get_length() > to_arr_type->get_length()) {
                    return false;
                }

                to_arr_type = static_cast<ArrayType*>(to_arr_type->get_elm_type());
                from_arr_type = static_cast<ArrayType*>(from_arr_type->get_elm_type());
            }
            
            // Allow for zero filling the remainder.
            if (from_arr_type->get_length() > to_arr_type->get_length()) {
                return false;
            }

            if (to_arr_type->get_elm_type()->is_not(from_arr_type->get_elm_type())) {
                return false;
            }
            return true;
        }
        return to_type->is(from_type);
    }
    case TypeKind::Function: {
        if (from_type->get_kind() == TypeKind::FuncsRef) {
            auto to_func_type = static_cast<FunctionType*>(to_type);
            
            // TODO: in the future we want to allow selection of the
            //       specific function they ask for.
            auto ref = static_cast<IdentRef*>(expr);
            auto func = (*ref->funcs_ref)[0];
            if (func->return_type->is_not(to_func_type->get_return_type())) {
                return false;
            }
            auto& param_types = to_func_type->get_param_types();
            if (func->params.size() != param_types.size()) {
                return false;
            }
            for (size_t i = 0; i < param_types.size(); i++) {
                if (param_types[i]->is_not(func->params[i]->type)) {
                    return false;
                }
            }

            context.queue_gen(func);
            return true;
        }

        return to_type->is(from_type);
    }
    default:
        return to_type->is(from_type);
    }
}

bool acorn::Sema::is_castable_to(Type* to_type, Expr* expr) const {
    if ((to_type->is_real_pointer() || to_type->is_integer() || to_type->is_bool()) &&
        (expr->type->is_real_pointer() || expr->type->is_integer() || expr->type->is_bool())) {
        Type* from_type = expr->type;
        if (!try_remove_const_for_compare(to_type, from_type, expr)) {
            return false;
        }
        // Pointers and numbers can cast to each other.
        return true;
    } else if (to_type->is_integer()) {
        return expr->type->is_number() || expr->type->is_real_pointer();
    } else if (to_type->is_float()) {
        return expr->type->is_number();
    } else {
        return is_assignable_to(to_type, expr);
    }
}

bool acorn::Sema::try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr) const {

    if (to_type->is_array() && from_type->does_contain_const()) {
        if (!from_type->is_array()) {
            // Check for array to pointer.
            if (from_type->is_pointer()) {
                auto to_arr_Type = static_cast<ArrayType*>(to_type);
                auto from_ptr_type = static_cast<PointerType*>(from_type);

                auto to_elm_type = to_arr_Type->get_elm_type();
                auto from_elm_type = from_ptr_type->get_elm_type();

                if (from_elm_type->does_contain_const() && !to_elm_type->does_contain_const()) {
                    return false;
                }
            }

            return false;
        }

        auto to_arr_Type = static_cast<ArrayType*>(to_type);
        Type* to_base_type = to_arr_Type->get_base_type();

        auto from_arr_Type = static_cast<ArrayType*>(from_type);
        Type* from_base_type = from_arr_Type->get_base_type();

        if (!has_valid_constness(to_base_type, from_base_type)) {
            return false;
        }
    } else if (to_type->is_pointer() && expr->is(NodeKind::Null)) {
        // Ignore this case because null can assign to all pointers
        // so we do not want to decense the pointers.
    } else if (!has_valid_constness(to_type, from_type)) {
        return false;
    }

    if (to_type->does_contain_const()) {
        to_type = to_type->remove_all_const();
    }
    if (from_type->does_contain_const()) {
        from_type = from_type->remove_all_const();
    }
   
    return true;
}

bool acorn::Sema::has_valid_constness(Type* to_type, Type* from_type) const {

    // There is nothing that can be violated if the from_type does not
    // even contain const.
    if (!from_type->does_contain_const()) {
        return true;
    }

    if (to_type->is_pointer()) {
        
        auto to_type_itr = to_type;
        auto from_type_itr = from_type;
        do {
            if (!from_type_itr->is_pointer()) {
                // The pointers do not have the same depth.
                return false;
            }

            auto to_ptr_type = static_cast<PointerType*>(to_type_itr);
            auto from_ptr_type = static_cast<PointerType*>(from_type_itr);

            auto to_elm_type = to_ptr_type->get_elm_type();
            auto from_elm_type = from_ptr_type->get_elm_type();

            if (from_elm_type->is_const() && !to_elm_type->is_const()) {
                // Underlying memory is const by the assignment's not
                // const memory.
                return false;
            }
            to_type_itr = to_elm_type;
            from_type_itr = from_elm_type;
        } while (to_type_itr->is_pointer());
    } else if (to_type->is_array() && from_type->does_contain_const()) {
        if (!from_type->is_array()) return false;

        auto to_arr_Type = static_cast<ArrayType*>(to_type);
        Type* to_base_type = to_arr_Type->get_base_type();

        auto from_arr_Type = static_cast<ArrayType*>(from_type);
        Type* from_base_type = from_arr_Type->get_base_type();

        if (!has_valid_constness(to_base_type, from_base_type)) {
            return false;
        }
    }

    return true;
}

void acorn::Sema::check_modifiable(Expr* expr) {
    if (!is_lvalue(expr)) {
        error(expand(expr), "Expected to be a modifiable value")
            .end_error(ErrCode::SemaExpectedModifiable);
        return;
    }
    if (expr->type->is_const()) {
        if (expr->is(NodeKind::IdentRef) &&
            static_cast<IdentRef*>(expr)->found_kind == IdentRef::VarKind) {
            error(expand(expr), "Cannot reassign to a const variable")
                .end_error(ErrCode::SemaReassignConstVariable);
        } else {
            error(expand(expr), "Cannot assign to const address")
                .end_error(ErrCode::SemaReassignConstAddress);
        }
    }
}

bool acorn::Sema::is_lvalue(Expr* expr) {
    if (expr->is(NodeKind::MemoryAccess)) {
        return true;
    }
    if (expr->is(NodeKind::IdentRef) || expr->is(NodeKind::DotOperator)) {
        auto ref = static_cast<IdentRef*>(expr);
        return ref->is_var_ref();
    }

    if (expr->is(NodeKind::UnaryOp)) {
        auto unary = static_cast<UnaryOp*>(expr);
        return unary->op == '*';
    }

    return false;
}

void acorn::Sema::check_division_by_zero(PointSourceLoc error_loc, Expr* expr) {
    if (!expr->is_foldable) return;

    if (auto ll_constant = gen_constant(error_loc, expr)) {
        if (!ll_constant->isZeroValue()) return;
        
        error(error_loc, "Division by zero")
            .end_error(ErrCode::SemaDivisionByZero);
    }
}

void acorn::Sema::create_cast(Expr* expr, Type* to_type) {
    // TODO: Removing all constness here although because function types are
    // pointers it may be possible at some point that you can cast between them
    // but at that point it is unclear if the type needs casting or not?
    if (expr->type->remove_all_const()->is_not(to_type->remove_all_const())) {
        expr->cast_type = to_type;
    }
}

bool acorn::Sema::check_condition(Expr* cond) {
    if (!is_condition(cond->type)) {
        error(expand(cond), "Expected condition")
            .end_error(ErrCode::SemaExpectedCondition);
        return false;
    }
    return true;
}

bool acorn::Sema::is_condition(Type* type) const {
    return type->is_bool() ||
           type->is_pointer() ||
           type->get_kind() == TypeKind::Null;
}

void acorn::Sema::check_modifier_incompatibilities(Decl* decl) {
    if (decl->has_modifier(Modifier::DllImport) && !decl->has_modifier(Modifier::Native)) {
        error(decl->get_modifier_location(Modifier::DllImport),
              "Cannot have dllimport modifier without native modifier")
            .end_error(ErrCode::SemaDllImportWithoutNativeModifier);
    }
    if (decl->has_modifier(Modifier::Private) && decl->has_modifier(Modifier::Public)) {
        error(decl, "Cannot have both prv and pub modifiers")
            .end_error(ErrCode::SemaHaveBothPrivateAndPublicModifier);
    }
}

void acorn::Sema::display_circular_dep_error(SourceLoc error_loc, Decl* dep, const char* msg, ErrCode error_code) {
    logger.begin_error(error_loc, msg);
    llvm::SmallVector<Decl*> dep_chain;
    Decl* start_dep = dep;
    while (dep) {
        if (std::ranges::find(dep_chain, dep) != dep_chain.end()) {
            // Prevent possible endless dependency determination.
            break;
        }

        dep_chain.push_back(dep);
        dep = dep->dependency;
    }

    logger.add_line("Dependency graph:").remove_period();
    logger.add_empty_line();

    // Calculate the maximum name length to format the display better.
    size_t max_name_length = 0;
    for (const auto& dep : dep_chain) {
        max_name_length = std::max(max_name_length, dep->name.reduce().size());
    }

    for (auto itr = dep_chain.begin(); itr != dep_chain.end(); ++itr) {
        Decl* dep_lhs = *itr;
        Decl* dep_rhs = (itr + 1) != dep_chain.end() ? *(itr + 1) : start_dep;

        logger.add_line([dep_lhs, dep_rhs, max_name_length](Logger& logger) {
            size_t lhs_pad = max_name_length - dep_lhs->name.reduce().size();
            size_t rhs_pad = max_name_length - dep_rhs->name.reduce().size();

            logger.fmt_print("  '%s'%s deps-on '%s'.   %s",
                             dep_lhs->name,
                             std::string(lhs_pad, ' '),
                             dep_rhs->name,
                             std::string(rhs_pad, ' '));
            dep_lhs->show_location_msg(logger);
        }).remove_period();
    }

    logger.end_error(error_code);
}

acorn::Struct* acorn::Sema::find_struct(Identifier name) {

    if (auto found_struct = file->get_namespace()->find_struct(name)) {
        return found_struct;
    }

    if (auto found_struct = file->find_struct(name)) {
        return found_struct;
    }

    auto& imports = file->get_imports();
    auto itr = imports.find(name);
    if (itr != imports.end()) {
        auto importn = itr->second;
        if (importn->is_imported_struct()) {
            return itr->second->imported_struct;
        }
    }

    return nullptr;
}

llvm::Constant* acorn::Sema::gen_constant(PointSourceLoc error_loc, Expr* expr) {
    IRGenerator generator(context);
    auto ll_value = generator.gen_rvalue(expr);
    if (ll_value->getValueID() == llvm::Value::ValueTy::PoisonValueVal) {
        error(error_loc, "Signed overflow")
            .end_error(ErrCode::NumericOverflow);
        return nullptr;
    }

    return llvm::cast<llvm::Constant>(ll_value);
}

std::string acorn::Sema::get_type_mismatch_error(Type* to_type, Expr* expr) const {
    if (to_type->is_integer() && to_type->is_not(context.char_type) &&
        expr->is(NodeKind::Number) && expr->is_foldable && expr->type->is_integer()) {
   
        return get_error_msg_for_value_not_fit_type(static_cast<Number*>(expr));
    } else if (to_type->is_function_type() && expr->type->get_kind() == TypeKind::FuncsRef) {
        
        auto ref = static_cast<IdentRef*>(expr);
        auto func = (*ref->funcs_ref)[0];
        
        auto types = func->params | std::views::transform([](Var* param) { return param->type; })
                                  | std::ranges::to<llvm::SmallVector<Type*>>();

        auto from_type = type_table.get_function_type(func->return_type, types);
        return get_type_mismatch_error(to_type, from_type);
    } else {
        return get_type_mismatch_error(to_type, expr->type);
    }
}

std::string acorn::Sema::get_type_mismatch_error(Type* to_type, Type* from_type) const {
    if (from_type->is(context.funcs_ref_type)) {
        return std::format("Mismatched types. Expected '{}' but found a reference to a function",
            to_type->to_string());
    } else {
        return std::format("Mismatched types. Expected '{}' but found '{}'",
            to_type->to_string(), from_type->to_string());
    }
}

acorn::Var* acorn::Sema::SemScope::find_variable(Identifier name) const {
    const SemScope* scope_ptr = this;
    while (scope_ptr) {
        auto itr =std::ranges::find_if(scope_ptr->variables, [name](Var* var) {
            return var->name == name; });
        if (itr != scope_ptr->variables.end()) {
            return *itr;
        }

        scope_ptr = scope_ptr->parent;
    }

    return nullptr;
}
