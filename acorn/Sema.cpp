#include "Sema.h"

#include <ranges>

#include "Util.h"
#include "Context.h"
#include "Type.h"
#include "SourceExpansion.h"
#include "ir/IRGen.h"
#include "Module.h"
#include "SourceFile.h"
#include "SpellChecking.h"
#include "DeepCopyAST.h"

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

        if (canidate->is_generic()) {
            logger.begin_error(canidate->loc, "Function 'main' cannot be generic")
                  .end_error(ErrCode::SemaMainFunctionCannotBeGeneric);
        }

        if (canidate->parsed_return_type->is_not(context.int_type) &&
            canidate->parsed_return_type->is_not(context.void_type)) {
            logger.begin_error(canidate->loc, "Function 'main' should have return type of 'int' or 'void'")
                  .end_error(ErrCode::SemaMainBadReturnType);
        }
        if (canidate->modifiers & Modifier::Native) {
            logger.begin_error(canidate->get_modifier_location(Modifier::Native),
                               "Function 'main' cannot have native modifier")
                .end_error(ErrCode::SemaMainCannotHaveModifier);
        }
        if (canidate->modifiers & Modifier::DllImport) {
            logger.begin_error(canidate->get_modifier_location(Modifier::DllImport),
                               "Function 'main' cannot have dllimport modifier")
                  .end_error(ErrCode::SemaMainCannotHaveModifier);
        }
    }
    return false;
}

void acorn::Sema::check_for_duplicate_functions(Namespace* nspace, Context& context) {
    nspace->set_duplicates_checked();

    for (const auto& [_, funcs] : nspace->get_functions()) {
        check_for_duplicate_functions(funcs, context);
    }

    for (const auto& [_, composite] : nspace->get_composites()) {
        if (composite->is(NodeKind::Struct)) {
            auto structn = static_cast<Struct*>(composite);

            check_for_duplicate_functions(structn->nspace, context);
            check_for_duplicate_functions(structn->constructors, context);

            for (auto& duplicate_info : structn->duplicate_struct_func_infos) {
                report_redeclaration(duplicate_info.duplicate_function,
                                     duplicate_info.prior_function,
                                     duplicate_info.duplicate_function->is_constructor ? "constructor" : "destructor",
                                     ErrCode::SemaDuplicateFunc);
            }
        } else if (composite->is(NodeKind::Interface)) {
            auto interfacen = static_cast<Interface*>(composite);
            auto& funcs = interfacen->functions;
            for (auto itr = funcs.begin(); itr != funcs.end(); ++itr) {
                for (auto itr2 = itr + 1; itr2 != funcs.end(); ++itr2) {
                    if ((*itr)->name != (*itr2)->name) {
                        continue;
                    }
                    if (check_for_duplicate_match(*itr, *itr2)) {
                        break;
                    }
                }
            }
        }
    }
}

void acorn::Sema::check_for_duplicate_functions(const FuncList& funcs, Context& context) {
    bool decls_have_errors = false;
    for (auto itr = funcs.begin(); itr != funcs.end(); ++itr) {
        Func* func = *itr;
        Sema analyzer(context, func->file, func->get_logger());
        if (!analyzer.check_function_decl(func)) {
            decls_have_errors = true;
        }
    }
    if (decls_have_errors) {
        // Do not proceed to checking duplicates because it could report
        // nonsensical information.
        return;
    }

    for (auto itr = funcs.begin(); itr != funcs.end(); ++itr) {
        for (auto itr2 = itr+1; itr2 != funcs.end(); ++itr2) {
            if (check_for_duplicate_match(*itr, *itr2)) {
                break;
            }
        }
    }
}

void acorn::Sema::check_all_other_duplicates(Module& modl, Context& context) {
    // Reporting all other duplcates.
    auto get_duplicate_kind_str = [](Decl* decl) finline {
        if (decl->is(NodeKind::Var)) {
            return "variable";
        } else if (decl->is(NodeKind::Struct)) {
            return "struct";
        } else if (decl->is(NodeKind::Enum)) {
            return "enum";
        } else if (decl->is(NodeKind::Interface)) {
            return "interface";
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
        } else if (decl->is(NodeKind::Enum)) {
            return ErrCode::SemaDuplicateGlobalEnum;
        } else if (decl->is(NodeKind::Interface)) {
            return ErrCode::SemaDuplicateGlobalInterface;
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
    if (do_functions_match<false>(func1, func2)) {
        report_redeclaration(func1, func2, func1->is_constructor ? "constructor" : "function", ErrCode::SemaDuplicateFunc);
        return true;
    }
    return false;
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

void acorn::Sema::report_nodes_wrong_scopes(Module& modl) {

    auto report = []<typename T>(Logger& logger,
                                 T loc,
                                 ScopeLocation location,
                                 auto expr_or_stmt_str) finline {
        const char* scope_str;
        if (location == ScopeLocation::Global) {
            scope_str = "global";
        } else if (location == ScopeLocation::Struct) {
            scope_str = "struct";
        } else if (location == ScopeLocation::Interface) {
            scope_str = "interface";
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

    // This goes after resolving imports as to not have to waste time checking
    // if the String struct exists.
    if (!context.should_stand_alone()) {
        // Auto importing the String struct.
        file->try_add_import(context.std_string_struct_import);
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

    auto add_composite = [&logger, importn](Decl* composite) finline {
        if (importn->is_static) {
            logger.begin_error(importn->loc, "Cannot static import a %s", composite->get_composite_kind())
                .end_error(ErrCode::SemaCannotStaticImportStruct);
        } else {
            importn->set_imported_composite(composite);
        }
    };

    auto report_could_not_find = [&logger](ImportStmt::KeyPart& key_part,
                                           const char* fmt,
                                           ErrorSpellChecker& spell_checker) finline{
        logger.begin_error(key_part.error_loc, fmt, key_part.name);
        spell_checker.search(logger, key_part.name);
        logger.end_error(ErrCode::SemaCouldNotResolveImport);
    };

    auto report_could_not_find_general = [&report_could_not_find](ImportStmt::KeyPart& key_part,
                                                                  ErrorSpellChecker& spell_checker) finline {
        report_could_not_find(key_part, "Could not find '%s'", spell_checker);
    };
    auto report_could_not_find_composite = [&report_could_not_find](ImportStmt::KeyPart& key_part,
                                                                 ErrorSpellChecker& spell_checker) finline {
        report_could_not_find(key_part, "Could not find struct, enum, or interface '%s'", spell_checker);
    };
    auto report_could_not_find_namespace = [&report_could_not_find](ImportStmt::KeyPart& key_part,
                                                                    ErrorSpellChecker& spell_checker) finline{
        report_could_not_find(key_part, "Could not find namespace '%s'", spell_checker);
    };
    auto report_could_not_find_module = [&report_could_not_find](ImportStmt::KeyPart& key_part,
                                                                 ErrorSpellChecker& spell_checker) finline{
        report_could_not_find(key_part, "Could not find module '%s'", spell_checker);
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

    ErrorSpellChecker spell_checker(context.should_show_spell_checking());

    if (key.size() == 1 && importn->within_same_modl) {
        auto& modl = importn->file->modl;
        Identifier ident = key[0].name;
        if (auto nspace = modl.find_namespace(ident)) {
            add_namespace(nspace);
        } else if (auto composite = modl.find_composite(ident)) {
            add_composite(composite);
        } else {
            spell_checker.add_searches(modl.get_namespaces());
            spell_checker.add_searches(modl.get_composites());
            report_could_not_find_general(key[0], spell_checker);
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
            spell_checker.add_searches(context.get_modules());
            report_could_not_find_module(key[0], spell_checker);
        }
        return;
    }

    if (key.size() == 2 && importn->within_same_modl) {
        auto& modl = importn->file->modl;
        if (auto nspace = modl.find_namespace(key[0].name)) {
            if (auto composite = nspace->find_composite(key[1].name)) {
                add_composite(composite);
            } else {
                spell_checker.add_searches(nspace->get_composites());
                report_could_not_find_composite(key[1], spell_checker);
            }
        } else {
            spell_checker.add_searches(modl.get_namespaces());
            report_could_not_find_namespace(key[0], spell_checker);
        }
        return;
    }

    if (key.size() == 2 && importn->within_parent_modl) {
        auto& modl = importn->file->modl;

        if (key[0].name != context.module_identifier) {
            report_expect_module_identifier(key[0]);
            return;
        }

        if (auto composite = modl.find_composite(key[1].name)) {
            add_composite(composite);
        } else {
            spell_checker.add_searches(modl.get_composites());
            report_could_not_find_composite(key[1], spell_checker);
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
            } else if (auto composite = modl->find_composite(key[1].name)) {
                add_composite(composite);
            } else {
                spell_checker.add_searches(modl->get_namespaces());
                spell_checker.add_searches(modl->get_composites());
                report_could_not_find_general(key[1], spell_checker);
            }
        } else {
            spell_checker.add_searches(context.get_modules());
            report_could_not_find_module(key[0], spell_checker);
        }
        return;
    }

    if (key.size() == 3) {
        if (auto modl = context.find_module(key[0].name)) {
            if (auto nspace = modl->find_namespace(key[1].name)) {
                if (auto composite = nspace->find_composite(key[2].name)) {
                    add_composite(composite);
                } else {
                    spell_checker.add_searches(nspace->get_composites());
                    report_could_not_find_composite(key[2], spell_checker);
                }
            } else {
                spell_checker.add_searches(modl->get_namespaces());
                report_could_not_find_namespace(key[1], spell_checker);
            }
        } else {
            spell_checker.add_searches(context.get_modules());
            report_could_not_find_module(key[0], spell_checker);
        }
        return;
    }

    report_invalid_import();
}


// Declaration checking
//--------------------------------------

void acorn::Sema::check_function(Func* func) {

    // The function is a member function so need to check the fields first
    // in case they are referenced.
    //
    // Note: we cannot check this in `check_function_decl` because otherwise
    // the struct may attempt to use the precomputed information of the function's
    // declaration to determine things such as if it matches an interface function
    // but then its declaration would not have been checked.
    if (func->structn) {
        if (!ensure_struct_checked(func->loc, func->structn)) {
            return;
        }
    }

    if (func->has_modifier(Modifier::Native)) {
        return;
    }

    if (func->interfacen) {
        return;
    }

    // -- debug
    // Logger::debug("checking function: %s", func->name);

    cur_func   = func;
    cur_struct = func->structn;

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
}

void acorn::Sema::check_variable(Var* var) {

    auto cleanup = [this, var]() finline {
        var->is_being_checked = false;
        cur_global_var = nullptr;
        if (cur_scope) {
            cur_scope->cur_try = nullptr;
        }
    };

    check_modifier_incompatibilities(var);
    if (var->has_modifier(Modifier::Readonly) && !var->is_field()) {
        error(var, "Only fields can have readonly modifier")
            .end_error(ErrCode::SemaOnlyFieldsCanHaveReadonlyModifier);
    }

    // This must go up top before returning due to any errors because otherwise
    // future variables will not be able to find the reference of the variable
    // leading to bad error messages.
    bool needs_added_to_local_scope = !var->is_global && !var->is_param() && !var->is_field();
    if (needs_added_to_local_scope) {
        add_variable_to_local_scope(var);
    }

    // Reporting errors if the variable is local to a function and has
    // modifiers.
    if (!var->is_global && !var->is_field() && var->modifiers) {
        for (uint32_t mod = Modifier::Start; mod != Modifier::End; mod *= 2) {
            if (var->modifiers & mod) {
                error(var->get_modifier_location(mod), "Modifier cannot apply to local variable")
                    .end_error(ErrCode::SemaLocalVarHasModifiers);
            }
        }
    }

    if (var->is_global) {
        cur_global_var = var;
    }
    var->is_being_checked = true;
    var->has_been_checked = true; // Set early to prevent circular checking.


    // Check the assignment.
    //
    if (var->assignment) {
        if (var->assignment->tryn) {
            if (var->is_param()) {
                error(var->assignment->tryn->loc, "Parameters cannot use try expression")
                    .still_give_main_location_priority()
                    .add_individual_underline(var->loc)
                    .end_error(ErrCode::SemaParamsCannotUseTry);
                return cleanup();
            } else if (var->is_field()) {
                error(var->assignment->tryn->loc, "Fields cannot use try expression")
                    .still_give_main_location_priority()
                    .add_individual_underline(var->loc)
                    .end_error(ErrCode::SemaFieldsCAnnotUseTry);
                return cleanup();
            }

            cur_scope->cur_try = var->assignment->tryn;
        }

        TypeKind type_kind = var->parsed_type->get_kind();
        // Check an array assignment taking into account the type of the variable.
        if (var->assignment->is(NodeKind::Array) &&
            type_kind == TypeKind::Array || type_kind == TypeKind::UnresolvedArray
            ) {

            // TODO: Optimization: The type is being fixed here and below.
            var->type = fixup_type(var->parsed_type);
            if (!var->type) {
                return cleanup();
            }

            auto arr_type = static_cast<ArrayType*>(var->type);
            check_array(static_cast<Array*>(var->assignment), arr_type->get_elm_type());
        }
        // Check instances like:  `a: int[] = [1,2,3,4];`
        else if (var->assignment->is(NodeKind::Array) &&
                 type_kind == TypeKind::AssignDeterminedArray) {

            auto assign_det_arr_type = static_cast<AssignDeterminedArrayType*>(var->parsed_type);

            // TODO: Optimization: The type is being fixed here and below.
            auto fixed_elm_type = fixup_type(assign_det_arr_type->get_elm_type());
            if (!fixed_elm_type) {
                return cleanup();
            }

            auto arr_type = static_cast<ArrayType*>(var->type);
            check_array(static_cast<Array*>(var->assignment), fixed_elm_type);
        } else if (var->assignment->is(NodeKind::MoveObj)) {
            check_moveobj(static_cast<MoveObj*>(var->assignment), true);
        } else {
            check_node(var->assignment);
        }

        if (!var->assignment->type) {
            return cleanup();
        }

        if (var->assignment->tryn) {
            check_try(var->assignment->tryn, true);
        }
    }

    if (var->parsed_type->get_kind() == TypeKind::AssignDeterminedArray && !var->assignment) {
        error(var, "Must have assignment to determine array type's length")
            .end_error(ErrCode::SemaVarRequiresAssignForDetArrType);
        return cleanup();
    }


    // If the variable uses type inference then resolving the type based on the assignment.
    //
    if (var->parsed_type == context.auto_type || var->parsed_type == context.const_auto_type) {
        if (!var->assignment) {
            error(var, "Must have assignment to determine infered type")
                .end_error(ErrCode::SemaMustHaveAssignmentToDetAuto);
            return cleanup();
        }

        switch (var->assignment->type->get_kind()) {
        case TypeKind::Void:
        case TypeKind::NamespaceRef:
        case TypeKind::EmptyArray:
        case TypeKind::Null:
        case TypeKind::Interface:
        case TypeKind::Expr:
            error(expand(var), "Cannot assign incomplete type '%s' to variable with inferred type",
                  var->assignment->type)
                .end_error(ErrCode::SemaCannotAssignIncompleteTypeToAuto);
            return cleanup();
        case TypeKind::Range:
            error(expand(var), "Cannot assign type 'range' to variable with inferred type",
                  var->assignment->type)
                .end_error(ErrCode::SemaCannotAssignIncompleteTypeToAuto);
            return cleanup();
        default:
            break;
        }

        var->type = var->assignment->type;
        if (var->type->is_const()) {
            // A :: 55;
            // b := A;
            // ^^^^^^
            // we don't want b to be const here.
            var->type = type_table.remove_const(var->type);
        }

        // Apply const at all depths.
        if (var->parsed_type == context.const_auto_type) {
            // TODO (maddie): is this not missing const being applied at all
            // depths for pointers?
            if (var->assignment->is(NodeKind::Array)) {
                auto arr_type = static_cast<ArrayType*>(var->type);

                llvm::SmallVector<uint32_t, 8> lengths;
                Type* elm_type = nullptr;
                while (true) {
                    elm_type = arr_type->get_elm_type();
                    lengths.push_back(arr_type->get_length());

                    if (!elm_type->is_array()) {
                        break;
                    }
                    arr_type = static_cast<ArrayType*>(elm_type);
                }

                auto base_type = type_table.get_const_type(elm_type);
                auto new_arr_type = type_table.get_arr_type(base_type, lengths.back());
                for (auto itr = lengths.rbegin() + 1; itr != lengths.rend(); ++itr) {
                    new_arr_type = type_table.get_arr_type(new_arr_type, *itr);
                }
                var->type = type_table.get_const_type(new_arr_type);

            } else if (!var->type->is_const()) {
                var->type = type_table.get_const_type(var->type);
            }
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

    // Make sure to keep this below the code that checks if the variable is
    // foldable!
    if (needs_added_to_local_scope) {
        if (!var->is_foldable) {
            cur_func->vars_to_alloc.push_back(var);
        }
    }

    if (var->type->is(context.void_type)) {
        error(var, "Variables cannot have type 'void'")
            .end_error(ErrCode::SemaVariableCannotHaveVoidType);
        return cleanup();
    }

    if (!var->assignment && !var->has_modifier(Modifier::Native) && !var->is_param()) {
        if (var->type->is_const()) {
            error(var, "Variables declared 'const' must be assigned a value")
                .end_error(ErrCode::SemaVariableConstNoValue);
            return cleanup();
        } else if (var->type->is_array()) {
            auto arr_type = static_cast<ArrayType*>(var->type);
            auto base_type = arr_type->get_base_type();
            if (base_type->is_const()) {
                error(var, "Variables with constant array must be assigned a value")
                    .end_error(ErrCode::SemaVariableConstNoValue);
                return cleanup();
            }
        }
    }

    // !! WARNING: If this code ever changes to not call `is_asignable_to` when
    // the variable has been declared `auto` then extra work must be done to
    // ensure things such as accessing of function references that are private.
    if (var->assignment && !is_assignable_to(var->type, var->assignment)) {
        bool is_assignable_to = false;

        if (var->assignment->is(NodeKind::FuncCall)) {
            FuncCall* call = static_cast<FuncCall*>(var->assignment);
            if (may_implicitly_convert_return_ptr(var->type, call)) {
                is_assignable_to = true;
                call->implicitly_converts_return = true;
            }
        }

        if (!is_assignable_to) {
            error(expand(var), get_type_mismatch_error(var->type, var->assignment).c_str())
                .end_error(ErrCode::SemaVariableTypeMismatch);
        }
    } else if (var->assignment) {
        create_cast(var->assignment, var->type);
    }

    return cleanup();
}

void acorn::Sema::check_struct(Struct* structn) {

    // -- Debug
    // Logger::debug("checking struct: %s", structn->name);

    check_modifiers_for_composite(structn, "Structs");

    cur_struct = structn;

    structn->is_being_checked = true;
    structn->has_been_checked = true; // Set early to prevent circular checking.

    for (size_t i = 0; i < structn->unresolved_extensions.size(); i++) {
        auto& extension = structn->unresolved_extensions[i];

        // Check for duplicates.
        for (long long j = static_cast<long long>(i - 1); j >= 0; j--) {
            auto& other_extension = structn->unresolved_extensions[j];
            if (extension.name == other_extension.name) {
                error(extension.error_loc, "Duplicate extension '%s'", extension.name)
                    .end_error(ErrCode::SemaDuplicateExtension);
                break;
            }
        }

        if (auto composite = find_composite(extension.name)) {
            if (composite->is_not(NodeKind::Interface)) {
                error(extension.error_loc, "Cannot extend from type '%s'", composite->name)
                    .end_error(ErrCode::SemaCannotExtendFromType);
                continue;
            }

            if (extension.is_dynamic) {
                structn->uses_vtable = true;
            }

            auto interfacen = static_cast<Interface*>(composite);
            structn->interface_extensions.push_back({ interfacen, extension.is_dynamic });
            check_struct_interface_extension(structn, interfacen, extension);

        } else {
            error(extension.error_loc, "Could not find extension '%s'", extension.name)
                .end_error(ErrCode::SemaCouldNotFindExtension);
        }
    }

    auto process_field_struct_type_state = [this, structn](SourceLoc field_loc, StructType* field_struct_type) finline {
        auto field_struct = field_struct_type->get_struct();

        structn->needs_destruction       |= field_struct->needs_destruction;
        structn->fields_need_destruction |= field_struct->needs_destruction;

        structn->needs_copy_call       |= field_struct->needs_copy_call;
        structn->fields_need_copy_call |= field_struct->needs_copy_call;

        structn->needs_move_call       |= field_struct->needs_move_call;
        structn->fields_need_move_call |= field_struct->needs_move_call;

        structn->needs_default_call |= field_struct->needs_default_call;

        if (field_struct->is_being_checked) {
            display_circular_dep_error(field_loc,
                                       cur_struct,
                                       "Circular struct dependency results in infinite storage requirement for struct",
                                       ErrCode::SemaCircularStructDeclDependency);
            structn->has_errors = true;
        }
    };

    uint32_t ll_field_count = 0;

    if (structn->uses_vtable) {
        structn->is_default_foldable = false;
        for (auto& extension : structn->interface_extensions) {
            if (!extension.is_dynamic) continue;
            ++ll_field_count;
        }
    }

    if (!structn->constructors.empty()) {
        structn->is_default_foldable = false;
    }

    for (Var* field : structn->fields) {

        field->ll_field_idx = ll_field_count;
        ++ll_field_count;

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
            structn->has_errors = true;
        } else {
            if (field->type->is_struct()) {
                process_field_struct_type_state(field->loc, static_cast<StructType*>(field->type));
            } else if (field->type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(field->type);
                auto base_type = arr_type->get_base_type();
                if (base_type->is_struct()) {
                    process_field_struct_type_state(field->loc, static_cast<StructType*>(base_type));
                }
            }
        }

        if (field->assignment) {
            if (field->type) {
                structn->is_default_foldable &= field->assignment->is_foldable;
            }

            structn->fields_have_assignments = true;
        } else if (field->type) {
            structn->is_default_foldable &= field->type->is_default_foldable();
        }
    }

    structn->needs_default_call |= structn->fields_have_assignments || structn->default_constructor;


    // !!!! No longer need to treat the struct as if it has been checked since the rest of Sema only cares
    // if the struct's fields have been checked not if the struct's functions have been. It is now safe to
    // continue on to checking information about the struct's member functions.
    //
    cur_struct = nullptr;
    structn->is_being_checked = false;

    if (structn->destructor) {
        if (structn->destructor->has_checked_declaration || check_function_decl(structn->destructor)) {
            if (!structn->destructor->params.empty()) {
                error(structn->destructor, "Destructors should not have any parameters")
                    .end_error(ErrCode::SemaDestructorsCannotHaveParams);
            }
        }
        if (structn->is_generic) {
            // Making sure to create a generic instance for the destructor since it is not
            // called directly.
            auto generic_instance = static_cast<GenericStructInstance*>(structn);
            generic_instance->generic_destructor_instance =
                structn->destructor->get_generic_instance(context.get_allocator(),
                                                          generic_instance->bound_types,
                                                          { context.void_type },
                                                          structn);
        }
    }
    auto check_move_or_copy_has_correct_param_type = [this, structn](Func* constructor,
                                                                     Type* elm_type,
                                                                     bool is_copy) finline {
        if (constructor->has_checked_declaration || check_function_decl(constructor)) {
            if (constructor->has_errors) {
                return;
            }
            // TODO (maddie): can this not result in circular dependencies? Should this not be checking
            // if the constructor has errors before continuing?
            auto struct_ptr_type = type_table.get_ptr_type(elm_type);
            if (constructor->params.size() != 1) {
                error(constructor,
                        "%s constructor should have one parameter of type '%s'",
                        is_copy ? "Copy" : "Move", struct_ptr_type)
                    .end_error(ErrCode::SemaCopyConstructorExpectsOneParam);
            } else {
                auto param1 = constructor->params[0];
                Type* param_type = param1->type;
                // Resolve situation like:
                //
                // generics(T)
                // struct A {
                //    fn copyobj(o: const A*) {}
                // }
                //
                if (param_type->does_contain_generics() && structn->is_generic) {
                    if (param_type->is_pointer()) {
                        auto param_ptr_type = static_cast<PointerType*>(param_type);
                        auto param_elm_type = param_ptr_type->get_elm_type();

                        if (param_elm_type->get_kind() == TypeKind::PartiallyBoundStruct) {
                            auto partially_bound_struct_type = static_cast<PartiallyBoundStructType*>(param_elm_type);
                            auto unbound_generic_struct = partially_bound_struct_type->get_unbound_generic_struct();

                            if (unbound_generic_struct->nspace == structn->nspace) {
                                auto generic_struct_instance = static_cast<GenericStructInstance*>(structn);

                                auto fixed_elm_type = fixup_partially_bound_struct_type(param_elm_type,
                                                                                        &generic_struct_instance->bound_types);
                                if (!fixed_elm_type) {
                                    return;
                                }

                                Type* new_param_type = type_table.get_ptr_type(fixed_elm_type);
                                if (param_type->is_const()) {
                                    new_param_type = type_table.get_const_type(new_param_type);
                                }
                                param_type = new_param_type;
                            }
                        }
                    }
                }

                if (param_type->is_not(struct_ptr_type)) {
                    error(param1,
                            "%s constructor parameter should be of type '%s'",
                            is_copy ? "Copy" : "Move", struct_ptr_type)
                        .end_error(is_copy ? ErrCode::SemaCopyConstructorExpectedStructPtrType
                                            : ErrCode::SemaMoveConstructorExpectedStructPtrType);
                }
            }
        }
    };
    if (structn->copy_constructor) {
        auto const_struct_type = type_table.get_const_type(structn->struct_type);
        check_move_or_copy_has_correct_param_type(structn->copy_constructor, const_struct_type, true);
        if (structn->is_generic) {
            // Making sure to create a generic instance for the move constructor since it is not
            // called directly.
            auto generic_instance = static_cast<GenericStructInstance*>(structn);
            generic_instance->generic_copy_constructor_instance =
                structn->copy_constructor->get_generic_instance(context.get_allocator(),
                                                                generic_instance->bound_types,
                                                                { context.void_type, type_table.get_ptr_type(const_struct_type) },
                                                                structn);
        }
    }
    if (structn->move_constructor) {
        check_move_or_copy_has_correct_param_type(structn->move_constructor, structn->struct_type, false);
        if (structn->is_generic) {
            // Making sure to create a generic instance for the move constructor since it is not
            // called directly.
            auto generic_instance = static_cast<GenericStructInstance*>(structn);
            generic_instance->generic_move_constructor_instance =
                structn->move_constructor->get_generic_instance(context.get_allocator(),
                                                                generic_instance->bound_types,
                                                                { context.void_type, type_table.get_ptr_type(structn->struct_type) },
                                                                structn);
        }
    }
    if (structn->default_constructor) {
        if (structn->is_generic) {
            // Making sure to create a generic instance for the move constructor since it is not
            // called directly.
            auto generic_instance = static_cast<GenericStructInstance*>(structn);
            generic_instance->generic_default_constructor_instance =
                structn->default_constructor->get_generic_instance(context.get_allocator(),
                                                                   generic_instance->bound_types,
                                                                   { context.void_type },
                                                                   structn);
        }
    }
}

void acorn::Sema::check_enum(Enum* enumn) {

    // -- Debug
    // Logger::debug("Checking enum: %s", enumn->name);

    check_modifiers_for_composite(enumn, "Enums");

    enumn->has_been_checked = true;
    enumn->is_being_checked = true;

    uint64_t defualt_index = 0;
    Type* values_type = enumn->enum_type->get_values_type();

    // TODO: check to make sure the explicit values type is a foldable type?
    bool has_explicit_values_type = values_type != nullptr;

    if (has_explicit_values_type) {
        if (is_incomplete_type(values_type)) {
            error(enumn, "Value type '%s' is an incomplete type", values_type)
                .end_error(ErrCode::SemaEnumValuesTypeIncomplete);
            return;
        }
    }

    size_t   count = 0;
    uint64_t index_counter = 0;
    bool has_non_assigning = false;
    bool has_conflicting_values_type = false;
    for (auto& value : enumn->values) {

        for (size_t i = 0; i < count; i++) {
            if (value.name == enumn->values[i].name) {
                error(value.name_loc, "Name '%s' already defined in enum", value.name)
                    .end_error(ErrCode::SemaDuplicateEnumValueName);
                break;
            }
        }

        if (value.assignment) {
            check_node(value.assignment);

            if (value.assignment->type) {
                bool assignment_is_integer = value.assignment->type->is_integer();

                if (!value.assignment->is_foldable) {
                    error(expand(value.assignment), "Values of enum expected to be determined at compile time")
                        .end_error(ErrCode::SemaEnumValuesNotFoldable);
                } else if (value.assignment->type->is_integer()) {
                    if (auto ll_const = gen_constant(value.assignment)) {
                        auto ll_integer = llvm::cast<llvm::ConstantInt>(ll_const);
                        value.index = ll_integer->getZExtValue();
                        index_counter = value.index;

                        if (count == 0) {
                            defualt_index = ll_integer->getZExtValue();
                        }
                    }
                } else {
                    value.index = index_counter;
                }

                if (values_type) {
                    if (!is_assignable_to(values_type, value.assignment)) {
                        if (has_explicit_values_type) {
                            error(expand(value.assignment), "Enum value expected to be type '%s' but found '%s'",
                                  values_type, value.assignment->type)
                                .end_error(ErrCode::SemaEnumValueWrongType);
                        } else {
                            has_conflicting_values_type = true;
                            error(expand(value.assignment), "Incompatible types for enum '%s'. First found '%s' but now '%s'",
                                  enumn->name, values_type, value.assignment->type)
                            .end_error(ErrCode::SemaIncompatibleEnumValueTypes);
                        }
                    }
                } else {
                    values_type = value.assignment->type;
                }
            }
        } else {
            has_non_assigning = true;

            // No assignment so set it to be equal to one past the last value.
            value.index = index_counter;
        }

        ++index_counter;
        ++count;
    }

    if (has_non_assigning && !has_conflicting_values_type &&
        values_type && !values_type->is_integer()) {
        // Must always assign to all enum slots if the
        for (auto& value : enumn->values) {
            if (!value.assignment) {
                error(value.name_loc, "Enum values require assignment when the value type of the enum is '%s'",
                      values_type)
                    .end_error(ErrCode::SemaEnumValueNoAssignment);
            }
        }
    }

    enumn->enum_type->set_default_index(defualt_index);
    enumn->enum_type->set_values_type(values_type);
    if (values_type && values_type->is_integer()) {
        enumn->enum_type->set_index_type(values_type);
    } else {
        enumn->enum_type->set_index_type(context.int_type);
    }

    enumn->enum_type->set_values_type(!values_type ? context.int_type : values_type);
    enumn->is_being_checked = false;

}

void acorn::Sema::check_interface(Interface* interfacen) {

    interfacen->has_been_checked = true;
    interfacen->is_being_checked = true;

    check_modifiers_for_composite(interfacen, "interface");

    for (auto func : interfacen->functions) {
        if (func->modifiers != 0) {
            for (uint32_t mod = Modifier::Start; mod != Modifier::End; mod *= 2) {
                if (func->has_modifier(mod)) {
                    error(func->get_modifier_location(mod), "Interface functions cannot have modifiers")
                        .end_error(ErrCode::SemaInterfaceFuncNoHaveModifier);
                }
            }
        }

        check_function_decl(func);
    }

    interfacen->is_being_checked = false;
}

bool acorn::Sema::check_function_decl(Func* func) {


    // -- debug
    // Logger::debug("checking function declaration: %s | %s", func->name, func->file->path);


    // TODO: This does not take into account that it is possible a
    // parameter with a default value calls another function and that
    // other function's declaration has not been fullfilled yet.
    func->has_checked_declaration = true;
    func->is_checking_declaration = true;

    cur_func_decl = func;

    auto error_cleanup = [](Func* func) finline {
        func->has_errors = true;
        func->is_checking_declaration = false;
    };

    if (func->structn) {
        if (func->has_modifier(Modifier::Native)) {
            error(func->get_modifier_location(Modifier::Native), "Member functions cannot have 'native' modifier")
                .end_error(ErrCode::SemaMemberFuncHasNativeModifier);
        }
        if (func->has_modifier(Modifier::DllImport)) {
            error(func->get_modifier_location(Modifier::DllImport), "Member functions cannot have 'dllimport' modifier")
                .end_error(ErrCode::SemaMemberFuncHasDllimportModifier);
        }
    } else if (!func->interfacen && func->is_constant) {
        error(func->get_function_const_location(), "Only member functions may be marked 'const'")
            .end_error(ErrCode::SemaOnlyMemberFuncsMarkedConst);
    }

    if (func == context.get_main_function() && !func->raised_errors.empty()) {
        error(func, "Function 'main' cannot be defined to raise errors")
            .end_error(ErrCode::SemaMainFunctionCannotDefineRaisesError);
    }

    if (!func->raised_errors.empty() && func->has_modifier(Modifier::Native)) {
        error(func, "Functions with modifier 'native' cannot be defined to raise errors")
            .end_error(ErrCode::SemaNativeFunctionsCannotDefineRaisesError);
    }

    if (func->interfacen && func->default_params_offset != -1) {
        auto err_loc = func->get_function_first_default_param_location();
        error(err_loc, "Interface functions cannot have default parameter values")
            .end_error(ErrCode::SemaInterfaceFuncHaveDefaultParamVal);
    }

    if (func->is_generic()) {

        if (func->interfacen) {
            error(func, "Generic functions cannot be part of interfaces")
                .end_error(ErrCode::SemaGenericFunctionCannotBePartOfInterface);
            error_cleanup(func);
            return false;
        }

        if (func->has_modifier(Modifier::Native)) {
            error(func->get_modifier_location(Modifier::Native), "Generic functions cannot have 'native' modifier")
                .end_error(ErrCode::SemaGenericFuncsCannotBeNative);
            error_cleanup(func);
            return false;
        }
        if (func->has_modifier(Modifier::Native)) {
            error(func->get_modifier_location(Modifier::DllImport), "Generic functions cannot have 'dllimport' modifier")
                .end_error(ErrCode::SemaGenericFuncsCannotHaveDllimport);
            error_cleanup(func);
            return false;
        }
    }

    bool raised_errors_have_errors = false;
    for (auto& raised_error : func->raised_errors) {
        if (!check_raised_error(raised_error)) {
            raised_errors_have_errors = true;
        }
    }
    if (raised_errors_have_errors) {
        error_cleanup(func);
        return false;
    }

    check_modifier_incompatibilities(func);

    if (func->uses_native_varargs && !func->has_modifier(Modifier::Native)) {
        error(func, "Functions cannot use native variadic parameters unless marked as native")
            .end_error(ErrCode::SemaFuncHasNativeVarArgButNotNative);
    }
    if (func->has_modifier(Modifier::Readonly)) {
        error(func, "Functions cannot have readonly modifier")
            .end_error(ErrCode::SemaFuncsCannotHaveReadonlyModifier);
    }

    if (func->parsed_return_type->get_kind() == TypeKind::AssignDeterminedArray) {
        error(func, "Functions cannot have return type '%s'", func->parsed_return_type)
            .end_error(ErrCode::SemaFuncsCannotHaveAssignDetArrType);
        error_cleanup(func);
        return false;
    }

    func->return_type = fixup_type(func->parsed_return_type);
    if (!func->return_type) {
        // Failed to fixup return type so returning early.
        error_cleanup(func);
        return false;
    }
    func->partially_qualified_types.push_back(func->return_type);

    if (func->uses_varargs) {
        auto last_param = func->params.back();
        if (last_param->has_implicit_ptr) {
            error(last_param->loc, "Variadic arguments cannot use implicit pointers")
                .end_error(ErrCode::SemaVariadicArgumentsCannotUseImplicitPtrs);
            error_cleanup(func);
            return false;
        }
    }

    if (func->is_constant && (func->is_constructor || func->is_destructor)) {
        error(func->get_function_const_location(), "%s cannot be marked 'const'",
              func->is_constructor ? "Constructors" : "Destructors")
            .end_error(ErrCode::SemaOnlyMemberFuncsMarkedConst);
        error_cleanup(func);
        return false;
    }

    if (func->has_modifier(Modifier::Native)) {
        if (func->return_type->is_aggregate()) {
            if (func->return_type->is_struct()) {
                error(func, "Functions with modifier 'native' cannot return struct types")
                    .end_error(ErrCode::SemaNativeFuncCannotRetAggregate);
            } else if (func->return_type->is_array()) {
                error(func, "Functions with modifier 'native' cannot return array types")
                    .end_error(ErrCode::SemaNativeFuncCannotRetAggregate);
            } else if (func->return_type->is_slice()) {
                error(func, "Functions with modifier 'native' cannot return slice types")
                    .end_error(ErrCode::SemaNativeFuncCannotRetAggregate);
            }
        }
        if (func->has_implicit_return_ptr) {
            error(func, "Functions with modifier 'native' cannot return implicit pointers")
                .end_error(ErrCode::SemaNativeFuncCannotRetImplicitPtr);
        }
        if (func->structn) {
            error(func, "Functions with modifier 'native' cannot be member functions")
                .end_error(ErrCode::SemaNativeFuncCannotBeMemberFunc);
        }
    }

    // If we ever decide to allow nesting functions for some reason then this
    // will possibly be a problem because it will have overriden the current scope.
    size_t pcount = 0;
    bool encountered_param_default_value = false;
    Var* last_default_param;
    for (Var* param : func->params) {
        // Check for duplicate parameter names.
        //
        // Only have to search parameters that were declared before
        // this variable.
        for (size_t i = 0; i < pcount; i++) {
            if (param->name == func->params[i]->name) {
                error(param, "Duplicate declaration of parameter '%s'", param->name)
                    .end_error(ErrCode::SemaDuplicateParamVariableDecl);
                error_cleanup(func);
                return false;
            }
        }

        if (encountered_param_default_value && !param->assignment) {
            error(expand(last_default_param), "Parameters with default values must come last")
                .end_error(ErrCode::SemaDefaultParamValueMustComeLast);
            error_cleanup(func);
            return false;
        }

        if (param->assignment) {
            last_default_param = param;
            encountered_param_default_value = true;
        }


        if (param->assignment_contains_generics) {
            if (param->parsed_type->get_kind() == TypeKind::AssignDeterminedArray) {
                error(param, "Cannot determine size of array because the expression it depends on contains generic types")
                    .end_error(ErrCode::SemaCannotDetArrSizeAssignContainsGenericTypes);
                func->has_errors = true;
                continue;
            }

            if (param->parsed_type != context.auto_type) {
                param->type = fixup_type(param->parsed_type);
                if (param->type) {
                    func->partially_qualified_types.push_back(param->type);
                }
            } else {
                func->partially_qualified_types.push_back(context.indeterminate_type);
            }
            continue;
        }

        check_variable(param);
        if (!param->type) {
            func->has_errors = true;
            continue;
        }

        if (func->is_generic()) {
            func->partially_qualified_types.push_back(param->type);
        }

        if (func->has_modifier(Modifier::Native)) {
            if (param->type->is_aggregate()) {
                if (param->type->is_struct()) {
                    error(param, "Parameters of functions with modifier 'native' cannot have a struct type")
                        .end_error(ErrCode::SemaNativeFuncParamsCannotBeAggregate);
                    func->has_errors = true;
                } else {
                    error(param, "Parameters of functions with modifier 'native' cannot have an array type")
                        .end_error(ErrCode::SemaNativeFuncParamsCannotBeAggregate);
                    func->has_errors = true;
                }
            } else if (param->has_implicit_ptr) {
                error(param, "Parameters of functions with modifier 'native' cannot have implicit pointers")
                    .end_error(ErrCode::SemaNativeFuncParamCannotHaveImplicitPtr);
                func->has_errors = true;
            }
        }

        ++pcount;
    }

    if (func->has_modifier(Modifier::Native)) {
        Identifier link_name = func->linkname.empty() ? func->name
                                                      : Identifier::get(func->linkname);
        auto itr = context.ll_intrinsics_table.find(link_name);
        if (itr != context.ll_intrinsics_table.end()) {
            func->ll_intrinsic_id = itr->second;

            // Validating that the parameters and return are correct.

            for (Var* param : func->params) {
                if (param->assignment) {
                    error(expand(param), "Parameters of intrinsic functions cannot have a default value")
                        .end_error(ErrCode::SemaIntrinsicFuncParamDefValue);
                    func->has_errors = true;
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
                    func_decl += intrinsic_def.name.to_string();
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

                func->has_errors = true;
            }
        }
    }

    if ((func->uses_native_varargs || func->uses_varargs) && func->default_params_offset != -1) {
        for (Var* param : func->params) {
            if (!param->assignment) continue;
            error(param, "Functions with variadic parameters cannot have parameters with default values")
                .end_error(ErrCode::SemaFuncWithVarArgNoParamDefaultVal);
            func->has_errors = true;
        }
    }

    func->is_checking_declaration = false;
    return !func->has_errors;
}

bool acorn::Sema::check_raised_error(RaisedError& raised_error) {
    if (auto composite = find_composite(raised_error.name)) {
        if (composite->is_not(NodeKind::Struct)) {
            error(raised_error.error_loc, "Raised error '%s' must be an error struct", raised_error.name)
                .end_error(ErrCode::SemaRaisedErrorNotStruct);
            return false;
        }

        auto structn = static_cast<Struct*>(composite);
        if (!ensure_struct_checked(raised_error.error_loc, structn)) {
            return false;
        }

        bool extends_error_interface = false;
        for (auto& extension : structn->interface_extensions) {
            if (extension.interfacen == context.std_error_interface) {
                extends_error_interface = true;
                break;
            }
        }

        if (!extends_error_interface) {
            error(raised_error.error_loc, "Cannot raise '%s' because it does not extend 'std.Error' interface",
                  raised_error.name)
                .end_error(ErrCode::SemaRaisedErrorNotExtendErrorInterface);
            return false;
        }

        raised_error.structn = structn;

    } else {
        error(raised_error.error_loc, "Raised error '%s' not found", raised_error.name)
            .end_error(ErrCode::SemaRaisedErrorNotFound);
        return false;
    }
    return true;
}

void acorn::Sema::ensure_global_variable_checked(SourceLoc error_loc, Var* var) {
    if (cur_global_var) {
        cur_global_var->dependency = var;

        if (var->is_being_checked) {
            if (cur_global_var->dependency == cur_global_var) {
                report_error_cannot_use_variable_before_assigned(error_loc, cur_global_var);
            } else {
                display_circular_dep_error(error_loc,
                                           cur_global_var,
                                           "Global variables form a circular dependency",
                                           ErrCode::SemaGlobalCircularDependency);
            }
        }
    }

    if (!var->has_been_checked) {
        Sema sema(context, var->file, var->get_logger());
        sema.check_variable(var);
    }
}

bool acorn::Sema::ensure_struct_checked(SourceLoc error_loc, Struct* structn) {
    if (cur_struct) {
        cur_struct->dependency = structn;

        // We do not display circular dependencies here because the fields of a struct need
        // to be able to reference the struct.

        //display_circular_dep_error(error_loc,
        //                           cur_struct,
        //                           "Structs form a circular dependency",
        //                           ErrCode::SemaGlobalCircularDependency);
    }

    if (!structn->has_been_checked) {
        Sema sema(context, structn->file, structn->get_logger());
        sema.check_struct(structn);
    }
    return !structn->has_errors;
}

void acorn::Sema::ensure_enum_checked(SourceLoc error_loc, Enum* enumn) {
    if (cur_enum) {
        cur_enum->dependency = enumn;

        if (cur_enum->is_being_checked) {
            display_circular_dep_error(error_loc,
                                       cur_enum,
                                       "Enum forms a circular dependency",
                                       ErrCode::SemaGlobalCircularDependency);
        }
    }

    if (!enumn->has_been_checked) {
        Sema sema(context, enumn->file, enumn->get_logger());
        sema.check_enum(enumn);
    }
}

void acorn::Sema::ensure_interface_checked(SourceLoc error_loc, Interface* interfacen) {
    if (cur_interface) {
        cur_interface->dependency = interfacen;

        if (cur_interface->is_being_checked) {
            display_circular_dep_error(error_loc,
                                       cur_interface,
                                       "Interface forms a circular dependency",
                                       ErrCode::SemaGlobalCircularDependency);
        }
    }

    if (!interfacen->has_been_checked) {
        Sema sema(context, interfacen->file, interfacen->get_logger());
        sema.check_interface(interfacen);
    }
}

void acorn::Sema::check_modifier_incompatibilities(Decl* decl) {
    if (decl->has_modifier(Modifier::DllImport) && !decl->has_modifier(Modifier::Native)) {
        error(decl->get_modifier_location(Modifier::DllImport),
              "Cannot have dllimport modifier without native modifier")
            .end_error(ErrCode::SemaDllImportWithoutNativeModifier);
    }
#define is_pow2(n) ((n) & ((n) - 1)) == 0
    uint32_t access_bits = (decl->modifiers >> Modifier::AccessShift) & Modifier::AccessMask;
    if (is_pow2(access_bits)) {
        if (decl->has_modifier(Modifier::Public) && decl->has_modifier(Modifier::Private)) {
            error(decl, "Cannot have both private and public modifiers")
                .end_error(ErrCode::SemaHaveBothPrivateAndPublicModifier);
        }
        if (decl->has_modifier(Modifier::Public) && decl->has_modifier(Modifier::Readonly)) {
            error(decl, "Cannot have both readonly and public modifiers")
                .end_error(ErrCode::SemaHaveBothPrivateAndPublicModifier);
        }
        if (decl->has_modifier(Modifier::Private) && decl->has_modifier(Modifier::Readonly)) {
            error(decl, "Cannot have both private and readonly modifiers")
                .end_error(ErrCode::SemaHaveBothPrivateAndPublicModifier);
        }
    }
#undef is_pow2
}

void acorn::Sema::check_modifiers_for_composite(Decl* decl, const char* composite_type_str) {
    check_modifier_incompatibilities(decl);
    if (decl->has_modifier(Modifier::DllImport)) {
        error(decl->get_modifier_location(Modifier::DllImport), "%s cannot have dllimport modifier", composite_type_str)
            .end_error(ErrCode::SemaCompositeCannotHaveDllImportModifier);
    }
    if (decl->has_modifier(Modifier::Native)) {
        error(decl->get_modifier_location(Modifier::Native), "%s cannot have native modifier", composite_type_str)
            .end_error(ErrCode::SemaCompositeCannotHaveNativeModifier);
    }
}

template<bool check_for_interface>
bool acorn::Sema::do_functions_match(const Func* func1, const Func* func2) {

    if (func1->is_generic() || func2->is_generic()) {
        return check_for_interface;
    }

    auto get_param_count = [](const Func* func) finline {
        if constexpr (!check_for_interface) {
            if (func->default_params_offset == -1) {
                return func->params.size();
            }
            return func->default_params_offset;
        } else {
            return func->params.size();
        }
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

    if (func1->is_constant != func2->is_constant) {
        return false;
    }

    return true;
}

void acorn::Sema::check_struct_interface_extension(Struct* structn,
                                                   Interface* interfacen,
                                                   const Struct::UnresolvedExtension& extension) {

    if (!interfacen->has_been_checked) {
        ensure_interface_checked(extension.error_loc, interfacen);
    }

    if (interfacen == context.std_error_interface) {
        if (!extension.is_dynamic) {
            error(structn, "Extension of 'std.Error' interface must be dynamic. Use '*Error' instead")
                .end_error(ErrCode::SemaErrorExtensionNotDynamic);
        }
    }

    for (auto interface_func : interfacen->functions) {

        if (!interface_func->has_checked_declaration) {
            check_function_decl(interface_func);
        }

        if (interface_func->has_errors) {
            structn->has_errors = true;
            continue;
        }

        auto funcs = structn->nspace->find_functions(interface_func->name);
        if (!funcs) {
            error(structn, "Struct missing interface function '%s'", interface_func->get_decl_string())
                .end_error(ErrCode::SemaStructMissingInterfaceFunc);
            structn->has_errors = true;
            continue;
        }

        // Making sure all the functions declarations have been checked.
        for (auto func : *funcs) {
            if (!func->has_checked_declaration) {
                if (!check_function_decl(func)) {
                    structn->has_errors = true;
                    return;
                }
            } else if (func->has_errors) {
                structn->has_errors = true;
                return;
            }
        }

        bool found_match = false;
        for (auto func : *funcs) {
            found_match = do_interface_functions_matches(interface_func, func);
            if (found_match) {
                func->mapped_interface_func = interface_func;
                func->is_dynamic = extension.is_dynamic;

                // This is done here since it needs to happen after the function's declaration
                // has been processed in order to determine if the functions match.
                if (func->default_params_offset != -1) {
                    auto err_loc = func->get_function_first_default_param_location();
                    error(err_loc, "Interface function implementations cannot have default parameter values")
                        .end_error(ErrCode::SemaFuncImplInterfaceFuncHaveDefaultParamVal);
                }
                break;
            }
        }

        if (!found_match) {
            structn->has_errors = true;
            if (funcs->size() == 1) {
                auto func = (*funcs)[0];
                logger.begin_error(func->loc,
                                   "Function does not match interface function: '%s'",
                                   interface_func->get_decl_string());
                logger.add_empty_line();
                display_interface_func_mismatch_info(interface_func, func, false, true);
                logger.end_error(ErrCode::SemaInterfaceFuncNotMatch);
            } else {
                logger.begin_error(structn->loc,
                                   "No overloaded function matches interface function: '%s'",
                                   interface_func->get_decl_string());
                logger.still_give_main_location_priority();
                logger.add_individual_underline(extension.error_loc);
                for (auto func : *funcs) {
                    logger.add_empty_line();
                    logger.add_line("Could not match: '%s'", func->get_decl_string());
                    display_interface_func_mismatch_info(interface_func, func, true, false);
                }
                logger.end_error(ErrCode::SemaInterfaceFuncNotMatch);
            }
        }
    }
}

bool acorn::Sema::do_interface_functions_matches(Func* interface_func, Func* func) {

    if (interface_func->return_type->is_not(func->return_type)) {
        return false;
    }

    if (!do_functions_match<true>(interface_func, func)) {
        return false;
    }

    // Checking to make sure raised errors match.
    //
    if (interface_func->raised_errors.size() != func->raised_errors.size()) {
        return false;
    }

    for (size_t i = 0; i < interface_func->raised_errors.size(); i++) {
        auto& interface_error = interface_func->raised_errors[i];
        auto& func_error      = func->raised_errors[i];
        if (interface_error.structn != func_error.structn) {
            return false;
        }
    }

    return true;
}

// Statements checking
//--------------------------------------

void acorn::Sema::check_node(Node* node) {
    switch (node->kind) {
    case NodeKind::Var:
        return check_variable(static_cast<Var*>(node));
    case NodeKind::IdentRef:
        return check_ident_ref(static_cast<IdentRef*>(node), nspace, nullptr, false);
    case NodeKind::DotOperator:
        return check_dot_operator(static_cast<DotOperator*>(node), false);
    case NodeKind::ReturnStmt:
        return check_return(static_cast<ReturnStmt*>(node));
    case NodeKind::IfStmt: {
        bool ignore1, ignore2;
        return check_if(static_cast<IfStmt*>(node), ignore1, ignore2);
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
    case NodeKind::RaiseStmt:
        return check_raise(static_cast<RaiseStmt*>(node));
    case NodeKind::RecoverStmt:
        return check_recover(static_cast<RecoverStmt*>(node));
    case NodeKind::StructInitializer:
        return check_struct_initializer(static_cast<StructInitializer*>(node));
    case NodeKind::This:
        return check_this(static_cast<This*>(node));
    case NodeKind::SizeOf:
        return check_sizeof(static_cast<SizeOf*>(node));
    case NodeKind::MoveObj:
        return check_moveobj(static_cast<MoveObj*>(node), false);
    case NodeKind::Ternary:
        return check_ternary(static_cast<Ternary*>(node));
    case NodeKind::TypeExpr:
        return check_type_expr(static_cast<TypeExpr*>(node));
    case NodeKind::Reflect:
        return check_reflect(static_cast<Reflect*>(node));
    default:
        acorn_fatal("check_node(): missing case");
    }
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

        if (is_incomplete_statement(stmt)) {
            if (stmt->is_expression()) {
                logger.begin_error(expand(stmt), "Incomplete statement");
            } else {
                logger.begin_error(stmt->loc, "Incomplete statement");
            }
            logger.end_error(ErrCode::SemaIncompleteStmt);
            continue;
        }

        if (stmt->is(NodeKind::Func)) {
            error(stmt, "Functions cannot be declared within another function")
                .end_error(ErrCode::SemaNoLocalFuncs);
        } else if (stmt->is(NodeKind::Struct)) {
            error(stmt, "Structs cannot be declared within a function")
                .end_error(ErrCode::SemaNoLocalStructs);
        } else {
            if (stmt->is_expression()) {
                Expr* expr = static_cast<Expr*>(stmt);
                if (expr->tryn) {
                    cur_scope->cur_try = expr->tryn;
                    check_node(expr);
                    check_try(expr->tryn, false);
                    cur_scope->cur_try = nullptr;
                    continue;
                }
            }
            check_node(stmt);
        }
    }
}

void acorn::Sema::check_return(ReturnStmt* ret) {
    cur_scope->all_paths_return = true;
    cur_scope->all_paths_branch = true;
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
            cur_func->aggr_ret_var = nullptr;
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
    } else if (ret->value) {
        create_cast(ret->value, cur_func->return_type);
    }
}

void acorn::Sema::check_if(IfStmt* ifs, bool& all_paths_return, bool& all_paths_branch) {

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
                    check_is_condition(ifs->post_variable_cond);
                }
            }
        }

    } else {
        check_node(ifs->cond);
        Expr* cond = static_cast<Expr*>(ifs->cond);
        if (cond->type) {
            check_is_condition(cond);
        }
    }

    check_scope(ifs->scope, &sem_scope);
    all_paths_return = sem_scope.all_paths_return;
    all_paths_branch = sem_scope.all_paths_branch;
    pop_scope();

    if (ifs->elseif && ifs->elseif->is(NodeKind::IfStmt)) {
        bool all_paths_return2, all_paths_branch2;
        check_if(static_cast<IfStmt*>(ifs->elseif), all_paths_return2, all_paths_branch2);
        all_paths_return &= all_paths_return2;
        all_paths_branch &= all_paths_branch2;
    } else if (ifs->elseif) {
        check_scope(static_cast<ScopeStmt*>(ifs->elseif));
    } else {
        // If an else does not exist then not all paths return/branch.
        all_paths_return = false;
        all_paths_branch = false;
    }

    cur_scope->all_paths_return = all_paths_return;

    cur_scope->found_terminal = all_paths_return;
}

void acorn::Sema::check_predicate_loop(PredicateLoopStmt* loop) {
    if (loop->cond) {
        check_node(loop->cond);
        if (loop->cond->type) {
            check_is_condition(loop->cond);
        }
    }

    if (loop->cond && loop->cond->is(NodeKind::BinOp)) {
        auto bin_op = static_cast<BinOp*>(loop->cond);

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
            error(expand(loop->cond), "Cannot assign value in a loop cond")
                .end_error(ErrCode::SemaCannotAssignValueInLoopCond);
            break;
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
        if (is_incomplete_statement(loop->inc)) {
            error(loop->inc, "Loop increment is an incomplete expression")
                .end_error(ErrCode::SemaIncIncompleteExpr);
        }
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
    if (!loop->var->is_foldable) {
        cur_func->vars_to_alloc.push_back(loop->var);
    }

    check_node(loop->container);

    if (loop->container->type) {
        if (loop->container->type->is_array() || loop->container->type->is_slice()) {
            auto arr_type = static_cast<ContainerType*>(loop->container->type);
            auto elm_type = arr_type->get_elm_type();

            if (var_type == context.auto_type) {
                loop->var->type = elm_type;
                if (!loop->var_auto_ptr) {
                    loop->var->type = elm_type;
                } else {
                    loop->var->type = type_table.get_ptr_type(elm_type);
                    loop->references_memory = true;
                }
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
                    auto container_loc = expand(loop->container);
                    auto start_loc = loop->var->loc.ptr;
                    auto error_loc = PointSourceLoc{
                        .ptr = start_loc,
                        .length = static_cast<uint16_t>(container_loc.end() - start_loc),
                        .point = start_loc,
                        .point_length = loop->var->loc.length,
                    };
                    error(error_loc, "Cannot assign type '%s' to variable '%s' with type '%s'",
                          elm_type, loop->var->name, var_type)
                        .end_error(ErrCode::SemaCannotAssignIteratorElmTypeToVar);
                }
            }
        } else if (loop->container->type->is_range()) {

            auto range_type = static_cast<RangeType*>(loop->container->type);
            auto value_type = range_type->get_value_type();

            if (var_type == context.auto_type) {
                loop->var->type = value_type;
            } else if (var_type == context.const_auto_type) {
                loop->var->type = type_table.get_const_type(value_type);
            } else {
                bool types_match = has_valid_constness(var_type, value_type) && var_type->is_ignore_const(value_type);
                if (!types_match) {
                    error(loop->var, "Expected type '%s' for variable", value_type)
                        .end_error(ErrCode::SemaCannotAssignIteratorElmTypeToVar);
                }
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
    cur_scope->all_paths_branch = true;

    if (loop_depth == 0) {
        if (loop_control->is(NodeKind::BreakStmt)) {
            error(loop_control, "break statements may only be used in loops")
                .end_error(ErrCode::SemaLoopControlOnlyInLoops);
        } else {
            error(loop_control, "continue statements may only be used in loops")
                .end_error(ErrCode::SemaLoopControlOnlyInLoops);
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

    bool all_paths_return = true, all_paths_branch = true;
    auto check_case_scope = [this, switchn, &all_paths_return, &all_paths_branch](ScopeStmt* scope) finline {
        SemScope sem_scope = push_scope();
        check_scope(scope, &sem_scope);
        all_paths_return &= sem_scope.all_paths_return;
        all_paths_branch &= sem_scope.all_paths_branch;
        pop_scope();
    };

    // TODO (maddie): This should probably not be so strict and as long as all cases are
    // handled such as when dealing with enums it should be able to determine it returns/branches.
    if (!switchn->default_scope) {
        all_paths_return = false;
        all_paths_branch = false;
    }

    struct CmpNumber {
        union {
            int64_t  value_s64;
            uint64_t value_u64;
            float    value_f32;
            double   value_f64;
        };

        enum class Kind {
            SIGNED_INT,
            UNSIGNED_INT,
            FLOAT,
            DOUBLE,
            EMPTY
        } kind;

        CmpNumber()
            : kind(Kind::EMPTY) {
        }

        static CmpNumber get(Sema& sema, Expr* value) {
            CmpNumber number;
            switch (value->get_final_type()->get_kind()) {
            case TypeKind::Int:
            case TypeKind::Int8:
            case TypeKind::Int16:
            case TypeKind::Int32:
            case TypeKind::Int64:
            case TypeKind::ISize: {
                auto ll_value = llvm::cast<llvm::ConstantInt>(sema.gen_constant(value));
                number.value_s64 = ll_value->getZExtValue();
                number.kind = Kind::SIGNED_INT;
                break;
            }
            case TypeKind::UInt8:
            case TypeKind::UInt16:
            case TypeKind::UInt32:
            case TypeKind::UInt64:
            case TypeKind::USize:
            case TypeKind::Char:
            case TypeKind::Char16: {
                auto ll_value = llvm::cast<llvm::ConstantInt>(sema.gen_constant(value));
                number.value_u64 = ll_value->getZExtValue();
                number.kind = Kind::UNSIGNED_INT;
                break;
            }
            case TypeKind::Float: {
                auto ll_value = llvm::cast<llvm::ConstantFP>(sema.gen_constant(value));
                number.value_f32 = ll_value->getValue().convertToFloat();
                number.kind = Kind::FLOAT;
                break;
            }
            case TypeKind::Double: {
                auto ll_value = llvm::cast<llvm::ConstantFP>(sema.gen_constant(value));
                number.value_f64 = ll_value->getValue().convertToDouble();
                number.kind = Kind::DOUBLE;
                break;
            }
            case TypeKind::Enum: {
                auto ll_value = llvm::cast<llvm::ConstantInt>(sema.gen_constant(value));
                auto enum_type = static_cast<EnumType*>(value->type);
                if (enum_type->get_index_type()->is_signed()) {
                    number.value_s64 = ll_value->getZExtValue();
                } else {
                    number.value_u64 = ll_value->getZExtValue();
                }
                break;
            }
            default:
                acorn_fatal("Unreachable. Not a foldable type");
            }
            return number;
        }

        static CmpNumber get_signed_int(int64_t value) {
            CmpNumber number;
            number.value_s64 = value;
            number.kind = Kind::SIGNED_INT;
            return number;
        }

        static CmpNumber get_unsigned_int(uint64_t value) {
            CmpNumber number;
            number.value_u64 = value;
            number.kind = Kind::UNSIGNED_INT;
            return number;
        }
    };

    // TODO: If the user has a crazy amount of cases it might be better to just
    // use a hashmap.
    llvm::SmallVector<CmpNumber, 64> prior_values;

    auto is_value_in_range = []
        (BinOp* range, CmpNumber value, CmpNumber cmp_lhs, CmpNumber cmp_rhs) finline {

        switch (range->op) {
        case Token::RangeEq: {
            if (value.kind == CmpNumber::Kind::SIGNED_INT) {
                return value.value_s64 >= cmp_lhs.value_s64 && value.value_s64 <= cmp_rhs.value_s64;
            } else {
                return value.value_u64 >= cmp_lhs.value_u64 && value.value_u64 <= cmp_rhs.value_u64;
            }
        }
        case Token::RangeLt: {
            if (value.kind == CmpNumber::Kind::SIGNED_INT) {
                return value.value_s64 >= cmp_lhs.value_s64 && value.value_s64 < cmp_rhs.value_s64;
            } else {
                return value.value_u64 >= cmp_lhs.value_u64 && value.value_u64 < cmp_rhs.value_u64;
            }
        }
        default:
            acorn_fatal("Unreachable. Unknown range operator");
            return false;
        }
    };

    auto report_error_for_duplicate = [this](SwitchCase scase, SwitchCase prior_case) finline{
            error(expand(scase.cond), "Duplicate value for switch")
                    .add_line([file = this->file, cond = prior_case.cond](Logger& l) {
                        l.print("Previous case at: ");
                        print_source_location(l, file, cond->loc);
                    })
                    .end_error(ErrCode::SemaDuplicateSwitchCase);
    };

    auto add_foldable_value = [this, &prior_values, switchn, &is_value_in_range, &report_error_for_duplicate]
        (CmpNumber value, SwitchCase scase) {

        for (size_t i = 0; i < prior_values.size(); i++) {
            auto& prior_case = switchn->cases[i];
            if (prior_case.cond && prior_case.cond->type && prior_case.cond->type->is_range()) {

                auto range      = static_cast<BinOp*>(prior_case.cond);
                auto range_type = static_cast<RangeType*>(prior_case.cond->type);
                auto value_type = range_type->get_value_type();

                if (!switchn->on->type->is_ignore_const(value_type)) {
                    // They are not the same type is we do not want to check for duplicates.
                    continue;
                } else if (!prior_case.cond->is_foldable) {
                    // We can only check for duplicates if the range is foldable.
                    continue;
                }

                auto ll_int_type = llvm::Type::getIntNTy(context.get_ll_context(), value_type->get_number_of_bits());

                auto cmp_lhs = CmpNumber::get(*this, range->lhs);
                auto cmp_rhs = CmpNumber::get(*this, range->rhs);

                if (is_value_in_range(range, value, cmp_lhs, cmp_rhs)) {
                    report_error_for_duplicate(scase, prior_case);
                    break;
                }
            } else {
#define cmp(v)                                      \
if (prior_value.value_s64 == value.value_s64) {     \
    report_error_for_duplicate(scase, prior_case);  \
}
                auto& prior_value = prior_values[i];

                if (prior_value.kind == CmpNumber::Kind::EMPTY) {
                    continue;
                }

                switch (prior_value.kind) {
                case CmpNumber::Kind::SIGNED_INT:   cmp(value_s64); break;
                case CmpNumber::Kind::UNSIGNED_INT: cmp(value_u64); break;
                case CmpNumber::Kind::FLOAT:        cmp(value_f32); break;
                case CmpNumber::Kind::DOUBLE:       cmp(value_f64); break;
                default:
                    acorn_fatal("unreachable");
                }
            }
#undef cmp
        }

        prior_values.push_back(value);
    };

    auto check_for_range_duplicates = [this, switchn, &prior_values, is_value_in_range, &report_error_for_duplicate]
        (SwitchCase scase, Type* value_type) finline {

        auto report_error = [this, scase](SwitchCase prior_case) finline {
            error(expand(scase.cond), "Duplicate value for switch")
                    .add_line([file = this->file, cond = prior_case.cond](Logger& l) {
                        l.print("Previous case at: ");
                        print_source_location(l, file, cond->loc);
                    })
                    .end_error(ErrCode::SemaDuplicateSwitchCase);
        };

        auto range = static_cast<BinOp*>(scase.cond);
        uint64_t total_range_values = get_total_number_of_values_in_range(range);

        if (total_range_values > 64) {
            // If there are too many values we want to use an if chain
            // so as to not explode the compiler. The if version of the
            // switch can then check if the value is between two values.
            switchn->all_conds_foldable = false;
        }

        auto lhs = CmpNumber::get(*this, range->lhs);
        auto rhs = CmpNumber::get(*this, range->rhs);

        for (size_t i = 0; i < prior_values.size(); i++) {
            auto& prior_case = switchn->cases[i];

            if (prior_case.cond && prior_case.cond->type->is_range()) {
                // TODO: This logic could be optimized to store the start and end into the ll_values
                // and then check ranges that way.

                auto cmp_range      = static_cast<BinOp*>(prior_case.cond);
                auto cmp_range_type = static_cast<RangeType*>(prior_case.cond->type);
                auto cmp_value_type = cmp_range_type->get_value_type();

                if (!switchn->on->type->is_ignore_const(cmp_value_type)) {
                    // They are not the same type is we do not want to check for duplicates.
                    continue;
                } else if (!prior_case.cond->is_foldable) {
                    // We can only check for duplicates if the range is foldable.
                    continue;
                }

                auto cmp_lhs = CmpNumber::get(*this, cmp_range->lhs);
                auto cmp_rhs = CmpNumber::get(*this, cmp_range->rhs);

                bool found_error = false;
                switch (cmp_range->op) {
                case Token::RangeEq: {

                    // end1 >= start2 and start1 <= end2
                    // or
                    // end2 >= start1 and start2 <= end1
                    if (rhs.kind == CmpNumber::Kind::SIGNED_INT) {
                        if (rhs.value_s64 >= cmp_lhs.value_s64 && lhs.value_s64 <= cmp_rhs.value_s64) {
                            report_error_for_duplicate(scase, prior_case);
                            found_error = true;
                        }
                    } else {
                        if (rhs.value_u64 >= cmp_lhs.value_u64 && lhs.value_u64 <= cmp_rhs.value_u64) {
                            report_error_for_duplicate(scase, prior_case);
                            found_error = true;
                        }
                    }

                    break;
                }
                case Token::RangeLt: {

                    // end1 > start2 and start1 < end2
                    // or
                    // end2 > start1 and start2 < end1
                    if (rhs.kind == CmpNumber::Kind::SIGNED_INT) {
                        if (rhs.value_s64 > cmp_lhs.value_s64 && lhs.value_s64 < cmp_rhs.value_s64) {
                            report_error_for_duplicate(scase, prior_case);
                            found_error = true;
                        }
                    } else {
                        if (rhs.value_u64 > cmp_lhs.value_u64 && lhs.value_u64 < cmp_rhs.value_u64) {
                            report_error_for_duplicate(scase, prior_case);
                            found_error = true;
                        }
                    }

                    break;
                }
                default:
                    acorn_fatal("Unreachable. Unknown range operator");
                    break;
                }

                if (found_error) {
                    break;
                }
            } else {
                auto prior_value = prior_values[i];
                if (prior_value.kind == CmpNumber::Kind::EMPTY) {
                    continue;
                }

                if (is_value_in_range(range, prior_value, lhs, rhs)) {
                    report_error_for_duplicate(scase, prior_case);
                    break;
                }
            }
        }
    };


    Enum* switch_on_enum = nullptr;
    if (switchn->on->type->is_enum()) {
        auto enum_type = static_cast<EnumType*>(switchn->on->type);
        switch_on_enum = enum_type->get_enum();
    }

    auto find_ident_refrence_enum = [switch_on_enum](IdentRef* ref) finline {
        auto itr = std::ranges::find_if(switch_on_enum->values, [ref](const Enum::Value& value){
            return value.name == ref->ident;
        });
        return itr;
    };

    size_t case_count = 0;
    for (SwitchCase scase : switchn->cases) {
        if (scase.cond) {
            bool cond_foldable = false;
            if (switch_on_enum && scase.cond->is(NodeKind::IdentRef)) {
                auto ref = static_cast<IdentRef*>(scase.cond);

                // First check if the identifier refers to an identifier in the enum.
                auto enum_value = find_ident_refrence_enum(ref);
                if (enum_value != switch_on_enum->values.end()) {
                    ref->set_enum_value_ref(enum_value);
                    scase.cond->type = switchn->on->type;
                    cond_foldable = true;
                } else {
                    check_ident_ref(ref, nspace, nullptr, false);
                    cond_foldable = ref->is_foldable || ref->is_enum_value_ref();
                }
            } else if (switch_on_enum && scase.cond->is(NodeKind::BinOp)) {
                auto bin_op = static_cast<BinOp*>(scase.cond);

                Enum::Value* lhs_enum_value = nullptr;
                Enum::Value* rhs_enum_value = nullptr;

                if (bin_op->op == Token::RangeEq || bin_op->op == Token::RangeLt) {

                    if (bin_op->lhs->is(NodeKind::IdentRef)) {
                        auto ref = static_cast<IdentRef*>(bin_op->lhs);
                        lhs_enum_value = find_ident_refrence_enum(ref);
                    }

                    if (bin_op->rhs->is(NodeKind::IdentRef)) {
                        auto ref = static_cast<IdentRef*>(bin_op->rhs);
                        rhs_enum_value = find_ident_refrence_enum(ref);
                    }

                    if (lhs_enum_value && rhs_enum_value) {

                        auto lhs_ref = static_cast<IdentRef*>(bin_op->lhs);
                        auto rhs_ref = static_cast<IdentRef*>(bin_op->rhs);

                        lhs_ref->set_enum_value_ref(lhs_enum_value);
                        rhs_ref->set_enum_value_ref(rhs_enum_value);

                        bin_op->lhs->type = switch_on_enum->enum_type;
                        bin_op->rhs->type = switch_on_enum->enum_type;
                        bin_op->type = type_table.get_range_type(switch_on_enum->enum_type);
                        cond_foldable = true;

                        check_constant_range_for_bigger_lhs(bin_op, switch_on_enum->enum_type);

                    } else if (lhs_enum_value) {
                        check_node(bin_op->rhs);
                        if (bin_op->rhs->type && bin_op->rhs->type->is(switch_on_enum->enum_type)) {

                            auto ref = static_cast<IdentRef*>(bin_op->lhs);
                            ref->set_enum_value_ref(lhs_enum_value);

                            bin_op->type = type_table.get_range_type(switch_on_enum->enum_type);
                            bin_op->lhs->type = switch_on_enum->enum_type;
                            bin_op->rhs->type = switch_on_enum->enum_type;
                            cond_foldable = bin_op->rhs->is_foldable;

                            check_constant_range_for_bigger_lhs(bin_op, switch_on_enum->enum_type);

                        } else {
                            report_binary_op_cannot_apply(bin_op, bin_op->rhs);
                        }
                    } else if (rhs_enum_value) {
                        check_node(bin_op->lhs);
                        if (bin_op->lhs->type && bin_op->lhs->type->is(switch_on_enum->enum_type)) {

                            auto ref = static_cast<IdentRef*>(bin_op->rhs);
                            ref->set_enum_value_ref(rhs_enum_value);

                            bin_op->type = type_table.get_range_type(switch_on_enum->enum_type);
                            bin_op->lhs->type = switch_on_enum->enum_type;
                            bin_op->rhs->type = switch_on_enum->enum_type;
                            cond_foldable = bin_op->rhs->is_foldable;

                            check_constant_range_for_bigger_lhs(bin_op, switch_on_enum->enum_type);

                        } else {
                            report_binary_op_cannot_apply(bin_op, bin_op->lhs);
                        }
                    } else {
                        check_binary_op(bin_op);
                        cond_foldable = scase.cond->is_foldable;
                    }
                } else {
                    // Not a range so cannot attempt to reference the enum.
                    check_binary_op(bin_op);
                    cond_foldable = scase.cond->is_foldable;
                }
            } else {
                check_node(scase.cond);
                cond_foldable = scase.cond->is_foldable;
                if (scase.cond->is(NodeKind::DotOperator)) {
                    auto dot = static_cast<DotOperator*>(scase.cond);
                    cond_foldable = dot->is_enum_value_ref();
                }
            }

            if (scase.cond->type) {
                bool is_bool_type = scase.cond->type->is_ignore_const(context.bool_type);

                if (scase.cond->type->is_range()) {

                    auto range_type = static_cast<RangeType*>(scase.cond->type);
                    auto value_type = range_type->get_value_type();

                    if (!switchn->on->type->is_ignore_const(value_type)) {
                        error(expand(scase.cond), "Cannot compare case type '%s' to range value type '%s'",
                              scase.cond->type, value_type)
                            .end_error(ErrCode::SemaCannotCompareCaseType);
                        prior_values.push_back({}); // Need to still add empty to keep indices correct.
                    } else if (cond_foldable) {
                        check_for_range_duplicates(scase, value_type);
                    } else {
                        // TODO: We should allow for this?
                        error(expand(scase.cond), "Ranges in switch statements must be able to be determined at compile time")
                            .end_error(ErrCode::SemaSwitchRangeNotFoldable);
                    }

                    prior_values.push_back({});
                    // End of range case.
                } else if (!is_bool_type &&
                    !switchn->on->type->is_ignore_const(scase.cond->type)) {

                    error(expand(scase.cond), "Cannot compare case type '%s' to type '%s'",
                          scase.cond->type, switchn->on->type)
                        .end_error(ErrCode::SemaCannotCompareCaseType);
                    prior_values.push_back({}); // Need to still add empty to keep indices correct.

                } else if (cond_foldable) {
                    add_foldable_value(CmpNumber::get(*this, scase.cond), scase);
                } else {
                    prior_values.push_back({}); // Need to still add empty to keep indices correct.
                }

                if (scase.cond->is(NodeKind::Bool)) {
                    auto bool_cond = static_cast<Bool*>(scase.cond);
                    error(scase.cond, "Cannot compare to %s", bool_cond->value ? "true" : "false")
                        .end_error(ErrCode::SemaCaseCannotBeBoolLiteral);
                }

                if (!cond_foldable || is_bool_type) {
                    switchn->all_conds_foldable = false;
                }
            } else {
                prior_values.push_back({}); // Need to still add empty to keep indices correct.
            }
        } else if (case_count != switchn->cases.size() - 1) {
            error(scase.scope, "Default case must go at end of switch")
                .end_error(ErrCode::SemaDefaultCaseMustGoLast);
            prior_values.push_back({}); // Need to still add empty to keep indices correct.
        }
        check_case_scope(scase.scope);

        ++case_count;
    }

    cur_scope->all_paths_return = all_paths_return;
    cur_scope->all_paths_branch = all_paths_branch;
    cur_scope->found_terminal   = all_paths_return;
}

void acorn::Sema::check_raise(RaiseStmt* raise) {
    cur_scope->all_paths_return = true;
    cur_scope->all_paths_branch = true;
    cur_scope->found_terminal = true;

    ++cur_func->num_returns;

    if (context.should_stand_alone()) {
        error(raise, "Cannot use raise when stand alone is enabled")
            .end_error(ErrCode::SemaCannotRaiseWhenStandAlone);
        return;
    }

    //if (cur_func == context.get_main_function()) {
    //    error(raise, "Cannot use raise in the main function")
    //        .end_error(ErrCode::SemaCannotRaiseInMainFunction);
    //    return;
    //}

    if (raise->expr->is_not(NodeKind::StructInitializer)) {
        error(raise->expr, "Expected error struct initializer")
            .end_error(ErrCode::SemaWrongRaiseExpr);
        return;
    }

    auto initializer = static_cast<StructInitializer*>(raise->expr);
    check_struct_initializer(initializer);

    yield_if(initializer);

    const Struct::InterfaceExtension* extension =
        initializer->structn->find_interface_extension(context.error_interface_identifier);

    if (!extension || extension->interfacen != context.std_error_interface) {
        error(expand(raise), "Expected raised struct '%s' to extend 'std.Error' interface", initializer->structn->name)
            .end_error(ErrCode::SemaRaiseStructNoErrorInterface);
        return;
    }

    if (!initializer->structn->aborts_error) {
        bool found_raised_error = false;
        for (auto& raised_error : cur_func->raised_errors) {
            if (raised_error.structn == initializer->structn) {
                found_raised_error = true;
                break;
            }
        }
        if (!found_raised_error) {
            error(expand(raise), "Function '%s' must specify that it raises '%s' in its definition",
                  cur_func->name, initializer->structn->name)
                .end_error(ErrCode::SemaFuncDoesNotRaiseErrorInDef);
        }
    }

    raise->raised_error = initializer->structn;
}

void acorn::Sema::check_try(Try* tryn, bool assigns) {

    yield_if(tryn->caught_expr);

    if (!cur_scope) {
        error(tryn, "Cannot catch errors at global scope")
            .end_error(ErrCode::SemaCannotCatchErrorsAtGlobalScope);
        return;
    }

    if (context.should_stand_alone()) {
        error(tryn, "Cannot use try when stand alone is enabled")
            .end_error(ErrCode::SemaCannotTryWhenStandAlone);
        return;
    }

    if (tryn->caught_errors.empty()) {
        error(expand(tryn), "Expression does not raise error")
            .end_error(ErrCode::SemaExprDoesNotRaiseErrors);
    }

    if (tryn->catch_scope) {
        SemScope sem_scope = push_scope();
        Try* prev_catch_scope_try = catch_scope_try;
        catch_scope_try = tryn;

        if (tryn->caught_var) {
            add_variable_to_local_scope(tryn->caught_var);

            tryn->caught_var->parsed_type = type_table.get_ptr_type(context.std_error_interface->interface_type);
            tryn->caught_var->type = type_table.get_ptr_type(context.std_error_interface->interface_type);
        }

        check_scope(tryn->catch_scope, &sem_scope);

        if (assigns && !cur_scope->all_paths_branch) {
            error(expand(tryn), "Assignment lacks value when error is caught")
                .add_line("Note: Either all paths must branch or you must use 'recover' with a value")
                .end_error(ErrCode::SemaTryCatchAssignValueNotAssigned);
        }

        pop_scope();
        catch_scope_try = prev_catch_scope_try;
    } else {

        // See if all the errors get passed along or not.
        bool passes_at_least_one_error = false;
        bool passes_all_errors = true;
        for (Struct* caught_struct : tryn->caught_errors) {
            auto itr = std::ranges::find_if(cur_func->raised_errors, [caught_struct](auto& e) {
                return e.structn == caught_struct;
            });
            if (itr != cur_func->raised_errors.end()) {
                passes_at_least_one_error = true;
            } else {
                passes_all_errors = false;
            }
        }

        if (passes_at_least_one_error && !passes_all_errors) {
            error(expand(tryn), "Must catch error because only some of the errors are caught by this function")
                .add_line("Note: This would result in ambiguity for if the errors should be aborted or not")
                .end_error(ErrCode::SemaOnlyPartlyPassesAlongError);
        }

        tryn->passes_error_along = passes_all_errors;
        if (passes_all_errors) {
            ++cur_func->num_returns;
        }
    }

    tryn->type = tryn->caught_expr->type;

}

void acorn::Sema::check_recover(RecoverStmt* recover) {

    check_and_verify_type(recover->value);

    if (!catch_scope_try) {
        error(recover, "Can only use 'recover' in catch scopes")
            .end_error(ErrCode::SemaRecoverStmtNotInCatchScope);
        return;
    }

    if (!catch_scope_try->catch_recoveree) {
        error(recover, "The try expression does not assign in order to recover")
            .end_error(ErrCode::SemaRecoverStmtNoRecoveree);
        return;
    }

    cur_scope->all_paths_branch = true;
    cur_scope->found_terminal = true;
}


// Expression checking
//--------------------------------------

void acorn::Sema::check_binary_op(BinOp* bin_op) {

    Expr* lhs = bin_op->lhs;
    Expr* rhs = bin_op->rhs;

    auto get_number_type = [this, bin_op](bool enforce_lhs, Type* lhs_type, Type* rhs_type) finline -> Type* {
        // TODO: If one of the types is a float and the other an integer
        //       we may want the bitwidth to be less than or equal to the
        //       size of the float for the integer.

        if (lhs_type->is_float()) {
            if (rhs_type->is_float()) {
                uint32_t lbits = rhs_type->get_number_of_bits();
                uint32_t rbits = lhs_type->get_number_of_bits();
                return lbits > rbits ? lhs_type : rhs_type;
            } else if (rhs_type->is_integer()) {
                return lhs_type->remove_all_const();
            }
            return nullptr;
        } else if (rhs_type->is_float()) {
            if (lhs_type->is_float()) {
                uint32_t lbits = rhs_type->get_number_of_bits();
                uint32_t rbits = lhs_type->get_number_of_bits();
                return lbits > rbits ? lhs_type : rhs_type;
            } else if (lhs_type->is_integer()) {
                return rhs_type->remove_all_const();
            }
            return nullptr;
        }
        return get_integer_type_for_binary_op(enforce_lhs, bin_op, lhs_type, rhs_type);
    };

    auto get_add_sub_mul_type = [=, this](bool enforce_lhs, Type* lhs_type, Type* rhs_type) finline->Type* {
        // valid pointer arithmetic cases:
        //
        // ptr + int
        // int + ptr
        //
        // ptr - int
        // ptr - ptr

        if (lhs_type->is_pointer() || lhs_type->is_array()) {
            // Pointer arithmetic
            if (bin_op->op == '+' && !rhs_type->is_integer()) {
                report_binary_op_mistmatch_types(bin_op);
                return nullptr;
            } else if (!(rhs_type->is_integer() || rhs_type->is_ignore_const(lhs_type))) {
                report_binary_op_mistmatch_types(bin_op);
                return nullptr;
            }

            if (lhs_type->is_pointer() && rhs_type->is_pointer()) {
                return context.isize_type;
            }
            if (lhs_type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(lhs_type);
                return type_table.get_ptr_type(arr_type->get_elm_type());
            }
            return lhs_type;
        } else if (!enforce_lhs && rhs_type->is_pointer() || rhs_type->is_array()) {
            // Pointer arithmetic
            if (bin_op->op == '+' && !lhs_type->is_integer()) {
                report_binary_op_mistmatch_types(bin_op);
                return nullptr;
            } else if (bin_op->op == '-') {
                // ptr - ptr   case would have already been handled.
                report_binary_op_mistmatch_types(bin_op);
                return nullptr;
            }

            if (rhs_type->is_array()) {
                auto arr_type = static_cast<ArrayType*>(rhs_type);
                return type_table.get_ptr_type(arr_type->get_elm_type());
            }
            return rhs_type;
        }

        if (!lhs_type->is_number()) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return nullptr;
        }
        if (!rhs_type->is_number()) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return nullptr;
        }
        if (Type* result_type = get_number_type(enforce_lhs, lhs_type, rhs_type)) {
            return result_type;
        }

        report_binary_op_mistmatch_types(bin_op);
        return nullptr;
    };

    auto get_div_mod_type = [=, this](bool enforce_lhs, Type* lhs_type, Type* rhs_type) finline -> Type* {
        if (!lhs_type->is_number()) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return nullptr;
        }
        if (!rhs_type->is_number()) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return nullptr;
        }

        check_division_by_zero(bin_op, rhs);

        if (Type* result_type = get_number_type(enforce_lhs, lhs_type, rhs_type)) {
            return result_type;
        }

        report_binary_op_mistmatch_types(bin_op);
        return nullptr;
    };

    auto get_logical_bitwise_type = [=, this](bool enforce_lhs, Type* lhs_type, Type* rhs_type) finline -> Type* {

        if (!(lhs_type->is_integer() || lhs_type->is_bool())) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return nullptr;
        }
        if (!(rhs_type->is_integer() || rhs_type->is_bool())) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return nullptr;
        }
        if (Type* result_type = get_integer_type_for_binary_op(enforce_lhs, bin_op, lhs_type, rhs_type)) {
            return result_type;
        }

        report_binary_op_mistmatch_types(bin_op);
        return nullptr;
    };

    auto get_shifts_type = [=, this](bool enforce_lhs, Type* lhs_type, Type* rhs_type) finline -> Type* {
        if (!lhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return nullptr;
        }
        if (!rhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return nullptr;
        }
        if (Type* result_type = get_integer_type_for_binary_op(enforce_lhs, bin_op, lhs_type, rhs_type)) {
            return result_type;
        }

        report_binary_op_mistmatch_types(bin_op);
        return nullptr;
    };

    // Need to handle try here because assignment operators can
    // have try statements.
    auto cleanup_try = [this, bin_op]() finline {
        if (bin_op->rhs->tryn)
            cur_scope->cur_try = nullptr;
    };

    if (bin_op->rhs->tryn) {
        cur_scope->cur_try = bin_op->rhs->tryn;
    }

    check_node(lhs);
    if (!lhs->type) {
        return cleanup_try();
    }
    if (bin_op->op == '=' && rhs->is(NodeKind::MoveObj)) {
        check_moveobj(static_cast<MoveObj*>(rhs), true);
        if (!rhs->type) {
            return cleanup_try();
        }
    } else {
        check_node(rhs);
        if (!rhs->type) {
            return cleanup_try();
        }
    }

    if (!lhs->is_foldable || !rhs->is_foldable) {
        bin_op->is_foldable = false;
    }

    EnumType* lhs_enum_type = nullptr, *rhs_enum_type = nullptr;
    if (lhs->type->is_enum()) {
        auto enum_type = static_cast<EnumType*>(lhs->type);
        lhs_enum_type = enum_type;
    } else if (auto enum_type = lhs->type->get_container_enum_type()) {
        lhs_enum_type = enum_type;
    }
    if (rhs->type->is_enum()) {
        auto enum_type = static_cast<EnumType*>(rhs->type);
        rhs_enum_type = enum_type;
    } else if (auto enum_type = rhs->type->get_container_enum_type()) {
        rhs_enum_type = enum_type;
    }

    if (lhs_enum_type || rhs_enum_type) {
        check_binary_op_for_enums(bin_op, lhs_enum_type, rhs_enum_type);
        return cleanup_try();
    }

    if (lhs->trivially_reassignable && rhs->trivially_reassignable) {
        bin_op->trivially_reassignable = true;
    }

    Type* lhs_type = lhs->type;
    Type* rhs_type = rhs->type;

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

        if (!check_modifiable(bin_op->lhs, bin_op)) {
            return cleanup_try();
        }

        switch (bin_op->op) {
        case '=': {
            if (!is_assignable_to(bin_op->lhs->type, bin_op->rhs)) {
                bool is_assignable_to = false;

                if (bin_op->rhs->is(NodeKind::FuncCall)) {
                    FuncCall* call = static_cast<FuncCall*>(bin_op->rhs);
                    if (may_implicitly_convert_return_ptr(bin_op->lhs->type, call)) {
                        is_assignable_to = true;
                        call->implicitly_converts_return = true;
                    }
                }

                if (!is_assignable_to) {
                    error(expand(bin_op), get_type_mismatch_error(bin_op->lhs->type, bin_op->rhs).c_str())
                        .end_error(ErrCode::SemaVariableTypeMismatch);
                    return cleanup_try();
                }
            }

            if (lhs_type->is_array()) {
                error(expand(bin_op), "Cannot reassign to arrays")
                    .end_error(ErrCode::SemaCannotRassignToArrays);
            }

            break;
        }
        case Token::AddEq: case Token::SubEq: case Token::MulEq: {
            if (!get_add_sub_mul_type(true, lhs_type, rhs_type))
                return cleanup_try();
            break;
        }
        case Token::DivEq: case Token::ModEq: {
            if (!get_div_mod_type(true, lhs_type, rhs_type))
                return cleanup_try();
            break;
        }
        case Token::AndEq: case Token::OrEq: case Token::CaretEq: {
            if (!get_logical_bitwise_type(true, lhs_type, rhs_type))
                return cleanup_try();
            break;
        }
        case Token::TildeEq: {
            if (!lhs_type->is_integer()) {
                report_binary_op_cannot_apply(bin_op, lhs);
                return cleanup_try();
            }
            if (!rhs_type->is_integer()) {
                report_binary_op_cannot_apply(bin_op, rhs);
                return cleanup_try();
            }
            if (!get_integer_type_for_binary_op(true, bin_op, lhs_type, rhs_type)) {
                report_binary_op_mistmatch_types(bin_op);
                return cleanup_try();
            }
            break;
        case Token::LtLtEq: case Token::GtGtEq: {
            if (!get_shifts_type(true, lhs_type, rhs_type)) {
                return cleanup_try();
            }
            break;
        }
        }
        }

        if (!lhs_type->is_pointer()) {
            create_cast(bin_op->rhs, lhs_type);
        }
        bin_op->type = lhs_type;

        if (bin_op->rhs->tryn) {
            check_try(bin_op->rhs->tryn, true);
            cleanup_try();
        }

        break;
    }
    case '+': case '-': case '*': {
        auto result_type = get_add_sub_mul_type(false, lhs_type, rhs_type);
        if (!result_type) return;

        // Create needed casts if not pointer arithmetic.
        if (!lhs_type->is_pointer() && !rhs_type->is_pointer() &&
            !lhs_type->is_array()   && !rhs_type->is_array()) {
            create_cast(lhs, result_type);
            create_cast(rhs, result_type);
        }

        bin_op->type = result_type;

        break;
    }
    case '/': case '%': {
        auto result_type = get_div_mod_type(false, lhs_type, rhs_type);
        if (!result_type) return;
        bin_op->type = result_type;
        create_cast(lhs, result_type);
        create_cast(rhs, result_type);
        break;
    }
    case '^': case '&': case '|': {
        auto result_type = get_logical_bitwise_type(false, lhs_type, rhs_type);
        if (!result_type) return;
        bin_op->type = result_type;
        create_cast(lhs, result_type);
        create_cast(rhs, result_type);
        break;
    }
    case Token::LtLt: case Token::GtGt: {
        auto result_type = get_shifts_type(false, lhs_type, rhs_type);
        if (!result_type) return;
        bin_op->type = result_type;
        create_cast(lhs, result_type);
        create_cast(rhs, result_type);
        break;
    }
    case '<': case '>':
    case Token::GtEq: case Token::LtEq:
    case Token::EqEq: case Token::ExEq: {
        if (!lhs_type->is_comparable()) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return;
        }
        if (!rhs_type->is_comparable()) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return;
        }

        // This is a hack, get_integer_type was not meant to work with pointers
        // but since it's first check is if the types are equal ignoring const
        // it can still work with pointers.
        auto result_type = get_integer_type_for_binary_op(false, bin_op, lhs_type, rhs_type);
        if (!(result_type ||
             (rhs_type->is_pointer() && lhs_type->get_kind() == TypeKind::Null) ||
             (lhs_type->is_pointer() && rhs_type->get_kind() == TypeKind::Null)
              )) {
            report_binary_op_mistmatch_types(bin_op);
            return;
        } else if (result_type) {
            create_cast(lhs, result_type);
            create_cast(rhs, result_type);
        }
        bin_op->type = context.bool_type;
        break;
    }
    case Token::AndAnd: case Token::OrOr: {
        if (!is_condition(lhs_type)) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return;
        }
        if (!is_condition(rhs_type)) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return;
        }

        bin_op->type = context.bool_type;
        break;
    }
    case Token::RangeEq: case Token::RangeLt: {
        if (!lhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, lhs);
            return;
        }
        if (!rhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, rhs);
            return;
        }

        auto result_type = get_integer_type_for_binary_op(false, bin_op, lhs_type, rhs_type);

        if (!result_type) {
            report_binary_op_mistmatch_types(bin_op);
            return;
        }

        create_cast(lhs, result_type);
        create_cast(rhs, result_type);

        check_constant_range_for_bigger_lhs(bin_op, result_type);

        bin_op->type = type_table.get_range_type(result_type);
        break;
    }
    default:
        acorn_fatal("check_binary_op(): Failed to implement case");
        break;
    }
}

void acorn::Sema::check_binary_op_for_enums(BinOp* bin_op, EnumType* lhs_enum_type, EnumType* rhs_enum_type) {
    Type* lhs_type = bin_op->lhs->type;
    Type* rhs_type = bin_op->rhs->type;

    if (lhs_enum_type) {
        lhs_type = lhs_enum_type->get_values_type();
    }
    if (rhs_enum_type) {
        rhs_type = rhs_enum_type->get_values_type();
    }

    switch (bin_op->op) {
    case '=': {

        if (!check_modifiable(bin_op->lhs, bin_op)) {
            return;
        }

        // Make sure the destination is an enum type.
        if (!lhs_enum_type || !rhs_enum_type) {
            error(expand(bin_op->lhs), get_type_mismatch_error(bin_op->lhs->type, bin_op->rhs).c_str())
                .end_error(ErrCode::SemaVariableTypeMismatch);
        }

        bin_op->type = type_table.get_enum_container_type(lhs_enum_type);
        break;
    }
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
        if (!lhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, bin_op->lhs);
            return;
        }
        if (!rhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, bin_op->rhs);
            return;
        }

        // Make sure the destination is an enum type.
        if (!lhs_enum_type) {
            error(expand(bin_op->lhs), get_type_mismatch_error(bin_op->lhs->type, bin_op->rhs).c_str())
                .end_error(ErrCode::SemaVariableTypeMismatch);
        }

        if (!check_modifiable(bin_op->lhs, bin_op)) {
            return;
        }
        bin_op->type = type_table.get_enum_container_type(lhs_enum_type);

        break;
    }
    case '+': case '-': case '*':
    case '^': case '&': case '|':
    case Token::LtLt: case Token::GtGt: {
        if (!lhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, bin_op->lhs);
            return;
        }
        if (!rhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, bin_op->rhs);
            return;
        }

        bin_op->type = type_table.get_enum_container_type(lhs_enum_type);
        break;
    }
    case '/': case '%': {
        if (!lhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, bin_op->lhs);
            return;
        }
        if (!rhs_type->is_integer()) {
            report_binary_op_cannot_apply(bin_op, bin_op->rhs);
            return;
        }

        check_division_by_zero(bin_op, bin_op->rhs);

        bin_op->type = type_table.get_enum_container_type(lhs_enum_type);
        break;
    }
    case '<': case '>':
    case Token::GtEq: case Token::LtEq:
    case Token::EqEq: case Token::ExEq: {

        bool lhs_is_enum = bin_op->lhs->type->is_enum(),
             rhs_is_enum = bin_op->rhs->type->is_enum();
        if (!lhs_is_enum) {
            if (!lhs_type->is_integer()) {
                report_binary_op_cannot_apply(bin_op, bin_op->lhs);
                return;
            }
        }
        if (!rhs_is_enum) {
            if (!rhs_type->is_integer()) {
                report_binary_op_cannot_apply(bin_op, bin_op->rhs);
                return;
            }
        }

        // Make sure they are either comparing the container types to each
        // other or they are comparing the enum types
        if (lhs_is_enum ^ rhs_is_enum) {
            report_binary_op_mistmatch_types(bin_op);
            return;
        }

        bin_op->type = context.bool_type;
        break;
    }
    case Token::AndAnd: case Token::OrOr: {
        report_binary_op_cannot_apply(bin_op, bin_op->lhs);
        return;
    }
    case Token::RangeEq: case Token::RangeLt: {

        lhs_type = bin_op->lhs->type->remove_all_const();
        rhs_type = bin_op->rhs->type->remove_all_const();

        if (!(lhs_type->is_integer() || lhs_type->is_enum())) {
            report_binary_op_cannot_apply(bin_op, bin_op->lhs);
            return;
        }
        if (!(rhs_type->is_integer() || rhs_type->is_enum())) {
            report_binary_op_cannot_apply(bin_op, bin_op->rhs);
            return;
        }

        if (lhs_type->is_not(rhs_type)) {
            // One of them is an enum.
            report_binary_op_mistmatch_types(bin_op);
            return;
        }

        check_constant_range_for_bigger_lhs(bin_op, lhs_type);

        bin_op->type = type_table.get_range_type(lhs_type);
        break;
    }
    default:
        acorn_fatal("checkcheck_binary_op_for_enums_binary_op(): Failed to implement case");
        break;
    }
}

void acorn::Sema::check_constant_range_for_bigger_lhs(BinOp* bin_op, Type* result_type) {
    if (!bin_op->is_foldable) {
        return;
    }

    auto ll_lhs = llvm::cast<llvm::ConstantInt>(gen_constant(bin_op->lhs));
    auto ll_rhs = llvm::cast<llvm::ConstantInt>(gen_constant(bin_op->rhs));

    if (ll_lhs && ll_rhs) {

        auto lhs_value = ll_lhs->getZExtValue();
        auto rhs_value = ll_rhs->getZExtValue();

        auto report_lhs_bigger = [this, bin_op]() finline{
            auto op_str = token_kind_to_string(bin_op->op, context);
            error(expand(bin_op), "Operator %s expects right hand side to be bigger", op_str)
                .end_error(ErrCode::SemaRangeExpectsRightSideBigger);
        };

        if (result_type->is_signed()) {
            if (static_cast<int64_t>(lhs_value) > static_cast<int64_t>(rhs_value)) {
                report_lhs_bigger();
            }
        } else {
            if (lhs_value > rhs_value) {
                report_lhs_bigger();
            }
        }
    }
}

acorn::Type* acorn::Sema::get_integer_type_for_binary_op(bool enforce_lhs,
                                                         BinOp* bin_op,
                                                         Type* lhs_type,
                                                         Type* rhs_type) const {
    if (rhs_type->is_ignore_const(lhs_type)) {
        return lhs_type->remove_all_const();
    }

    // Allow numeric operations on integer types when one of them is
    // an integer literal but without an explicit type.

    if (lhs_type->is_integer() && rhs_type->is_integer()) {
        if (bin_op->rhs->trivially_reassignable) {
            return lhs_type->remove_all_const();
        }
        if (!enforce_lhs && bin_op->lhs->trivially_reassignable) {
            return rhs_type->remove_all_const();
        }
    }

    return nullptr;
}

void acorn::Sema::check_unary_op(UnaryOp* unary_op) {
    Expr* expr = unary_op->expr;
    check_and_verify_type(expr);

    auto error_no_applies = [this, unary_op, expr]() finline -> void {
        error(expand(unary_op), "Operator %s cannot apply to type '%s'",
              token_kind_to_string(unary_op->op, context), expr->type)
            .end_error(ErrCode::SemaUnaryOpTypeCannotApply);
    };

    if (expr->trivially_reassignable) {
        unary_op->trivially_reassignable = true;
    }

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

        Type* elm_type = expr->type;
        if (is_readonly_field_without_access(expr)) {
            if (!elm_type->is_const()) {
                elm_type = type_table.get_const_type(elm_type);
            }
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

        auto ptr_type = static_cast<PointerType*>(expr->type);
        if (ptr_type->get_elm_type()->is_interface()) {
            error(expand(unary_op), "Cannot dereference pointer to interface")
                .end_error(ErrCode::SemaCannotDereferencePtrToInterface);
            return;
        }

        unary_op->type = ptr_type->get_elm_type();
        unary_op->is_foldable = false;
        break;
    }
    case Token::AddAdd: case Token::SubSub:
    case Token::PostAddAdd: case Token::PostSubSub: {

        if (!check_modifiable(expr, unary_op, false)) {
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

template<bool is_spell_checking>
void acorn::Sema::check_ident_ref(IdentRef* ref,
                                  Namespace* search_nspace,
                                  StructType* search_struct_type,
                                  bool is_for_call,
                                  bool is_dot_op_site) {

    ErrorSpellChecker spell_checker(context.should_show_spell_checking());

    bool search_relative = search_nspace == nspace;
    bool search_nested = true;

    if (ref->relative_enforcement != IdentRef::RelativeEnforcement::None) {
        if (ref->relative_enforcement == IdentRef::RelativeEnforcement::File) {
            // search file first BUT NOT nested scopes then search module.
            search_nested = false;
            search_nspace = &file->get_module();
        } else {
            // search explicitly in module.
            search_nspace = &file->get_module();
            search_relative = false;
        }
    }

    if (is_comptime_if_cond) {
        // TODO (maddie): maybe this should check the global variable list if it fails to find
        // a variable

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

        spell_checker.add_searches(context.get_universal_constants());

        auto& logger = error(expand(ref), "Could not find identifier '%s'", ref->ident);
        spell_checker.search(logger, ref->ident);
        logger.end_error(ErrCode::SemaNoFindIdentRef);
        return;
    }

    auto find_function = [=, this, &spell_checker]() finline {
        if (search_relative) {
            if (cur_struct && search_nested) {
                if (auto* funcs = cur_struct->nspace->find_functions(ref->ident)) {
                    ref->set_funcs_ref(funcs);
                    return;
                }
                if constexpr (is_spell_checking) {
                    if (is_for_call) // Only suggest functions if the user is using call syntax.
                        spell_checker.add_searches(cur_struct->nspace->get_functions());
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

            if constexpr (is_spell_checking) {
                if (is_for_call) { // Only suggest functions if the user is using call syntax.
                    spell_checker.add_searches(file->get_functions());
                    for (const Namespace* nspace : file->get_static_imports()) {
                        spell_checker.add_searches(nspace->get_functions());
                    }
                }
            }
        }

        if (auto* funcs = search_nspace->find_functions(ref->ident)) {
            ref->set_funcs_ref(funcs);
        }
        if constexpr (is_spell_checking) {
            if (is_for_call) // Only suggest functions if the user is using call syntax.
                spell_checker.add_searches(search_nspace->get_functions());
        }
    };

    auto find_variable = [=, this, &spell_checker]() finline {
        if (search_relative && search_nested) {
            if (cur_scope && search_nested) {
                if (auto* var = cur_scope->find_variable(ref->ident)) {
                    ref->set_var_ref(var);
                    return;
                }
                if constexpr (is_spell_checking) {
                    auto scope_itr = cur_scope;
                    while (scope_itr) {
                        spellcheck_variables_for_ident(scope_itr->variables, spell_checker, is_for_call);
                        scope_itr = scope_itr->parent;
                    }
                }
            }

            if (cur_struct && search_nested) {
                if (auto* var = cur_struct->nspace->find_variable(ref->ident)) {
                    ref->set_var_ref(var);
                    return;
                }
                if constexpr (is_spell_checking) {
                    spellcheck_variables_for_ident(cur_struct->nspace->get_variables(), spell_checker, is_for_call);
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

            if constexpr (is_spell_checking) {
                spellcheck_variables_for_ident(file->get_variables(), spell_checker, is_for_call);
                for (const Namespace* nspace : file->get_static_imports()) {
                    spellcheck_variables_for_ident(nspace->get_variables(), spell_checker, is_for_call);
                }
            }
        }

        if (auto* var = search_nspace->find_variable(ref->ident)) {
            ref->set_var_ref(var);
            return;
        }
        if (auto* universal = context.get_universal_constant(ref->ident)) {
            ref->set_universal_ref(universal);
        }

        if constexpr (is_spell_checking) {
            spellcheck_variables_for_ident(search_nspace->get_variables(), spell_checker, is_for_call);
            if (!is_for_call)
                spell_checker.add_searches(context.get_universal_constants());
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

    if (!ref->found_ref()) {
        if (search_relative && cur_func && cur_func->is_generic()) {
            if (cur_func && cur_func->is_generic()) {
                auto itr = std::ranges::find_if(cur_func->generics, [ref](auto genericn) {
                    return genericn->name == ref->ident;
                                                });
                if (itr != cur_func->generics.end()) {
                    ref->set_generic_type_ref((*itr)->type);
                }
            }
        } // ELSE TODO deal with struct case.
    }

    // If still not found let us try and search for an imported module.
    if (!ref->found_ref() && search_relative) {
        if (auto importn = file->find_import(ref->ident)) {
            if (importn->is_imported_namespace()) {
                if (importn->is_static) {
                    error(expand(ref), "Cannot reference a static import")
                        .end_error(ErrCode::SemaCannotRefStaticImport);
                }
                ref->set_namespace_ref(importn->imported_nspace);
            } else if (importn->is_imported_composite()) {
                ref->set_composite_ref(importn->imported_composite);
            } else {
                acorn_fatal("Unknown import kind");
            }
        } else if (auto composite1 = file->find_composite(ref->ident)) {
            ref->set_composite_ref(composite1);
        } else if (auto composite2 = nspace->find_composite(ref->ident)) {
            ref->set_composite_ref(composite2);
        }

        if constexpr (is_spell_checking) {
            if (!is_for_call) {
                // Making sure it the site of the dot operator since that is really
                // the only time that this happens.
                if (is_dot_op_site && search_relative) {
                    spell_checker.add_searches(file->get_imports());
                }
            }

            // We do not want to suggest composites because in general users are just typing
            // expressions and looking for variables/functions not composites. Not to mention
            // the code that determines the type of a variable/return type, ect... is handled
            // elsewhere and does its own spell checking.
            //
            // spell_checker.add_searches(file->get_composites());
        }
    }

    if constexpr (is_spell_checking) {
        // Not reporting the error just reporting spell checking information.
        spell_checker.search(logger, ref->ident);
        return;
    }

    switch (ref->found_kind) {
    case IdentRef::VarKind: {

        Var* var_ref = ref->var_ref;
        if (var_ref->is_global) {
            ensure_global_variable_checked(ref->loc, ref->var_ref);
        }

        if (var_ref->is_field()) {
            if (var_ref->structn->is_generic) {
                // TODO (maddie): do we really want to do this, this way?
                if (!search_struct_type) {
                    acorn_assert(cur_struct != nullptr, "Must have the struct information to get the appropriate generic instance");
                    var_ref = cur_struct->fields[var_ref->field_idx];
                } else {
                    Struct* resolved_struct = search_struct_type->get_struct();
                    var_ref = resolved_struct->fields[var_ref->field_idx];
                }
                ref->var_ref = var_ref;
            }

            if (var_ref->structn->is_being_checked) {
                PointSourceLoc error_loc = ref->loc.to_point_source();
                if (ref->is(NodeKind::DotOperator)) {
                    auto dot = static_cast<DotOperator*>(ref);
                    error_loc = dot->expand_access_only();
                }

                error(error_loc, "Cannot access field '%s' because currently checking '%s'",
                      var_ref->name, var_ref->structn->name)
                    .end_error(ErrCode::SemaFieldCheckDependsOnParentStruct);
                return;
            }
        }

        if (!var_ref->type) {
            if (!var_ref->is_global && !var_ref->is_field() && var_ref->is_being_checked) {
                report_error_cannot_use_variable_before_assigned(ref->loc, var_ref);
            }

            return; // Assumes an error was already generated somewhere else.
        }

        ref->is_foldable = var_ref->is_foldable;
        ref->type = var_ref->type;

        if (var_ref->is_field() && cur_func && cur_func->is_constant) {
            // Referencing a field in a constant struct so the field must be
            // constant.
            if (!ref->type->is_const()) {
                ref->type = type_table.get_const_type(ref->type);
            }
        }

        break;
    }
    case IdentRef::FuncsKind: {

        // TODO (maddie): generics cases involving not calling the thing.
        if (!is_for_call) {

            // Cannot simply check this here when making a function call because
            // of function overloading and the call operator being able to choose
            // which function is best based on context.
            auto func = (*ref->funcs_ref)[0];
            if (func->structn) {
                auto dot = static_cast<DotOperator*>(ref);
                if (dot->site->is_not(NodeKind::TypeExpr)) {
                    auto struct_name = func->structn->name.to_string().str();
                    auto func_name   = ref->ident.to_string().str();
                    auto example_str = struct_name + "." + func_name;
                    error(expand(ref), "Must access member function references by type not object")
                        .add_line("Use: '%s' instead", example_str)
                        .end_error(ErrCode::SemaCannotAccessFuncsRefFromObj);
                    return;
                }
            }


            // TODO: In the future we will want to allow for selecting for
            // overloaded functions somehow.

            auto param_types = func->params | std::views::transform([](Var* param) { return param->type; })
                                            | std::ranges::to<llvm::SmallVector<Type*>>();

            ref->type = type_table.get_function_type(func->return_type,
                                                     std::move(param_types),
                                                     func->raised_errors,
                                                     func->uses_native_varargs);
            ref->is_foldable = false;

            break;
        }

        ref->type = context.funcs_ref_type;
        break;
    }
    case IdentRef::UniversalKind: {
        ref->type = ref->universal_ref->type;
        break;
    }
    case IdentRef::NamespaceKind: {
        ref->type = context.namespace_ref_type;
        break;
    }
    case IdentRef::CompositeKind: {
        if (ref->composite_ref->is(NodeKind::Struct)) {
            Struct* structn = static_cast<Struct*>(ref->composite_ref);
            if (!structn->is_generic) {
                ensure_struct_checked(ref->loc, static_cast<Struct*>(ref->composite_ref));
            }
        } else if (ref->composite_ref->is(NodeKind::Enum)) {
            ensure_enum_checked(ref->loc, static_cast<Enum*>(ref->composite_ref));
        } else if (ref->composite_ref->is(NodeKind::Interface)) {
            ensure_interface_checked(ref->loc, static_cast<Interface*>(ref->composite_ref));
        } else {
            acorn_fatal("Unknown composite type");
        }

        ref->type = context.expr_type;
        break;
    }
    case IdentRef::GenericTypeKind: {
        ref->type = context.expr_type;
        break;
    }
    case IdentRef::NoneKind: {
        auto& logger = error(expand(ref), "Could not find %s '%s'", is_for_call ? "function" : "identifier", ref->ident);
        check_ident_ref<true>(ref, search_nspace, search_struct_type, is_for_call, is_dot_op_site);
        logger.end_error(!is_for_call ? ErrCode::SemaNoFindIdentRef : ErrCode::SemaNoFindFuncIdentRef);
        return;
    }
    case IdentRef::EnumValueKind: {
        acorn_fatal("Cannot reference enum values here");
        return;
    }
    }

    if (ref->explicitly_binds_generics) {
        check_generic_bind_function_call(static_cast<GenericBindFuncCall*>(ref));
    }
}

void acorn::Sema::check_dot_operator(DotOperator* dot, bool is_for_call) {
    if (dot->site->is(NodeKind::IdentRef)) {
        IdentRef* ref = static_cast<IdentRef*>(dot->site);
        check_ident_ref(ref, nspace, nullptr, false, true);
        dot->is_foldable = ref->is_foldable;

        yield_if(dot->site);

        if (ref->type == context.namespace_ref_type) {
            // Special case in which we search in a given module.
            check_ident_ref(dot, ref->nspace_ref, nullptr, is_for_call);
            return;
        } else if (!ref->type) {
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
        check_ident_ref(dot, structn->nspace, struct_type, is_for_call);

        if (!dot->type) {
            return;
        }

        if (dot->is_var_ref() && dot->var_ref->has_modifier(Modifier::Private)) {
            if (!cur_func || cur_func->non_generic_struct_instance != dot->var_ref->non_generic_struct_instance) {
                error(expand(dot), "Cannot access field '%s', it is marked private",
                      dot->var_ref->name)
                    .end_error(ErrCode::SemaCannotAccessFieldIsPrivate);
            }
        }

        if (struct_type->is_const() && !dot->type->is_const()) {
            // The constness must be passed onto the field to prevent modification of fields.
            dot->type = type_table.get_const_type(dot->type);
        }
    };

    if (dot->ident == context.length_identifier &&
        (dot->site->type->is_array() || dot->site->type->is_slice())) {
        dot->is_array_length = true;
        dot->type = context.int_type;
        dot->trivially_reassignable = true;
        dot->is_foldable = true;
    } else if (dot->ident == context.ptr_identifier && dot->site->type->is_slice()) {
        auto slice_type = static_cast<SliceType*>(dot->site->type);
        dot->is_slice_ptr = true;
        dot->type = type_table.get_ptr_type(slice_type->get_elm_type());
    } else if (dot->ident == context.value_identifier && dot->site->type->is_enum()) {
        auto enum_type = static_cast<EnumType*>(dot->site->type);

        dot->is_enum_value = true;
        dot->type = enum_type->get_values_type();
        dot->is_foldable = false;

    } else if (dot->site->type->is_struct()) {
        auto struct_type = static_cast<StructType*>(dot->site->type);
        check_struct_ident_ref(struct_type);
    } else if (dot->site->type->is_pointer()) {
        auto ptr_type = static_cast<PointerType*>(dot->site->type);
        auto elm_type = ptr_type->get_elm_type();
        if (elm_type->is_struct()) {
            auto struct_type = static_cast<StructType*>(elm_type);
            check_struct_ident_ref(struct_type);
        } else if (elm_type->is_interface() && is_for_call) {
            auto intr_type = static_cast<InterfaceType*>(elm_type);
            auto interfacen = intr_type->get_interface();

            temp_ref_functions.clear();

            for (auto func : interfacen->functions) {
                if (func->name == dot->ident) {
                    temp_ref_functions.push_back(func);
                }
            }

            if (temp_ref_functions.empty()) {
                error(expand(dot), "Could not find function '%s'", dot->ident)
                    .end_error(ErrCode::SemaNoFindFuncIdentRef);
                return;
            }

            dot->set_funcs_ref(&temp_ref_functions);
            dot->type = context.funcs_ref_type;

        } else {
            report_error_cannot_access_field();
        }
    } else if (dot->site->is(NodeKind::IdentRef) && dot->site->type->get_kind() == TypeKind::Expr) {
        auto ref = static_cast<IdentRef*>(dot->site);
        auto composite = ref->composite_ref;
        if (composite->is(NodeKind::Enum)) {
            auto enumn = static_cast<Enum*>(composite);
            auto itr = std::ranges::find_if(enumn->values, [dot](const Enum::Value& value) {
                return value.name == dot->ident;
            });
            if (itr != enumn->values.end()) {
                Enum::Value& value = *itr;
                dot->type = enumn->enum_type;
                // It should be fine to take the address here since the array is no
                // longer modified after parsing.
                dot->set_enum_value_ref(&value);
            } else {
                error(expand(dot), "Could not find value '%s' in enum '%s'",
                      dot->ident, enumn->name)
                    .end_error(ErrCode::SemaEnumCouldNotFindValue);
            }
        } else if (composite->is(NodeKind::Struct) && !is_for_call) {
            // Check to see if the user is trying to access the member function of
            // a struct.
            auto structn = static_cast<Struct*>(composite);
            auto funcs = structn->nspace->find_functions(dot->ident);
            if (!funcs) {
                report_error_cannot_access_field();
                return;
            }

            auto func = (*funcs)[0];

            if (func->has_modifier(Modifier::Private)) {
                if (!cur_func || cur_func->non_generic_struct_instance != func->non_generic_struct_instance) {
                    error(expand(dot), "Cannot access function '%s', it is marked private",
                          func->name)
                        .end_error(ErrCode::SemaCannotAccessFuncIsPrivate);
                    return;
                }
            }

            auto param_types = func->params | std::views::transform([](Var* param) { return param->type; })
                                            | std::ranges::to<llvm::SmallVector<Type*>>();

            Type* struct_type = structn->struct_type;
            if (func->is_constant) {
                struct_type = type_table.get_const_type(struct_type);
            }
            auto struct_ptr_type = type_table.get_ptr_type(struct_type);
            param_types.insert(param_types.begin(), struct_ptr_type);

            dot->set_funcs_ref(funcs);
            dot->type = type_table.get_function_type(func->return_type,
                                                     std::move(param_types),
                                                     func->raised_errors,
                                                     func->uses_native_varargs);
            dot->is_foldable = false;

        } else {
            report_error_cannot_access_field();
        }
    } else {
        report_error_cannot_access_field();
    }
}

void acorn::Sema::check_function_call(FuncCall* call) {

    auto check_raised_errors = [this, call](const llvm::SmallVector<RaisedError>& raised_errors) finline {
        if (raised_errors.empty()) {
            return;
        }
        if (!cur_scope || !cur_scope->cur_try) {
            error(expand(call), "Call has uncaught errors")
                .end_error(ErrCode::SemaUncaughtErrors);
        } else if (cur_scope) {
            for (auto& raised_error : raised_errors) {
                cur_scope->cur_try->caught_errors.push_back(raised_error.structn);
            }
        }
    };

    bool args_have_errors = false;
    for (Expr* arg : call->args) {
        if (arg->is(NodeKind::MoveObj)) {
            check_moveobj(static_cast<MoveObj*>(arg), true);
        } else {
            check_node(arg);
        }
        if (!arg->type) args_have_errors = true;
    }
    if (args_have_errors) return;

    if (call->site->is(NodeKind::IdentRef)) {
        auto ref = static_cast<IdentRef*>(call->site);
        check_ident_ref(ref, nspace, nullptr, true);
        yield_if(ref);
    } else if (call->site->is(NodeKind::DotOperator)) {
        auto ref = static_cast<DotOperator*>(call->site);
        check_dot_operator(ref, true);
        yield_if(ref);
    } else {
        check_and_verify_type(call->site);
    }

    if (call->site->type->is_function()) {
        auto function_type = static_cast<FunctionType*>(call->site->type);
        check_function_type_call(call, function_type);
        check_raised_errors(function_type->get_raised_errors());
        return;
    }

    if (call->site->type->remove_all_const() != context.funcs_ref_type) {
        if (call->site->type == context.expr_type && call->site->is(NodeKind::IdentRef)) {
            auto ref = static_cast<IdentRef*>(call->site);
            Type* fixed_type = fixup_unresolved_generic_composite_type(ref->composite_ref, ref->loc, call->args);
            if (!fixed_type) {
                return;
            }

            call->type_for_type_expr = fixed_type;
            call->type = context.expr_type;
            return;
        }

        error(expand(call->site), "Type '%s' is not callable", call->site->type)
            .end_error(ErrCode::SemaTypeNoCallable);
        return;
    }

    bool is_const_object = false;
    if (call->site->is(NodeKind::DotOperator)) {
        auto dot = static_cast<DotOperator*>(call->site);
        if (dot->site->is(NodeKind::IdentRef)) {
            auto ref = static_cast<IdentRef*>(dot->site);
            if (ref->type->is_struct() && ref->type->is_const()) {
                is_const_object = true;
            } else if (ref->type->is_pointer()) {
                auto ptr_type = static_cast<PointerType*>(ref->type);
                if (ptr_type->get_elm_type()->is_const()) {
                    is_const_object = true;
                }
            }
        }
    } else if (cur_func && cur_func->is_constant) {
        is_const_object = true;
    }

    Struct* generic_parent_struct = nullptr;
    IdentRef* ref = static_cast<IdentRef*>(call->site);
    llvm::SmallVector<Type*> pre_bound_types;

    // Check if calling a member function of a generic struct in which case
    // the the currently bound generic types must be pre bound.
    if (cur_func && cur_func->structn && cur_func->structn->is_generic) {
        bool passes_generics_along = (ref->is_not(NodeKind::DotOperator) &&
                                      cur_func->non_generic_struct_instance == (*ref->funcs_ref)[0]->non_generic_struct_instance);
        if (!passes_generics_along && ref->is(NodeKind::DotOperator)) {
            auto dot = static_cast<DotOperator*>(ref);
            if (dot->site->is(NodeKind::This)) {
                passes_generics_along = true;
            }
        }

        if (passes_generics_along) {
            generic_parent_struct = cur_func->structn;
            pre_bound_types.insert(pre_bound_types.begin(),
                                    cur_func->generic_instance->bound_types.begin(),
                                    cur_func->generic_instance->bound_types.end());
        }
    }

    if (ref->explicitly_binds_generics) {
        auto generic_bind_call = static_cast<GenericBindFuncCall*>(ref);
        pre_bound_types = std::move(generic_bind_call->bound_types);
    } else if (ref->is(NodeKind::DotOperator)) {
        // Bind the types of the generic struct if there is one.
        auto dot_op = static_cast<DotOperator*>(ref);
        if (dot_op->site->type->is_struct()) {
            auto struct_type = static_cast<StructType*>(dot_op->site->type);
            auto structn = struct_type->get_struct();
            if (structn->is_generic) {
                generic_parent_struct = structn;
                auto generic_struct_instance = static_cast<GenericStructInstance*>(structn);
                pre_bound_types.insert(pre_bound_types.begin(),
                                       generic_struct_instance->bound_types.begin(),
                                       generic_struct_instance->bound_types.end());
            }
        }
    } else if (ref->is(NodeKind::This) && cur_func && cur_func->structn && cur_func->structn->is_generic) {
        // Check if calling a member function of a generic struct in which case
        // the the currently bound generic types must be pre bound.
    }

    Struct* ignored;
    auto called_func = check_function_decl_call(call,
                                                call->args,
                                                call->non_named_args_offset,
                                                *ref->funcs_ref,
                                                is_const_object,
                                                pre_bound_types,
                                                generic_parent_struct,
                                                ignored);
    if (!called_func) {
        return;
    }

    check_raised_errors(called_func->raised_errors);

    call->is_foldable = false;
    call->called_func = called_func;
    if (!called_func->is_generic()) {
        call->type = called_func->return_type;
    } else {
        call->type = call->generic_instance->qualified_decl_types[0];
    }
 }

void acorn::Sema::check_generic_bind_function_call(GenericBindFuncCall* call) {

    if (call->type != context.funcs_ref_type) {
        call->type = nullptr;
        error(expand(call), "Expected function in order to bind generics")
            .end_error(ErrCode::SemaExpectedFunctionToBindGenerics);
    }

    if (!call->is_funcs_ref()) {
        acorn_fatal("Not handling other cases yet");
    }

    if (call->args.empty()) {
        error(expand(call), "Expected generic arguments")
            .end_error(ErrCode::SemaGenericFuncBindCallNoArguments);
        call->type = nullptr;
        return;
    }

    bool uses_named_values = false;
    if (!check_generic_bind_arguments(call->args, uses_named_values)) {
        call->type = nullptr;
        return;
    }

    llvm::SmallVector<Func*, 8> candidates;
    size_t generic_func_count = 0;
    for (Func* func : *call->funcs_ref) {
        if (func->is_generic()) {
            ++generic_func_count;
            if (call->args.size() <= func->generics.size()) {
                candidates.push_back(func);
            }
        }
    }

    if (generic_func_count == 0) {
        error(expand(call), "Expected generic function in order to bind generics")
            .end_error(ErrCode::SemaExpectedGenericFuncToBindGenericTypes);
        call->type = nullptr;
        return;
    }

    if (candidates.empty()) {
        error(expand(call), "Too many generic arguments")
            .end_error(ErrCode::SemaGenericFuncBindCallWrongNumTypes);
        call->type = nullptr;
        return;
    }

    if (uses_named_values) {
        temp_ref_functions.clear();
        for (Func* candidate : candidates) {
            if (compare_generic_bind_candidate_with_named_args(call->args, candidate->generics, call->bound_types)) {
                temp_ref_functions.push_back(candidate);
            }
        }
        if (temp_ref_functions.empty()) {
            Logger& logger = error(expand(call), "Failed to bind generic arguments");
            for (Func* candidate : candidates) {
                logger.add_empty_line();
                logger.add_line("Could not match: '%s'", candidate->get_decl_string());
                display_generic_bind_named_args_fail_info(call->args, candidate->generics);
            }
            logger.end_error(ErrCode::SemaFailedToBindGenerics);
            call->type = nullptr;
            return;
        }

        if (temp_ref_functions.size() > 1) {
            auto& logger = error(expand(call), "Ambiguous choice between generic functions:")
                .remove_period()
                .add_empty_line();
            display_ambiguous_functions(temp_ref_functions);
            logger.end_error(ErrCode::SemaAmbiguousGenericCallBind);
            call->type = nullptr;
            return;
        }
    } else {
        temp_ref_functions = std::move(candidates);

        for (Expr* arg : call->args) {
            Type* bind_type = get_type_of_type_expr(arg);
            call->bound_types.push_back(bind_type);
        }
    }
}

bool acorn::Sema::compare_generic_bind_candidate_with_named_args(const llvm::SmallVector<Expr*>& args,
                                                                 const llvm::SmallVector<Generic*>& generics,
                                                                 llvm::SmallVector<Type*>& bound_types) {

    bool named_args_out_of_order = false;
    size_t named_arg_high_idx = 0;
    for (size_t i = 0; i < args.size(); i++) {

        auto arg      = args[i];
        auto genericn = generics[i];

        if (arg->is(NodeKind::NamedValue)) {

            auto named_arg = static_cast<NamedValue*>(arg);

            auto itr = std::ranges::find_if(generics, [name = named_arg->name](auto& genericn) {
                return genericn->name == name;
            });
            if (itr == generics.end()) {
                return false;
            }

            genericn = *itr;
            arg = named_arg->assignment;

            if (genericn->index != i) {
                named_args_out_of_order = true;
            }
            named_arg_high_idx = std::max(genericn->index, named_arg_high_idx);
        } else {
            // Cannot determine the order of the arguments if the
            // non-named arguments come after the named arguments
            // and the named arguments are not in order.
            if (named_args_out_of_order || named_arg_high_idx > i) {
                return false;
            }
        }

        Type* bind_type = get_type_of_type_expr(arg);

        if (genericn->index == i) {
            bound_types.push_back(bind_type);
        } else {
            // Ensure there is enough space.
            bound_types.resize(genericn->index + 1);
            bound_types[genericn->index] = bind_type;
        }
    }

    return true;
}

bool acorn::Sema::check_generic_bind_arguments(const llvm::SmallVector<Expr*>& args,
                                               bool& uses_named_values) {
    bool args_have_errors = false;
    for (Expr* arg : args) {

        check_node(arg);
        Expr* arg_value = arg;
        if (arg->is(NodeKind::NamedValue)) {
            uses_named_values = true;
            auto named_value = static_cast<NamedValue*>(arg);
            arg_value = named_value->assignment;
        }

        if (!arg->type) args_have_errors = true;

        if (arg_value->type != context.expr_type) {
            args_have_errors = true;
            error(expand(arg_value), "Expected argument to be a type expression")
                .end_error(ErrCode::SemaExpectedArgToBeTypeExprForGenericBind);
        }

        Type* bind_type = get_type_of_type_expr(arg_value);

        if (is_incomplete_type(bind_type)) {
            args_have_errors = true;
            error(expand(arg_value), "Cannot bind incomplete type")
                .end_error(ErrCode::SemaCannotBindIncompleteType);
        }
    }
    return !args_have_errors;
}

void acorn::Sema::check_function_type_call(FuncCall* call, FunctionType* func_type) {
    auto& param_types = func_type->get_param_types();
    bool uses_native_varargs = func_type->uses_native_varargs();

    auto display_error = [this, call, func_type]() finline {
        logger.begin_error(expand(call), "Invalid call to function type: %s", func_type);
        logger.add_empty_line();
        display_call_mismatch_info(func_type, call->args, false, true, call, {});
        logger.end_error(ErrCode::SemaInvalidFuncCallSingle);
    };

    if (!uses_native_varargs) {
        if (call->args.size() != param_types.size()) {
            display_error();
            return;
        }
    } else {
        if (call->args.size() < param_types.size()) {
            display_error();
            return;
        }
    }

    size_t last_index = param_types.size() - 1;
    for (size_t i = 0; i < call->args.size(); i++) {
        Expr* arg = call->args[i];

        if (uses_native_varargs && i > last_index) {
            // Still have to check for aggregates or incomplete types so that
            // types which are not able to be passed to native functions are
            // not passed.
            //
            if (arg->type->is_aggregate()) {
                display_error();
                return;
            }

            if (is_incomplete_type(arg->type)) {
                display_error();
                return;
            }

            continue;
        }


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
                                                   FuncList& candidates,
                                                   bool is_const_object,
                                                   const llvm::SmallVector<Type*>& pre_bound_types,
                                                   Struct* generic_parent_struct,
                                                   Struct*& fully_bound_parent_struct) {

    // Make sure the function declarations for the canidates have been checked.
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
        } else if (canidate->has_errors) {
            return nullptr;
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

    // Proceed to find the best canidate from the provided call canidates.
    bool is_ambiguous = false;
    bool selected_implicitly_converts_ptr_arg = false;
    bool passes_varargs_along = false;
    llvm::SmallVector<Type*> generic_bindings;
    llvm::SmallVector<Type*> qualified_decl_types;
    auto called_func = find_best_call_candidate(candidates,
                                                args,
                                                selected_implicitly_converts_ptr_arg,
                                                is_ambiguous,
                                                is_const_object,
                                                generic_bindings,
                                                pre_bound_types);
    if (!called_func) {
        display_call_mismatch_info(expand(call_node), call_node, candidates, args, pre_bound_types);
        return nullptr;
    }
    if (is_ambiguous) {
        display_call_ambiguous_info(expand(call_node), candidates, args, is_const_object, pre_bound_types);
        return nullptr;
    }

    bool uses_varargs = called_func->uses_varargs || called_func->uses_native_varargs;


    if (called_func->is_generic()) {
        // Attempt to let the return type bind to any left over unbound generic
        // types.
        //if (assignment_type && called_func->return_type->does_contain_generics()) {
        //    try_bind_type_to_generic_type(called_func->return_type, assignment_type, generic_bindings);
        //}

        for (Type* bound_type : generic_bindings) {
            if (!bound_type) {
                display_call_missing_bindings_info(call_node, called_func, generic_bindings);
                return nullptr;
            }
        }

        func_call_generic_bindings = &generic_bindings;

        // +1 for return type.
        qualified_decl_types.resize(called_func->params.size() + 1);

        // Attempt to fixup any indeterminate state.
        //
        for (size_t i = called_func->default_params_offset; i < called_func->params.size(); i++) {
            Var* param = called_func->params[i];

            if (param->assignment_contains_generics) {
                Expr* bound_expr = try_create_bound_expr_for_variable_with_indetermite_type(param);
                if (!bound_expr) {
                    func_call_generic_bindings = nullptr;
                    return nullptr;
                }

                if (call_node->is(NodeKind::FuncCall)) {
                    auto call = static_cast<FuncCall*>(call_node);
                    call->indeterminate_inferred_default_args.push_back(bound_expr);
                } else {
                    auto initializer = static_cast<StructInitializer*>(call_node);
                    initializer->indeterminate_inferred_default_values.push_back(bound_expr);
                }

                // +1 for return type.
                qualified_decl_types[i + 1] = bound_expr->type;

                continue;
            }
        }

        if (qualified_decl_types[0] == nullptr) {
            auto return_type = called_func->partially_qualified_types[0];
            return_type = fixup_type(return_type);
            qualified_decl_types[0] = return_type;
        }
    }


    // Creating casts from the argument to the parameter.
    size_t last_index = called_func->params.size() - 1;
    for (size_t i = 0; i < args.size(); i++) {

        if (called_func->uses_native_varargs && i > last_index) {
            continue;
        }

        auto arg_value = args[i];
        Var* param;
        Type* param_type;
        if (arg_value->is(NodeKind::NamedValue)) {
            auto named_arg = static_cast<NamedValue*>(arg_value);
            param = called_func->find_parameter(named_arg->name);
            param_type = param->type;
            named_arg->mapped_idx = param->param_idx;

            arg_value = named_arg->assignment;
        } else {
            if (called_func->uses_varargs && i >= last_index) {
                param = called_func->params[last_index];
                auto slice_type = static_cast<SliceType*>(param->type);
                param_type = slice_type->get_elm_type();
            } else {
                param = called_func->params[i];
                param_type = param->type;
            }
        }

        if (called_func->is_generic()) {
            if (qualified_decl_types[param->param_idx + 1] == nullptr) {
                param_type = called_func->partially_qualified_types[param->param_idx + 1];

                if (param_type->does_contain_generics()) {
                    auto qualified_generic_type = fixup_type(param_type);
                    qualified_decl_types[param->param_idx + 1] = qualified_generic_type;
                    param_type = qualified_generic_type;
                } else {
                    qualified_decl_types[param->param_idx + 1] = param_type;
                }
            } else {
                param_type = qualified_decl_types[param->param_idx + 1];
            }

            if (called_func->uses_varargs && i >= last_index) {
                auto slice_type = static_cast<SliceType*>(param_type);
                param_type = slice_type->get_elm_type();
            }
        }


        if (selected_implicitly_converts_ptr_arg && arg_value->is(NodeKind::FuncCall)) {
            auto arg_call = static_cast<FuncCall*>(arg_value);
            if (may_implicitly_convert_return_ptr(param_type, arg_call)) {
                arg_call->implicitly_converts_return = true;
                continue; // Do not continue to creating the cast.
            }
        }

        create_cast(arg_value, param_type);

    }


    if (called_func->is_generic()) {

        // Qualifying any parameters with default values that did not have an assigned
        // argument.
        if (called_func->default_params_offset != -1) {
            for (size_t i = called_func->default_params_offset; i < called_func->params.size(); i++) {
                if (qualified_decl_types[i + 1] == nullptr) {
                    Var* param = called_func->params[i];
                    qualified_decl_types[i + 1] = param->type;
                }
            }
        }

        // If calling a generic constructor then the pre bound types have not yet created
        // a generic struct because the constructor call is allowed to finish binding the
        // types first. We must take the types that have been bound by the function call and
        // create a finalized version of the generic struct instance out of it.
        if (called_func->is_constructor) {
            auto unbound_generic_struct = static_cast<UnboundGenericStruct*>(generic_parent_struct);

            llvm::SmallVector<Type*> parent_struct_bound_types;
            parent_struct_bound_types.insert(
                parent_struct_bound_types.begin(),
                generic_bindings.begin(),
                generic_bindings.begin() + unbound_generic_struct->generics.size());
            generic_parent_struct = unbound_generic_struct->get_generic_instance(context.get_allocator(), std::move(parent_struct_bound_types));

            if (!ensure_struct_checked(call_node->loc, generic_parent_struct)) {
                func_call_generic_bindings = nullptr;
                return nullptr;
            }

            fully_bound_parent_struct = generic_parent_struct;
        }

        auto instance = called_func->get_generic_instance(context.get_allocator(),
                                                          std::move(generic_bindings),
                                                          std::move(qualified_decl_types),
                                                          generic_parent_struct);
        if (call_node->is(NodeKind::FuncCall)) {
            auto call = static_cast<FuncCall*>(call_node);
            call->generic_instance = instance;
        } else {
            auto initializer = static_cast<StructInitializer*>(call_node);
            initializer->generic_called_instance = instance;
        }
        func_call_generic_bindings = nullptr;
    }

    return called_func;
}

acorn::Func* acorn::Sema::find_best_call_candidate(FuncList& candidates,
                                                   llvm::SmallVector<Expr*>& args,
                                                   bool& selected_implicitly_converts_ptr_arg,
                                                   bool& is_ambiguous,
                                                   bool is_const_object,
                                                   llvm::SmallVector<Type*>& generic_bindings,
                                                   const llvm::SmallVector<Type*>& pre_bound_types) {

    Func* selected = nullptr;
    uint64_t best_score = std::numeric_limits<uint64_t>::max();

    for (Func* candidate : candidates) {

        uint64_t score = 0;
        bool implicitly_converts_ptr_arg = false;

        // Keep a list of generic types that already have bindings.
        llvm::SmallVector<Type*> candidate_generic_bindings;
        if (!candidate->is_generic()) {
            auto status = compare_as_call_candidate<false, false>(candidate,
                                                                  args,
                                                                  is_const_object,
                                                                  score,
                                                                  implicitly_converts_ptr_arg,
                                                                  candidate_generic_bindings);
            if (status != CallCompareStatus::SUCCESS) {
                continue;
            }
        } else {
              candidate_generic_bindings.insert(
                candidate_generic_bindings.begin(),
                pre_bound_types.begin(),
                pre_bound_types.end()
            );
            candidate_generic_bindings.resize(candidate->generics.size());
            auto status = compare_as_call_candidate<false, true>(candidate,
                                                                 args,
                                                                 is_const_object,
                                                                 score,
                                                                 implicitly_converts_ptr_arg,
                                                                 candidate_generic_bindings);
            if (status != CallCompareStatus::SUCCESS) {
                continue;
            }
        }

        if (!selected || best_score > score) {
            selected = candidate;
            best_score = score;
            selected_implicitly_converts_ptr_arg = implicitly_converts_ptr_arg;
            is_ambiguous = false;
            generic_bindings = std::move(candidate_generic_bindings);
        } else if (best_score == score) {
            is_ambiguous = true;
        }
    }
    return selected;
}

template<bool for_score_gathering, bool checking_generic>
acorn::Sema::CallCompareStatus acorn::Sema::compare_as_call_candidate(const Func* candidate,
                                                                      const llvm::SmallVector<Expr*>& args,
                                                                      const bool is_const_object,
                                                                      uint64_t& score,
                                                                      bool& implicitly_converts_ptr_arg,
                                                                      llvm::SmallVector<Type*>& generic_bindings) {

    bool encountered_mismatched_types = false;
    bool forwards_varargs = false;

    if (!has_correct_number_of_args(candidate, args)) {
        return CallCompareStatus::INCORRECT_ARGS;
    }

    if (is_const_object && candidate->structn && !candidate->is_constant) {
        return CallCompareStatus::NON_CONST_FROM_CONST_OBJECT;
    } else if (!is_const_object && candidate->is_constant) {
        score += PREFER_NON_CONST_LIMIT;
    }

    if (candidate->has_modifier(Modifier::Private) && candidate->structn) {
        if (!cur_func || (cur_func->non_generic_struct_instance != candidate->non_generic_struct_instance)) {
            return CallCompareStatus::CANNOT_ACCESS_PRIVATE;
        }
    }

    if (candidate->uses_varargs || candidate->uses_native_varargs) {
        score += IS_VARARGS_LIMIT;
    }

    if constexpr (checking_generic) {
        score += IS_GENERIC_LIMIT;
    }

    size_t last_index = candidate->params.size() - 1;
    bool named_args_out_of_order = false;
    uint32_t named_arg_high_idx = 0;
    for (size_t i = 0; i < args.size(); i++) {
        Expr* arg_value = args[i];
        Type* param_type;
        Var* param;

        if (candidate->uses_native_varargs && i > last_index) {

            // Still have to check for aggregates or incomplete types so that
            // types which are not able to be passed to native functions are
            // not passed.
            //
            if (arg_value->type->is_aggregate()) {
                return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
            }

            if (is_incomplete_type(arg_value->type)) {
                return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
            }

            // Variadic arguments for native functions take any type except
            // aggregate types.
            continue;
        }

        if (arg_value->is(NodeKind::NamedValue)) {
            // Handle named arguments by finding the corresponding parameter

            auto named_arg = static_cast<NamedValue*>(arg_value);
            arg_value = named_arg->assignment;
            param = candidate->find_parameter(named_arg->name);

            // Check to make sure we found the parameter by the given name.
            if (!param) {
                return CallCompareStatus::INCORRECT_PARAM_BY_NAME_NOT_FOUND;
            }

            param_type = param->type;

            if (candidate->uses_varargs && i >= last_index) {
                return CallCompareStatus::CANNOT_USE_VARARGS_AS_NAMED_ARG;
            }

            if (param->param_idx != i) {
                named_args_out_of_order = true;
            }
            named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
        } else {

            if (candidate->uses_varargs && i >= last_index) {
                param = candidate->params[last_index];
                auto slice_type = static_cast<SliceType*>(param->type);
                param_type = slice_type->get_elm_type();

                if (forwards_varargs) {
                    return CallCompareStatus::FORWARD_VARIADIC_WITH_OTHERS;
                }

                // Checking for special condition in which a function passes its own variadic arguments
                // to another function that takes variadic arguments.
                if (cur_func && cur_func->forwards_varargs(arg_value)) {
                    if (i > last_index) {
                        return CallCompareStatus::FORWARD_VARIADIC_WITH_OTHERS;
                    }

                    // Do not proceed to check the argument because it forwards
                    // the variadic parameters.
                    forwards_varargs = true;
                    continue;
                }

            } else {
                param = candidate->params[i];
                param_type = param->type;
            }

            // Cannot determine the order of the arguments if the
            // non-named arguments come after the named arguments
            // and the named arguments are not in order.
            if (named_args_out_of_order || named_arg_high_idx > i) {
                return CallCompareStatus::OUT_OF_ORDER_PARAMS;
            }
        }

        if constexpr (checking_generic) {
            param_type = candidate->partially_qualified_types[param->param_idx + 1];
            if (candidate->uses_varargs && i >= last_index) {
                auto slice_type = static_cast<SliceType*>(param_type);
                param_type = slice_type->get_elm_type();
            }

            // Check for when the assignment is determined by the assignment of an expression
            // containing gernerics such that it is not possible to determine the type of the
            // parameter in order to compare.
            if (param_type == context.indeterminate_type) {
                continue;
            }

            if (param_type->does_contain_generics()) {
                if (!check_bind_type_to_generic_type(param_type, arg_value->type, generic_bindings)) {

                    if (param->has_implicit_ptr && arg_value->is_not(NodeKind::MoveObj)) {
                        if (!arg_value->type->is_pointer()) {
                            auto param_ptr_type = static_cast<PointerType*>(param_type);
                            auto param_elm_type = param_ptr_type->get_elm_type();

                            if (check_bind_type_to_generic_type(param_elm_type, arg_value->type, generic_bindings)) {
                                // argument success, continue.
                                continue;
                            } else {
                                return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
                            }
                        }
                    }

                    if (arg_value->is(NodeKind::FuncCall)) {
                        auto arg_call = static_cast<FuncCall*>(arg_value);
                        if (arg_call->called_func && arg_call->called_func->has_implicit_return_ptr) {

                            auto ptr_type = static_cast<PointerType*>(arg_call->type);
                            auto from_type = ptr_type->get_elm_type();

                            if (check_bind_type_to_generic_type(param_type, from_type, generic_bindings)) {
                                implicitly_converts_ptr_arg = true;
                                // argument success, continue.
                                continue;
                            } else {
                                return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
                            }
                        }
                    }

                    return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
                }

                // argument success, continue.
                continue;
            }
        }

        if (!is_assignable_to(param_type, arg_value)) {

            bool is_assignable = false;
            if (param->has_implicit_ptr && arg_value->is_not(NodeKind::MoveObj)) {
                auto ptr_type = static_cast<PointerType*>(param_type);
                if (may_implicitly_convert_ptr(ptr_type, arg_value)) {
                    if (arg_value->is_not(NodeKind::MoveObj)) {
                        is_assignable = true;
                    }
                }
            }

            if (!is_assignable && arg_value->is(NodeKind::FuncCall)) {
                auto arg_call = static_cast<FuncCall*>(arg_value);
                if (may_implicitly_convert_return_ptr(param_type, arg_call)) {
                    implicitly_converts_ptr_arg = true;
                    is_assignable = true;
                }
            }

            if (!is_assignable) {
                if constexpr (for_score_gathering) {
                    score += NOT_ASSIGNABLE_TYPES_LIMIT;
                    encountered_mismatched_types = true;
                    continue;
                } else {
                    return CallCompareStatus::ARGS_NOT_ASSIGNABLE;
                }
            }
        }

        if (param_type->remove_all_const()->is_not(arg_value->type->remove_all_const())) {
            if (context.is_std_any_type(param_type)) {
                score += IMPLICIT_CAST_TO_ANY_LIMIT;
            } else {
                // Lowest order case considered least important.
                ++score;
            }
        }
    }

    return CallCompareStatus::SUCCESS;
}

bool acorn::Sema::has_correct_number_of_args(const Func* candidate, const
                                             llvm::SmallVector<Expr*>& args) const {
    if (candidate->default_params_offset == -1) {
        if (!(candidate->uses_native_varargs || candidate->uses_varargs)) {
            if (candidate->params.size() != args.size()) {
                return false;
            }
        } else if (candidate->uses_varargs) {
            // Subtract off one because the last parameter represents
            // the variadic parameters and doesn't require it to be provided.
            if (args.size() < candidate->params.size() - 1) {
                return false;
            }
        } else {
            if (args.size() < candidate->params.size()) {
                return false;
            }
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

bool acorn::Sema::check_bind_type_to_generic_type(Type* to_type,   // Type at current level of comparison
                                                  Type* from_type, // Type at current level of comparison
                                                  llvm::SmallVector<Type*>& bindings,
                                                  bool enforce_elm_const) {
    if (to_type->is_generic()) {
        // Generic type can have any type bound so we are finished.
        auto generic_type = static_cast<GenericType*>(to_type);

        size_t generic_index = generic_type->get_generic_index();
        if (auto bound_type = bindings[generic_index]) {

            if (enforce_elm_const) {
                if (!bound_type->is_const() && from_type->is_const()) {
                    return false;
                }
            }

            if (!try_remove_const_for_compare(bound_type, from_type, nullptr)) {
                return false;
            }

            // TODO (maddie): does the implicit conversion of pointers happen here
            // or elsewhere?

            // TODO (maddie): this is way too strict. it will be at the very least
            // nice to check if the type is convertable to the other type. It might
            // also be a good idea to pass up information up the stack that basically
            // tells if all the types have been bound and if they have then use normal
            // assignment comparison instead.
            return bound_type->is(from_type);
        }

        // Constness situations and how they are reduced:
        //
        // 1.
        // generics[T]
        // fn foo(a: T*) {}
        //
        // v: int*;
        // foo(v);
        //
        // Neither `v` or `T` are const so T=int.
        //
        // 2.
        // generics[T]
        // fn foo(a: const T*) {}
        //
        // v: int*;
        // foo(v);
        //
        // Since `T` is const but `v` is not `T` it becomes T=int
        //
        // 3.
        // generics[T]
        // void foo(a: T*) {}
        //
        // v: const int*;
        // foo(v);
        //
        // Since `T` is not const but `v` is const `T` takes on the
        // constness of v and and so T=const int
        //
        // 4.
        // generics[T]
        // void foo(a: const T*) {}
        //
        // v: const int*;
        // foo(v);
        //
        // Since `T` is const and `v` is const `T` becomes T=int
        //

        if (to_type->is_const() && from_type->is_const()) {
            // Make sure not to make T const since it pattern matched
            // on the existing const.
            bindings[generic_index] = from_type->remove_all_const();
        } else {
            bindings[generic_index] = from_type;
        }
        return true;
    } else if (enforce_elm_const) {
        // Not generic but element of container, so need to enforce const rules.
        if (!to_type->is_const() && from_type->is_const()) {
            // Violates const rules.
            return false;
        }
    }


    switch (to_type->get_kind()) {
    case TypeKind::Pointer:
    case TypeKind::Slice: {
        if (from_type->get_kind() != to_type->get_kind()) {
            return false;
        }

        auto ptr_to_type   = static_cast<ContainerType*>(to_type);
        auto ptr_from_type = static_cast<ContainerType*>(from_type);

        auto elm_to_type   = ptr_to_type->get_elm_type();
        auto elm_from_type = ptr_from_type->get_elm_type();

        return check_bind_type_to_generic_type(elm_to_type, elm_from_type, bindings, true);
    }
    case TypeKind::Array: {
        if (!from_type->is_array()) {
            return false;
        }

        auto arr_to_type   = static_cast<ArrayType*>(to_type);
        auto arr_from_type = static_cast<ArrayType*>(from_type);

        auto elm_to_type   = arr_to_type->get_elm_type();
        auto elm_from_type = arr_from_type->get_elm_type();

        if (arr_to_type->get_length() != arr_from_type->get_length()) {
            return false;
        }

        return check_bind_type_to_generic_type(elm_to_type, elm_from_type, bindings, true);
    }
    case TypeKind::Function: {
        if (!from_type->is_function()) {
            return false;
        }

        auto func_to_type   = static_cast<FunctionType*>(to_type);
        auto func_from_type = static_cast<FunctionType*>(from_type);

        auto& to_param_types   = func_to_type->get_param_types();
        auto& from_param_types = func_from_type->get_param_types();

        if (to_param_types.size() != from_param_types.size()) {
            return false;
        }

        auto to_return_type   = func_to_type->get_return_type();
        auto from_return_type = func_from_type->get_return_type();

        if (!check_bind_type_to_generic_type(to_return_type, from_return_type, bindings)) {
            return false;
        }

        for (size_t i = 0; i < to_param_types.size(); i++) {
            auto to_param_type   = to_param_types[i];
            auto from_param_type = from_param_types[i];
            if (!check_bind_type_to_generic_type(to_param_type, from_param_type, bindings)) {
                return false;
            }
        }

        if (func_to_type->uses_native_varargs() != func_from_type->uses_native_varargs()) {
            return false;
        }

        auto& to_raised_errors   = func_to_type->get_raised_errors();
        auto& from_raised_errors = func_from_type->get_raised_errors();

        if (to_raised_errors.size() != from_raised_errors.size()) {
            return false;
        }

        for (size_t i = 0; i < to_raised_errors.size(); i++) {
            auto& to_raised_error   = to_raised_errors[i];
            auto& from_raised_error = from_raised_errors[i];
            if (to_raised_error.structn != from_raised_error.structn) {
                return false;
            }
        }

        return true;
    }
    case TypeKind::PartiallyBoundStruct: {
        if (!from_type->is_struct()) {
            return false;
        }

        auto partially_bound_struct_type = static_cast<PartiallyBoundStructType*>(to_type);
        auto from_struct_type            = static_cast<StructType*>(from_type);

        auto to_unbound_generic_struct = partially_bound_struct_type->get_unbound_generic_struct();
        auto from_struct = from_struct_type->get_struct();
        // Make sure we are effectively referring to the same struct.
        if (from_struct->nspace != to_unbound_generic_struct->nspace) {
            return false;
        }

        auto& partially_bound_types = partially_bound_struct_type->get_partially_bound_types();
        if (partially_bound_types.empty()) {
            // Checking if the bindings happens to be filled up for the entirety of the
            // struct's generics, and if so, then we can fully qualify the generic struct
            // and assume assignment.

            for (size_t i = 0; i < to_unbound_generic_struct->generics.size(); i++) {
                if (!bindings[i]) {
                    return false;
                }
            }

            return true;
        }

        // TODO (maddie): else deal with there being bound types that are generic.
        acorn_fatal("Not implemented yet");
        return false;
    }
    default:
        acorn_fatal("Unreachable. Hit type that cannot contain a generic");
        return false;
    }
}

void acorn::Sema::check_cast(Cast* cast) {
    auto fixed_cast_type = fixup_type(cast->explicit_cast_type);
    if (!fixed_cast_type) {
        return;
    }
    if (fixed_cast_type == context.indeterminate_type) {
        cast->type = context.indeterminate_type;
        return;
    }


    if (is_incomplete_type(fixed_cast_type)) {
        error(expand(cast), "Cannot cast to incomplete type '%s'", fixed_cast_type)
                .end_error(ErrCode::SemaCannotCastIncompleteType);
        return;
    }

    cast->type = fixed_cast_type;

    check_and_verify_type(cast->value);
    if (!is_castable_to(fixed_cast_type, cast->value)) {
        error(expand(cast), "Cannot cast from '%s' to '%s'",
              cast->value->type, fixed_cast_type)
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

        if (elm->is(NodeKind::Array) && dest_elm_type &&
            (dest_elm_type->get_kind() == TypeKind::Array || dest_elm_type->get_kind() == TypeKind::AssignDeterminedArray)) {
            auto next_dest_elm_type = static_cast<ContainerType*>(dest_elm_type)->get_elm_type();
            check_array(static_cast<Array*>(elm), next_dest_elm_type);
        } else {
            check_node(elm);
        }

        if (!elm->type) {
            values_have_errors = true;
            continue;
        }

        if (dest_elm_type) {
            // Checking that the type is not an `AssignDeterminedArray` because it
            // is possible elements of arrays to be assigned to the assign determined
            // arrays. The checks for dimensional compatibility are checked later when
            // the type is fixed up.
            //
            if (dest_elm_type->get_kind() != TypeKind::AssignDeterminedArray) {
                if (!is_assignable_to(dest_elm_type, elm)) {
                    error(expand(elm), "%s", get_type_mismatch_error(dest_elm_type, elm))
                        .end_error(ErrCode::SemaIncompatibleArrayElmTypes);
                    values_have_errors = true;
                }
            }
        } else if (!elm_type) {
            elm_type = elm->type;
            value_for_elm_type = elm;
        } else if (elm_type->is_not(elm->type)) {
            if (!is_assignable_to(elm_type, elm)) {
                // Check the reverse case.
                if (!is_assignable_to(elm->type, value_for_elm_type)) {
                    error(expand(elm), "Incompatible element types. %s",
                          get_type_mismatch_error(elm_type, elm))
                        .end_error(ErrCode::SemaIncompatibleArrayElmTypes);
                    values_have_errors = true;
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
        if (dest_elm_type->get_kind() != TypeKind::AssignDeterminedArray) {
            elm_type = dest_elm_type;
        } else {
            elm_type = arr->elms[0]->type;
        }
    }

    if (is_incomplete_type(elm_type)) {
        error(expand(arr), "Array has incomplete element type '%s'", elm_type)
            .end_error(ErrCode::SemaArrayIncompleteElmType);
        return;
    }

    for (Expr* elm : arr->elms) {
        if (elm) {
            create_cast(elm, elm_type);

            // Must go after after cast because casting may change foldability
            if (!elm->is_foldable) {
                arr->is_foldable = false;
            }
        }
    }

    arr->type = type_table.get_arr_type(elm_type, arr->elms.size());
}

void acorn::Sema::check_memory_access(MemoryAccess* mem_access) {
    check_and_verify_type(mem_access->site);
    check_and_verify_type(mem_access->index);

    mem_access->is_foldable = false;

    Type* access_type = mem_access->site->type;
    if (!(access_type->is_array() || access_type->is_pointer() || access_type->is_slice())) {
        if (access_type == context.expr_type) {
            auto elm_type = get_type_of_type_expr(mem_access->site);

            auto unresolved_type = UnresolvedArrayType::create(context.get_allocator(), elm_type, mem_access->index);
            auto fixed_expr_type = fixup_unresolved_array_type(unresolved_type);
            if (!fixed_expr_type) {
                return;
            }

            mem_access->expr_type = fixed_expr_type;
            mem_access->type = context.expr_type;
            return;
        }
        error(mem_access, "Cannot index memory of type '%s'", access_type)
            .end_error(ErrCode::SemaMemoryAccessBadType);
    } else {
        auto ctr_type = static_cast<ContainerType*>(access_type);
        mem_access->type = ctr_type->get_elm_type();
    }

    if (!mem_access->index->type->is_integer()) {
        error(expand(mem_access->index), "Expected index of memory access to be an integer")
            .end_error(ErrCode::SemaMemoryIndexNotInteger);
    } else {
        create_cast(mem_access->index, context.usize_type);
    }
}

void acorn::Sema::check_ternary(Ternary* ternary) {
    check_node(ternary->cond);
    if (ternary->cond->type) {
        if (!check_is_condition(ternary->cond)) {
            return;
        }
    }

    check_node(ternary->lhs);
    check_node(ternary->rhs);

    if (!ternary->lhs->type || !ternary->rhs->type) {
        return;
    }
    if (!ternary->lhs->is_foldable || !ternary->rhs->is_foldable) {
        ternary->is_foldable = false;
    }

    Type* lhs_type = ternary->lhs->type;
    Type* rhs_type = ternary->rhs->type;
    if (rhs_type->is_const()) {
        rhs_type = type_table.remove_const(rhs_type);
    }
    if (lhs_type->is_const()) {
        lhs_type = type_table.remove_const(lhs_type);
    }

    if (lhs_type->is_not(rhs_type)) {
        error(expand(ternary), "Operator ? has incompatible types '%s' and '%s'",
              ternary->lhs->type, ternary->rhs->type)
            .end_error(ErrCode::SemaTernaryIncompatibleTypes);
        return;
    }

    ternary->type = lhs_type;
}

void acorn::Sema::check_type_expr(TypeExpr* type_expr) {
    if (Type* fixed_type = fixup_type(type_expr->parsed_expr_type)) {
        type_expr->expr_type = fixed_type;
        type_expr->type = context.expr_type;
    }
}

void acorn::Sema::check_reflect(Reflect* reflect) {
    if (context.should_stand_alone()) {
        error(reflect, "Cannot reflect when stand alone is enabled")
            .end_error(ErrCode::SemaCannotReflectWhenStandAlone);
        return;
    }

    check_and_verify_type(reflect->expr);

    switch (reflect->reflect_kind) {
    case ReflectKind::TypeInfo: {

        if (reflect->expr->type == context.expr_type) {
            reflect->type_info_type = get_type_of_type_expr(reflect->expr);
        } else {
            reflect->type_info_type = reflect->expr->type;
        }

        if (is_incomplete_type(reflect->type_info_type)) {
            error(expand(reflect), "Cannot get type information of incomplete type '%s'",
                  reflect->type_info_type)
                .end_error(ErrCode::SemaCannotGetTypeInfoIncompleteType);
            return;
        }

        reflect->type = context.const_std_type_ptr;
        // TODO: come back to we will need some way for accessing the fields to be considered foldable.
        reflect->is_foldable = false;

        break;
    }
    default:
        acorn_fatal("Reflection kind not implemented");
    }
}

void acorn::Sema::check_struct_initializer(StructInitializer* initializer) {

    auto get_composite = [this](IdentRef* ref) -> Struct* {
        auto name = ref->ident;
        auto composite = find_composite(name);

        if (!composite) {
            error(ref, "Failed to find struct type '%s'", name)
                .end_error(ErrCode::SemaStructInitFailedToFindStruct);
            return nullptr;
        }

        if (composite->is_not(NodeKind::Struct)) {
            error(ref, "Expected to find struct for initializer but found %s",
                  composite->get_composite_kind())
                .end_error(ErrCode::SemaWrongCompositeKindForStructInitializer);
            return nullptr;
        }

        return static_cast<Struct*>(composite);
    };

    UnboundGenericStruct* unbound_generic_struct = nullptr;
    llvm::SmallVector<Type*> bound_types;
    Struct* structn = nullptr;
    if (initializer->site->is(NodeKind::IdentRef)) {

        auto ref = static_cast<IdentRef*>(initializer->site);
        structn = get_composite(ref);
        if (!structn) {
            return;
        }

        if (!structn->is_generic) {
            if (!ensure_struct_checked(ref->loc, structn)) {
                return;
            }
        } else {
            unbound_generic_struct = static_cast<UnboundGenericStruct*>(structn);
        }
    } else {
        auto call = static_cast<FuncCall*>(initializer->site);
        auto ref = static_cast<IdentRef*>(call->site);
        structn = get_composite(ref);
        if (!structn) {
            return;
        }

        if (!get_bound_types_for_generic_type(structn, ref->loc, call->args, bound_types)) {
            return;
        }
        unbound_generic_struct = static_cast<UnboundGenericStruct*>(structn);
    }

    bool args_have_errors = false;
    for (auto arg : initializer->values) {
        if (arg->is(NodeKind::MoveObj)) {
            check_moveobj(static_cast<MoveObj*>(arg), true);
        } else {
            check_node(arg);
        }
        if (!arg->type) args_have_errors = true;
    }
    if (args_have_errors) return;


    if (!structn->constructors.empty()) {

        Func* found_constructor = check_function_decl_call(initializer,
                                                           initializer->values,
                                                           initializer->non_named_vals_offset,
                                                           structn->constructors,
                                                           false,
                                                           std::move(bound_types),
                                                           unbound_generic_struct,
                                                           structn);
        if (!found_constructor) {
            return;
        }

        initializer->structn = structn;
        initializer->is_foldable = false;
        initializer->called_constructor = found_constructor;
        initializer->type = structn->struct_type;

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

    if (structn->is_generic) {
        if (bound_types.empty()) {
            error(initializer, "Expected generics arguments to bind to the generic struct")
                .end_error(ErrCode::SemaExpectedGenericsToBindToGenericStruct);
            return;
        } else if (bound_types.size() < unbound_generic_struct->generics.size()) {
            error(initializer, "Too few generics arguments to bind to the generic struct")
                .end_error(ErrCode::SemaTooFewGenericArgumentsForStruct);
            return;
        }

        structn = unbound_generic_struct->get_generic_instance(context.get_allocator(), std::move(bound_types));

        if (!ensure_struct_checked(initializer->loc, structn)) {
            return;
        }
    }

    bool named_values_out_of_order = false;
    uint32_t named_value_high_idx = 0;
    for (size_t i = 0; i < values.size(); i++) {
        Expr* value = values[i];

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

        if (field->assignment) {
            initializer->is_foldable &= field->assignment->is_foldable;
        } else {
            initializer->is_foldable &= field->type->is_default_foldable();
        }

        create_cast(value, field->type);
    }


    if (!structn->is_default_foldable) {
        initializer->is_foldable = false;
    }

    initializer->structn = structn;
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

    check_and_verify_type(sof->value);
    if (sof->value->type->get_kind() == TypeKind::Expr) {
        sof->type_with_size = get_type_of_type_expr(sof->value);
    } else {
        sof->type_with_size = sof->value->type;
    }

    if (is_incomplete_type(sof->type_with_size)) {
        error(expand(sof), "Cannot get size of unsized type '%s'", sof->type_with_size)
            .end_error(ErrCode::SemaCannotGetSizeOfUnsizedType);
        return;
    }

    sof->type = context.int_type;
}

void acorn::Sema::check_moveobj(MoveObj* move_obj, bool has_destination_address) {
    check_and_verify_type(move_obj->value);

    if (!is_lvalue(move_obj->value)) {
        error(expand(move_obj), "moveobj expects the value to have an address")
            .end_error(ErrCode::SemaMoveObjValueMustHaveAddress);
    }

    if (!has_destination_address) {
        error(expand(move_obj), "moveobj must be assigned to an address")
            .end_error(ErrCode::SemaMoveObjMustBeAssignedToAddress);
    }

    move_obj->type = move_obj->value->type;
    move_obj->is_foldable = false;
}

bool acorn::Sema::check_modifiable(Expr* expr, Expr* error_node, bool is_assignment) {

    if (!is_lvalue(expr)) {
        error(expand(error_node), "Expected to be a modifiable value")
            .end_error(ErrCode::SemaExpectedModifiable);
        return false;
    }

    if (expr->type->is_const()) {
        if (expr->is(NodeKind::IdentRef) &&
            static_cast<IdentRef*>(expr)->found_kind == IdentRef::VarKind) {

            auto ref = static_cast<IdentRef*>(expr);
            if (ref->var_ref->is_field() && !ref->var_ref->type->is_const() &&
                cur_func && cur_func->is_constant) {
                error(expand(error_node), "Cannot %s field in a const function",
                      is_assignment ? "reassign to" : "modify a")
                    .end_error(ErrCode::SemaReassignConstVariable);
            } else {
                error(expand(error_node), "Cannot %s a const variable", is_assignment ? "reassign to" : "modify")
                    .end_error(ErrCode::SemaReassignConstVariable);
            }

            return false;
        } else {
            error(expand(error_node), "Cannot %s a const address", is_assignment ? "assign to" : "modify")
                .end_error(ErrCode::SemaReassignConstAddress);
            return false;
        }
    }
    if (is_readonly_field_without_access(expr)) {
        auto dot = static_cast<DotOperator*>(expr);
        error(expand(error_node), "Cannot %s field '%s', it is marked readonly",
              is_assignment ? "assign to" : "modify", dot->var_ref->name)
            .end_error(ErrCode::SemaCannotAssignToReadonly);
        return false;
    }

    return true;
}

acorn::Expr* acorn::Sema::try_create_bound_expr_for_variable_with_indetermite_type(Var* param) {

    Expr* copied_assignment = deep_copy_expr(context.get_allocator(), param->assignment);
    Expr* unbound_assignment = param->assignment;
    param->assignment = copied_assignment;

    check_variable(param);
    if (!param->type) {
        param->assignment = unbound_assignment;
        return nullptr;
    }

    param->assignment = unbound_assignment;
    return copied_assignment;
}

void acorn::Sema::check_division_by_zero(Node* error_node, Expr* expr) {
    if (!expr->is_foldable) return;

    if (auto ll_constant = gen_constant(expr)) {
        if (!ll_constant->isZeroValue()) return;

        error(error_node, "Division by zero")
            .end_error(ErrCode::SemaDivisionByZero);
    }
}

bool acorn::Sema::check_is_condition(Expr* cond) {
    if (!is_condition(cond->type)) {
        error(expand(cond), "Expected condition")
            .end_error(ErrCode::SemaExpectedCondition);
        return false;
    }
    return true;
}

bool acorn::Sema::check_comptime_cond(Expr* cond, const char* comptime_type_str) {
    is_comptime_if_cond = true;
    check_node(cond);
    if (!cond->type) {
        return false;
    }

    if (!cond->is_foldable) {
        error(expand(cond), "Directive %s expects the condition to be determined at compile time", comptime_type_str)
            .end_error(ErrCode::SemaNotComptimeCompute);
        return false;
    }

    if (!is_condition(cond->type)) {
        return false;
    }

    auto ll_cond = llvm::cast<llvm::ConstantInt>(gen_constant(cond));
    if (!ll_cond) {
        return false;
    }
    return !ll_cond->isZero();
}


// Type fixup
//--------------------------------------

acorn::Type* acorn::Sema::fixup_type(Type* type, bool is_ptr_elm_type) {
    if (type->get_kind() == TypeKind::UnresolvedArray) {
        return fixup_unresolved_array_type(type);
    } else if (type->get_kind() == TypeKind::UnresolvedComposite) {
        return fixup_unresolved_composite_type(type, is_ptr_elm_type);
    } else if (type->get_kind() == TypeKind::UnresolvedGenericComposite) {
        return fixup_unresolved_generic_composite_type(type, is_ptr_elm_type);
    } else if (type->get_kind() == TypeKind::UnresolvedEnumValueType) {
        return fixup_unresolved_enum_value_type(type, is_ptr_elm_type);
    } else if (type->get_kind() == TypeKind::Function) {
        return fixup_function_type(type);
    } else if (type->is_pointer()) {

        auto ptr_type = static_cast<PointerType*>(type);
        auto elm_type = ptr_type->get_elm_type();
        auto fixed_elm_type = fixup_type(elm_type, true);
        if (!fixed_elm_type) {
            return nullptr;
        }

        if (fixed_elm_type == elm_type) {
            return ptr_type;
        } else {
            Type* fixed_ptr_type = type_table.get_ptr_type(fixed_elm_type);
            if (ptr_type->is_const()) {
                return type_table.get_const_type(fixed_ptr_type);
            }
            return fixed_ptr_type;
        }
    } else if (type->is_slice()) {

        auto slice_type = static_cast<SliceType*>(type);
        auto elm_type = slice_type->get_elm_type();
        auto fixed_elm_type = fixup_type(elm_type);
        if (!fixed_elm_type) {
            return nullptr;
        }

        if (fixed_elm_type == elm_type) {
            return slice_type;
        } else {
            Type* fixed_slice_type = type_table.get_slice_type(fixed_elm_type);
            if (slice_type->is_const()) {
                return type_table.get_const_type(fixed_slice_type);
            }
            return fixed_slice_type;
        }
    } else if (type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(type);
        auto elm_type = arr_type->get_elm_type();
        auto fixed_elm_type = fixup_type(elm_type);
        if (!fixed_elm_type) {
            return nullptr;
        }

        if (fixed_elm_type == elm_type) {
            return arr_type;
        } else {
            Type* fixed_arr_type = type_table.get_arr_type(fixed_elm_type, arr_type->get_length());
            if (arr_type->is_const()) {
                return type_table.get_const_type(fixed_arr_type);
            }
            return fixed_arr_type;
        }
    } else if (type->is_generic()) {
        return fixup_generic_type(type);
    } else if (type->get_kind() == TypeKind::PartiallyBoundStruct) {
        return fixup_partially_bound_struct_type(type, func_call_generic_bindings);
    } else {
        return type;
    }
}

acorn::Type* acorn::Sema::fixup_unresolved_array_type(Type* type) {
    auto unresolved_type = static_cast<UnresolvedArrayType*>(type);
    Expr* expr = unresolved_type->get_expr();

    Type* fixed_elm_type = fixup_type(unresolved_type->get_elm_type());
    if (!fixed_elm_type) {
        return nullptr;
    }

    check_node(expr);
    if (!expr->type) {
        // Failed to check the length expression so returning early.
        return nullptr;
    }

    if (!expr->type->is_integer()) {
        error(expand(expr), "Array length must be an integer type")
            .end_error(ErrCode::SemaArrayLengthNotInteger);
        return nullptr;
    }

    if (expr->type->get_number_of_bits() > 32) {
        error(expand(expr), "Array length must be less than or equal to a 32 bit integer")
            .end_error(ErrCode::SemaArrayLengthTooLargeType);
        return nullptr;
    }

    if (!expr->is_foldable) {
        error(expand(expr), "Array length must be able to be determined at compile time")
            .end_error(ErrCode::SemaArrayLengthNotComptime);
        return nullptr;
    }

    if (auto ll_length = gen_constant(expr)) {
        auto ll_int_length = llvm::cast<llvm::ConstantInt>(ll_length);
        uint32_t length = static_cast<uint32_t>(ll_int_length->getZExtValue());

        if (length == 0) {
            error(expand(expr), "Array length cannot be zero")
                .end_error(ErrCode::SemaArrayLengthZero);
            return nullptr;
        }

        if (expr->type->is_signed() && static_cast<int32_t>(length) < 0) {
            error(expand(expr), "Array length cannot be negative")
                .end_error(ErrCode::SemaArrayLengthNegative);
            return nullptr;
        }

        // Everything is okay we can create a new array out of it!
        auto fixed_type = type_table.get_arr_type(fixed_elm_type, length);
        if (type->is_const()) {
            return type_table.get_const_type(fixed_type);
        }
        return fixed_type;
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
        error(expand(var), "Wrong array dimensions for '%s', found '%s'",
                type, from_type)
            .end_error(ErrCode::SemaAssignDetArrWrongDimensions);
    };

    ContainerType* ctr_type = assign_det_arr_type;
    while (elm_type->get_kind() == TypeKind::AssignDeterminedArray  ||
           elm_type->get_kind() == TypeKind::Array) {
        if (from_elm_type->get_kind() != TypeKind::Array) {
            report_dimensions_error();
            return nullptr;
        }

        ctr_type = static_cast<ContainerType*>(elm_type);
        elm_type = ctr_type->get_elm_type();

        arr_type = static_cast<ArrayType*>(from_elm_type);
        from_elm_type = arr_type->get_elm_type();

        if (ctr_type->is_array()) {
            auto to_arr_type = static_cast<ArrayType*>(ctr_type);
            if (to_arr_type->get_length() != arr_type->get_length()) {
                // Return the given type so it says the types could not
                // be assigned to each other.
                return type;
            }
        }

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

acorn::Type* acorn::Sema::fixup_unresolved_composite_type(Type* type, bool is_ptr_elm_type) {

    auto unresolved_composite_type = static_cast<UnresolvedCompositeType*>(type);

    auto name = unresolved_composite_type->get_composite_name();
    auto error_loc = unresolved_composite_type->get_error_location();

    auto found_composite = find_composite_for_composite_type(name, error_loc);
    if (!found_composite) {
        return nullptr;
    }

    switch (found_composite->kind) {
    case NodeKind::Struct: {
        auto found_struct = static_cast<Struct*>(found_composite);

        if (found_struct->is_generic) {
            // Checking if within a member function, and if its a member function
            // of the struct being declared. If so, then the generic struct type
            // can take on the generics of the given struct.
            if (cur_func && cur_func->non_generic_struct_instance == found_struct) {
                Type* struct_type = cur_func->structn->struct_type;
                if (unresolved_composite_type->is_const()) {
                    struct_type = type_table.get_const_type(struct_type);
                }
                return struct_type;
            } else if (cur_func_decl && cur_func_decl->non_generic_struct_instance == found_struct) {

                // When there are no bound types it can be treated as if the type has
                // generic types but none of them where bound.

                auto unbound_generic_struct = static_cast<UnboundGenericStruct*>(found_struct);
                auto partially_bound_struct_type = PartiallyBoundStructType::create(context.get_allocator(),
                                                                                    unbound_generic_struct,
                                                                                    {});

                if (unresolved_composite_type->is_const()) {
                    return type_table.get_const_type(partially_bound_struct_type);
                }
                return partially_bound_struct_type;
            }

            error(error_loc, "Expected generic arguments for generic type")
                .end_error(ErrCode::SemaExpectedGenericArgsForGenericType);
            return nullptr;
        }

        if (!ensure_struct_checked(unresolved_composite_type->get_error_location(), found_struct)) {
            return nullptr;
        }

        Type* struct_type = found_struct->struct_type;
        if (unresolved_composite_type->is_const()) {
            struct_type = type_table.get_const_type(struct_type);
        }

        return struct_type;
    }
    case NodeKind::Enum: {
        auto found_enum = static_cast<Enum*>(found_composite);

        ensure_enum_checked(unresolved_composite_type->get_error_location(), found_enum);

        Type* enum_type = found_enum->enum_type;
        if (unresolved_composite_type->is_const()) {
            enum_type = type_table.get_const_type(enum_type);
        }

        return enum_type;
    }
    case NodeKind::Interface: {
        auto found_interface = static_cast<Interface*>(found_composite);

        ensure_interface_checked(unresolved_composite_type->get_error_location(), found_interface);

        Type* intr_type = found_interface->interface_type;
        if (unresolved_composite_type->is_const()) {
            intr_type = type_table.get_const_type(intr_type);
        }

        if (!is_ptr_elm_type) {
            error(unresolved_composite_type->get_error_location(), "Interface types should be declared a pointer")
                .add_line("Use type '%s*' instead", unresolved_composite_type->get_composite_name())
                .end_error(ErrCode::SemaInterfaceMustBeDeclAsPtr);
            return nullptr;
        }

        return intr_type;
    }
    default:
        acorn_fatal("Unknown composite kind");
        return nullptr;
    }
}

acorn::Type* acorn::Sema::fixup_unresolved_generic_composite_type(Type* type, bool is_ptr_elm_type) {

    auto unresolved_composite_type = static_cast<UnresolvedGenericCompositeType*>(type);

    auto name = unresolved_composite_type->get_composite_name();
    auto error_loc = unresolved_composite_type->get_error_location();

    auto found_composite = find_composite_for_composite_type(name, error_loc);
    if (!found_composite) {
        return nullptr;
    }

    const llvm::SmallVector<Expr*>& bound_exprs = unresolved_composite_type->get_bound_exprs();

    Type* struct_type = fixup_unresolved_generic_composite_type(found_composite, error_loc, bound_exprs);;
    if (type->is_const()) {
        return type_table.get_const_type(struct_type);
    }
    return struct_type;
}

acorn::Type* acorn::Sema::fixup_unresolved_generic_composite_type(Decl* found_composite,
                                                                  SourceLoc error_loc,
                                                                  const llvm::SmallVector<Expr*>& bound_exprs) {



    // TODO (maddie): with is generic types nonsense and generally having to fixup the type information of the bound
    // types then setting if this type contains generics.

    llvm::SmallVector<Type*> bound_types;
    if (!get_bound_types_for_generic_type(found_composite, error_loc, bound_exprs, bound_types)) {
        return nullptr;
    }

    auto unbound_generic_struct = static_cast<UnboundGenericStruct*>(found_composite);

    if (bound_exprs.size() < unbound_generic_struct->generics.size()) {
        error(error_loc, "Too few generic arguments for struct. Got %s but expected %s",
              bound_exprs.size(), unbound_generic_struct->generics.size())
            .end_error(ErrCode::SemaTooFewGenericArgumentsForStruct);
        return nullptr;
    }

    auto new_struct = unbound_generic_struct->get_generic_instance(context.get_allocator(), std::move(bound_types));

    if (!ensure_struct_checked(error_loc, new_struct)) {
        return nullptr;
    }

    return new_struct->struct_type;
}

bool acorn::Sema::get_bound_types_for_generic_type(Decl* found_composite,
                                                   SourceLoc error_loc,
                                                   const llvm::SmallVector<Expr*>& bound_exprs,
                                                   llvm::SmallVector<Type*>& bound_types) {
    // TODO (maddie): expand error location so things make more sense in error logging.

    if (bound_exprs.empty()) {
        error(error_loc, "Expected generic arguments for generic type")
            .end_error(ErrCode::SemaExpectedGenericArgsForGenericType);
        return false;
    }

    bool uses_named_args_values;
    if (found_composite->kind != NodeKind::Struct) {
        error(error_loc, "Type does not take generic arguments")
            .end_error(ErrCode::SemaTypeDoesNotTakeGenerics);
        return false;
    }

    auto found_struct = static_cast<Struct*>(found_composite);

    if (!found_struct->is_generic) {
        error(error_loc, "Type does not take generic arguments")
            .end_error(ErrCode::SemaTypeDoesNotTakeGenerics);
        return false;
    }

    auto unbound_generic_struct = static_cast<UnboundGenericStruct*>(found_struct);

    auto& generics = unbound_generic_struct->generics;

    if (bound_exprs.size() > unbound_generic_struct->generics.size()) {
        error(error_loc, "Too many generic arguments for struct. Got %s but expected %s",
              bound_exprs.size(), unbound_generic_struct->generics.size())
            .end_error(ErrCode::SemaTooManyGenericArgumentsForStruct);
        return false;
    }

    if (!check_generic_bind_arguments(bound_exprs, uses_named_args_values)) {
        return false;
    }

    if (uses_named_args_values) {
        if (compare_generic_bind_candidate_with_named_args(bound_exprs, generics, bound_types)) {
            // binds the types during the call.
        } else {
            Logger& logger = error(error_loc, "Failed to bind generic arguments");
            logger.add_empty_line();
            display_generic_bind_named_args_fail_info(bound_exprs, generics);
            logger.end_error(ErrCode::SemaFailedToBindGenerics);
            return false;
        }
    } else {
        for (Expr* arg : bound_exprs) {
            Type* bind_type = get_type_of_type_expr(arg);
            bound_types.push_back(bind_type);
        }
    }

    return true;
}

acorn::Type* acorn::Sema::fixup_unresolved_enum_value_type(Type* type, bool is_ptr_elm_type) {

    auto unresolved_enum_value_type = static_cast<UnresolvedEnumValueType*>(type);

    auto name = unresolved_enum_value_type->get_enum_name();
    auto error_loc = unresolved_enum_value_type->get_error_location();

    auto found_composite = find_composite_for_composite_type(name, error_loc);
    if (!found_composite) {
        return nullptr;
    }

    if (found_composite->is_not(NodeKind::Enum)) {
        error(error_loc, "Enum value type expects enum type")
            .end_error(ErrCode::SemaEnumValueTypeExpectsEnum);
        return nullptr;
    }

    auto found_enum = static_cast<Enum*>(found_composite);

    ensure_enum_checked(error_loc, found_enum);

    auto fixed_type = type_table.get_enum_container_type(found_enum->enum_type);
    if (type->is_const()) {
        return type_table.get_const_type(fixed_type);
    }
    return fixed_type;
}

acorn::Decl* acorn::Sema::find_composite_for_composite_type(Identifier name, SourceLoc error_loc) {

    auto found_composite = find_composite(name);

    if (!found_composite) {
        ErrorSpellChecker spell_checker(context.should_show_spell_checking());
        spell_checker.add_searches(file->get_namespace()->get_composites());
        spell_checker.add_searches(file->get_composites());
        for (auto& [import_key, importn] : file->get_imports()) {
            if (importn->is_imported_composite()) {
                spell_checker.add_search(import_key);
            }
        }

        auto end = error_loc.end();

        // HACK
        //
        // Checking if the user possibly mistyped common mistakes that happen when
        // writing code to assume it was a parsing mistake instead.

        auto try_detect_multiply_parse_error = [this, &spell_checker, name]
            (const char* end, SourceLoc error_loc) finline {
            if (*end == '*') {
                auto star_loc = end;
                ++end;
                while (is_whitespace(*end))  ++end;
                if (*end == ';' || *end == ']' || *end == ')') {
                    // could be something like `int a = b *;` in which case we will tell the
                    // user that it expected an expression instead.

                    logger.begin_error(error_loc, "Tried to interpret '%s' as a type but may be part of an incomplete expression",
                                       name);
                    SourceLoc arrow_loc = {
                        .ptr = star_loc,
                        .length = 1
                    };
                    logger.add_arrow_msg_alongside("expression after multiply?", arrow_loc);
                    spell_checker.search(logger, name);
                    logger.end_error(ErrCode::ParseExpectedExpression);
                    return true;
                }
            }

            return false;
        };

        while (true) {
            while (is_whitespace(*end))  ++end;
            if (try_detect_multiply_parse_error(end, error_loc)) {
                return nullptr;
            } else if (*end == '[') {
                go_until(end, '[', ']');
                if (try_detect_multiply_parse_error(end, error_loc)) {
                    return nullptr;
                }
            } else {
                // Did not detect that the user tried to type an expression instead.
                break;
            }
        }

        logger.begin_error(error_loc, "Could not find struct, enum, or interface type '%s'", name);
        spell_checker.search(logger, name);
        logger.end_error(ErrCode::SemaCouldNotFindStructType);
        return nullptr;
    }

    return found_composite;
}

acorn::Type* acorn::Sema::fixup_function_type(Type* type) {
    auto func_type = static_cast<FunctionType*>(type);

    llvm::SmallVector<Type*, 8> fixed_param_types;
    llvm::SmallVector<RaisedError> fixed_raised_errors;
    fixed_raised_errors.reserve(func_type->get_raised_errors().size());
    bool type_needed_fixing = false;

    auto ret_type = func_type->get_return_type();
    auto fixed_ret_type = fixup_type(ret_type);
    if (!fixed_ret_type) {
        return nullptr;
    }

    if (fixed_ret_type != ret_type) {
        type_needed_fixing = true;
    }

    for (auto param_type : func_type->get_param_types()) {
        auto fixed_param_type = fixup_type(param_type);
        if (!fixed_param_type) {
            return nullptr;
        }

        fixed_param_types.push_back(fixed_param_type);
        if (fixed_param_type != param_type) {
            type_needed_fixing = true;
        }
    }

    type_needed_fixing |= !func_type->get_raised_errors().empty();

    for (auto& raised_error : func_type->get_raised_errors()) {
        if (!check_raised_error(raised_error)) {
            return nullptr;
        }
        fixed_raised_errors.push_back(raised_error);
        raised_error.structn = nullptr;
    }

    if (type_needed_fixing) {
        return type_table.get_function_type(fixed_ret_type,
                                            std::move(fixed_param_types),
                                            std::move(fixed_raised_errors),
                                            func_type->uses_native_varargs());
    }

    return type;
}

acorn::Type* acorn::Sema::fixup_generic_type(Type* type) {
    auto generic_type = static_cast<GenericType*>(type);
    if (func_call_generic_bindings) {
        size_t generic_index = generic_type->get_generic_index();
        return (*func_call_generic_bindings)[generic_index];
    } else if (cur_func && cur_func->is_generic()) {
        // Get the bound type of the current generic state.
        size_t generic_index = generic_type->get_generic_index();
        return cur_func->generic_instance->bound_types[generic_index];
    } else if (cur_struct && cur_struct->is_generic) {
        auto generic_instance = static_cast<GenericStructInstance*>(cur_struct);
        size_t generic_index = generic_type->get_generic_index();
        return generic_instance->bound_types[generic_index];
    }
    return type;
}

acorn::Type* acorn::Sema::fixup_partially_bound_struct_type(Type* type, const llvm::SmallVector<Type*>* bound_types) {
    if (bound_types) {
        auto partially_bound_struct_type = static_cast<PartiallyBoundStructType*>(type);
        auto& partially_bound_types = partially_bound_struct_type->get_partially_bound_types();
        auto unbound_generic_struct = partially_bound_struct_type->get_unbound_generic_struct();

        llvm::SmallVector<Type*> fixed_bound_types;
        if (partially_bound_types.empty()) {
            fixed_bound_types.insert(fixed_bound_types.begin(),
                                     bound_types->begin(),
                                     bound_types->begin() + unbound_generic_struct->generics.size());

        } else {
            fixed_bound_types.reserve(unbound_generic_struct->generics.size());
            for (auto bound_type : partially_bound_types) {
                auto fixed_bound_type = fixup_type(bound_type);
                if (!fixed_bound_type) {
                    return nullptr;
                }

                fixed_bound_types.push_back(fixed_bound_type);
            }
        }

        auto new_struct = unbound_generic_struct->get_generic_instance(context.get_allocator(),
                                                                       std::move(fixed_bound_types));

        if (!ensure_struct_checked({}, new_struct)) {
            return nullptr;
        }

        if (partially_bound_struct_type->is_const()) {
            return type_table.get_const_type(new_struct->struct_type);
        }
        return new_struct->struct_type;
    }
    return type;
}


// Error reporting
//--------------------------------------

void acorn::Sema::spellcheck_variables_for_ident(const llvm::SmallVector<Var*>& variables,
                                                 ErrorSpellChecker& spell_checker,
                                                 bool is_for_call) {
    if (!is_for_call) {
        // Not for a call so we might as well try and suggest the variables.
        spell_checker.add_searches(variables);
        return;
    }

    // Otherwise it is for a call so try and suggest
    // variables that have callable types.
    for (auto var : variables) {
        if (var->type && var->type->is_callable()) {
            spell_checker.add_search(var->name);
        }
    }
}

void acorn::Sema::spellcheck_variables_for_ident(const llvm::DenseMap<Identifier, Var*>& variables,
                                                 ErrorSpellChecker& spell_checker,
                                                 bool is_for_call) {
    if (!is_for_call) {
        // Not for a call so we might as well try and suggest the variables.
        spell_checker.add_searches(variables);
        return;
    }

    for (auto& [_, var] : variables) {
        if (var->type && var->type->is_callable()) {
            spell_checker.add_search(var->name);
        }
    }
}

void acorn::Sema::display_call_mismatch_info(PointSourceLoc error_loc,
                                             Node* call_node,
                                             const FuncList& candidates,
                                             const llvm::SmallVector<Expr*>& args,
                                             const llvm::SmallVector<Type*>& pre_bound_types) {

    const char* func_type_str = candidates[0]->is_constructor ? "constructor" : "function";

    if (candidates.size() == 1) {

        Func* canidate = candidates[0];

        logger.begin_error(error_loc, "Invalid call to %s: '%s'", func_type_str, canidate->get_decl_string());
        logger.add_empty_line();
        display_call_mismatch_info(canidate, args, false, true, call_node, pre_bound_types);
        logger.end_error(ErrCode::SemaInvalidFuncCallSingle);

    } else {

        llvm::SmallVector<std::pair<uint64_t, const Func*>> candidates_and_scores;
        for (const Func* candidate : candidates) {
            uint64_t score = get_function_call_score(candidate, args, false, pre_bound_types);
            candidates_and_scores.push_back(std::make_pair(score, candidate));
        }
        std::ranges::sort(candidates_and_scores, [](const auto& lhs, const auto& rhs) {
            return lhs.first < rhs.first;
        });

        logger.begin_error(error_loc, "Could not find a valid overloaded %s to call", func_type_str);
        for (auto& [_, candidate] : candidates_and_scores | std::views::take(context.get_max_call_err_funcs())) {
            logger.add_empty_line();
            logger.add_line("Could not match: '%s'", candidate->get_decl_string());
            display_call_mismatch_info(candidate, args, true, false, call_node, pre_bound_types);
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
                                             bool indent,
                                             bool should_show_invidual_underlines,
                                             Node* call_node,
                                             const llvm::SmallVector<Type*>& pre_bound_types) {

#define err_line(n, fmt, ...) \
add_error_line(n, should_show_invidual_underlines, indent, "%s- " fmt, ##__VA_ARGS__);

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
            if (!candidate->uses_native_varargs()) {
                return args.size() == num_params;
            } else {
                return args.size() >= num_params;
            }
        }
    };

    if (!check_correct_number_of_args()) {
        if constexpr (is_func_decl) {
            if (candidate->default_params_offset != -1) {
                size_t min_params = candidate->default_params_offset;
                err_line(nullptr,
                         "incorrect number of args. Expected between %s-%s but found %s",
                         min_params, num_params, args.size());
                return;
            }
        }

        if constexpr (is_func_decl) {
            if (candidate->uses_native_varargs || candidate->uses_varargs) {
                err_line(nullptr,
                         "incorrect number of args. Expected at least %s but found %s",
                         num_params - (candidate->uses_varargs ? 1 : 0), args.size());
                return;
            }
        }

        if constexpr (!is_func_decl) {
            if (candidate->uses_native_varargs()) {
                err_line(nullptr,
                         "incorrect number of args. Expected at least %s but found %s",
                         num_params, args.size());
                return;
            }
        }

        err_line(nullptr,
                 "incorrect number of args. Expected %s but found %s",
                 num_params, args.size());

        return;
    }

    // Checking const correctness.
    if constexpr (is_func_decl) {
        if (candidate->structn && !candidate->is_constant && !candidate->is_constructor) {
            auto call = static_cast<FuncCall*>(call_node);
            if (call->site->is(NodeKind::IdentRef) && cur_func) {
                // Calling one member function from another.
                if (cur_func->is_constant) {
                    err_line(nullptr, "cannot call non-const function from const function");
                    return;
                }
            } else {

                auto report_error = [this, &logger = this->logger, indent, should_show_invidual_underlines]
                    (DotOperator* dot) {
                    err_line(nullptr, "calling non-const function with const memory of type '%s'", dot->site->type);
                };

                auto dot = static_cast<DotOperator*>(call->site);
                if (dot->site->type->is_struct()) {
                    if (dot->site->type->is_const()) {
                        report_error(dot);
                        return;
                    }
                } else {
                    auto ptr_type = static_cast<PointerType*>(dot->site->type);
                    auto elm_type = ptr_type->get_elm_type();
                    if (elm_type->is_struct() && elm_type->is_const()) {
                        report_error(dot);
                        return;
                    }
                }
            }
        }

        if (candidate->has_modifier(Modifier::Private) && candidate->structn) {
            if (!cur_func || (cur_func->non_generic_struct_instance != candidate->non_generic_struct_instance)) {
                err_line(nullptr, "it is private");
                return;
            }
        }
    }

    auto get_param_type = [candidate](Var* param, size_t arg_idx) constexpr -> Type* {
        if constexpr (is_func_decl) {
            size_t last_index = candidate->params.size() - 1;
            if (candidate->uses_varargs && arg_idx >= last_index) {
                auto slice_type = static_cast<SliceType*>(param->type);
                return slice_type->get_elm_type();
            } else {
                return param->type;
            }
        } else {
            return candidate->get_param_types()[arg_idx];
        }
    };

    bool uses_native_varargs;
    if constexpr (is_func_decl) {
        uses_native_varargs = candidate->uses_native_varargs;
    } else {
        uses_native_varargs = candidate->uses_native_varargs();
    }

    size_t last_index = num_params - 1;

    // Pre checking some of the errors because these errors may cause conflict
    // and poor error reporting when trying to checking assignability.
    bool forwards_varargs = false;
    bool named_args_out_of_order = false;
    uint32_t named_arg_high_idx = 0;
    Expr* forwards_arg_value = nullptr;
    for (size_t i = 0; i < args.size(); i++) {

        Expr* arg_value = args[i];
        Var* param = nullptr;

        if (arg_value->is(NodeKind::NamedValue)) {

            if constexpr (is_func_decl) {
                auto named_arg = static_cast<NamedValue*>(arg_value);
                arg_value = named_arg->assignment;
                param = candidate->find_parameter(named_arg->name);

                if (!param) {
                    if (should_show_invidual_underlines)
                        logger.add_individual_underline(named_arg->get_name_location());
                    logger.add_line("%s- could not find param '%s' for named arg",
                                    indent ? "  " : "", named_arg->name);
                    return;
                }

                if (candidate->uses_varargs && i >= candidate->params.size() - 1) {
                    err_line(arg_value, "cannot use the parameter name of a variadic parameter");
                    return;
                }

                named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
                if (param->param_idx != i) {
                    named_args_out_of_order = true;
                }
            } else {
                err_line(arg_value, "cannot use named arguments when calling based on type");
                return;
            }
        } else {
            if constexpr (is_func_decl) {
                if (candidate->uses_varargs && i >= last_index) {
                    param = candidate->params[last_index];

                    if (forwards_varargs) {
                        auto last_param = candidate->params.back();
                        err_line(arg_value, "cannot pass arguments after forwarding variadic argument '%s'",
                                 last_param->name);
                        logger.add_arrow_msg_alongside("forwards here", forwards_arg_value->loc);
                        return;
                    }

                    // Checking for special condition in which a function passes its own variadic arguments
                    // to another function that takes variadic arguments.
                    if (cur_func && cur_func->forwards_varargs(arg_value)) {
                        if (i > last_index) {
                            auto last_param = candidate->params.back();
                            err_line(arg_value, "cannot forward variadic argument '%s' after passing other",
                                     last_param->name);
                            logger.remove_period();
                            logger.add_line("  arguments to variadic parameter");
                            return;
                        }

                        // Do not proceed to check the argument because it forwards
                        // the variadic parameters.
                        forwards_varargs = true;
                        forwards_arg_value = arg_value;
                        continue;
                    }

                } else {
                    param = candidate->params[i];
                }

                if (named_args_out_of_order || named_arg_high_idx > i) {
                    // Do not continue reporting errors because the arguments
                    // being disordered would mean the error messages would not
                    // make any sense.
                    err_line(arg_value, "arg %s: causes the arguments to be out of order", i + 1);
                    logger.add_line("  either order arguments to match parameters or place before named arguments");
                    return;
                }
            }
        }
    }

    // Proceed to check assignablity of arguments.
    forwards_varargs = false;
    named_args_out_of_order = false;
    named_arg_high_idx = 0;
    llvm::SmallVector<Type*> generic_bindings;
    if constexpr (is_func_decl) {
        generic_bindings.insert(generic_bindings.begin(), pre_bound_types.begin(), pre_bound_types.end());
        generic_bindings.resize(candidate->generics.size());
    }

    auto add_err_line_moveobj_to_implicit_ptr = [this, indent, should_show_invidual_underlines]
        (size_t arg_index, Type* param_type, Expr* arg_value) {
        err_line(arg_value,
                 "arg %s: cannot implicitly convert using moveobj to pointer type '%s'",
                 arg_index + 1, param_type);
    };

    auto show_generic_types_where_line = [this, indent]
        (Type* param_type, const llvm::SmallVector<Type*>& generic_bindings) {

        std::string where_msg = get_type_with_generics_where_msg(param_type, generic_bindings, indent);
        if (!where_msg.empty()) {
            logger.add_line("%s         %s", indent ? "  " : "", where_msg)
                .remove_period();
        }
    };

    for (size_t i = 0; i < args.size(); i++) {

        Expr* arg_value = args[i];
        Var* param = nullptr;

        if (uses_native_varargs && i > last_index) {

            if (arg_value->type->is_aggregate()) {
                err_line(arg_value,
                            "arg %s: cannot pass aggregate types to native variadic parameters",
                            i + 1);
            }

            assert(arg_value != nullptr); // Tell warning to shut up.

            if (is_incomplete_type(arg_value->type)) {
                err_line(arg_value,
                            "arg %s: cannot pass incomplete types to native variadic parameters",
                            i + 1);
            }

            // No reason to check native variadic type information because
            // there is no parameter which they are compared to.
            continue;
        }

        if (arg_value->is(NodeKind::NamedValue)) {
            if constexpr (is_func_decl) {
                auto named_arg = static_cast<NamedValue*>(arg_value);
                arg_value = named_arg->assignment;
                param = candidate->find_parameter(named_arg->name);

                named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
                if (param->param_idx != i) {
                    named_args_out_of_order = true;
                }
            }
        } else {
            if constexpr (is_func_decl) {
                if (candidate->uses_varargs && i >= last_index) {
                    param = candidate->params[last_index];

                    // Checking for special condition in which a function passes its own variadic arguments
                    // to another function that takes variadic arguments.
                    if (cur_func && cur_func->forwards_varargs(arg_value)) {
                        // Do not proceed to check the argument because it forwards
                        // the variadic parameters.
                        forwards_varargs = true;
                        continue;
                    }

                } else {
                    param = candidate->params[i];
                }
            }
        }


        auto param_type = get_param_type(param, i);
        if constexpr (is_func_decl) {
            if (candidate->is_generic()) {
                param_type = candidate->partially_qualified_types[param->param_idx + 1];
                if (candidate->uses_varargs && i >= last_index) {
                    auto slice_type = static_cast<SliceType*>(param_type);
                    param_type = slice_type->get_elm_type();
                }

                if (param_type->does_contain_generics()) {
                    if (!check_bind_type_to_generic_type(param_type, arg_value->type, generic_bindings)) {

                        if (param->has_implicit_ptr) {
                            if (!arg_value->type->is_pointer()) {
                                auto param_ptr_type = static_cast<PointerType*>(param_type);
                                auto param_elm_type = param_ptr_type->get_elm_type();

                                if (check_bind_type_to_generic_type(param_elm_type, arg_value->type, generic_bindings)) {
                                    if (arg_value->is(NodeKind::MoveObj)) {
                                        add_err_line_moveobj_to_implicit_ptr(i, param_type, arg_value);
                                        get_type_with_generics_where_msg(param_type,
                                                                                               generic_bindings,
                                                                                               indent);
                                    }
                                    // argument success, continue.
                                    continue;
                                }
                            }
                        }

                        if (arg_value->is(NodeKind::FuncCall)) {
                            auto arg_call = static_cast<FuncCall*>(arg_value);
                            if (arg_call->called_func && arg_call->called_func->has_implicit_return_ptr) {

                                auto ptr_type = static_cast<PointerType*>(arg_call->type);
                                auto from_type = ptr_type->get_elm_type();

                                if (check_bind_type_to_generic_type(param_type, from_type, generic_bindings)) {
                                    // argument success, continue.
                                    continue;
                                }
                            }
                        }

                        // Mismatch info returns with the first character capitalized. Decapitalizing it.
                        std::string mismatch_info = get_type_mismatch_error(param_type, arg_value, param->has_implicit_ptr);
                        mismatch_info[0] = std::tolower(mismatch_info[0]);
                        err_line(arg_value, "arg %s: %s", i + 1, mismatch_info);

                        show_generic_types_where_line(param_type, generic_bindings);
                    }

                    // argument success, continue.
                    continue;
                }
            }
        }

        if (!is_assignable_to(param_type, arg_value)) {
            bool is_assignable = false;
            if (param && param->has_implicit_ptr) {
                auto ptr_type = static_cast<PointerType*>(param->type);
                if (may_implicitly_convert_ptr(ptr_type, arg_value)) {
                    is_assignable = true;
                    if (arg_value->is(NodeKind::MoveObj)) {
                        add_err_line_moveobj_to_implicit_ptr(i, param_type, arg_value);
                    }
                }
            }

            if (!is_assignable && arg_value->is(NodeKind::FuncCall)) {
                auto arg_call = static_cast<FuncCall*>(arg_value);
                if (may_implicitly_convert_return_ptr(param_type, arg_call)) {
                    is_assignable = true;
                }
            }

            if (!is_assignable) {
                // Mismatch info returns with the first character capitalized. Decapitalizing it.
                bool has_implicit_ptr = false;
                if constexpr (is_func_decl) {
                    has_implicit_ptr = param->has_implicit_ptr;
                }

                std::string mismatch_info = get_type_mismatch_error(param_type, arg_value, has_implicit_ptr);
                mismatch_info[0] = std::tolower(mismatch_info[0]);
                err_line(arg_value, "arg %s: %s", i + 1, mismatch_info);
            }
        }
    }

#undef err_line
}

uint64_t acorn::Sema::get_function_call_score(const Func* candidate,
                                              const llvm::SmallVector<Expr*>& args,
                                              bool is_const_object,
                                              const llvm::SmallVector<Type*>& pre_bound_types) {

    uint64_t score = 0;
    bool implicitly_converts_ptr_arg = 0;
    CallCompareStatus status;

    llvm::SmallVector<Type*> generic_bindings;
    if (!candidate->is_generic()) {
        status = compare_as_call_candidate<true, false>(candidate,
                                                        args,
                                                        is_const_object,
                                                        score,
                                                        implicitly_converts_ptr_arg,
                                                        generic_bindings);
    } else {
        generic_bindings.insert(generic_bindings.begin(), pre_bound_types.begin(), pre_bound_types.end());
        generic_bindings.resize(candidate->generics.size());
        status = compare_as_call_candidate<true, true>(candidate,
                                                       args,
                                                       is_const_object,
                                                       score,
                                                       implicitly_converts_ptr_arg,
                                                       generic_bindings);
    }

    switch (status) {
    case CallCompareStatus::INCORRECT_ARGS:
        return INCORRECT_NUM_ARGS_LIMIT;
    case CallCompareStatus::INCORRECT_PARAM_BY_NAME_NOT_FOUND:
    case CallCompareStatus::OUT_OF_ORDER_PARAMS:
        return INCORRECT_PARAM_NAME_OR_ORD_LIMIT;
    case CallCompareStatus::CANNOT_ACCESS_PRIVATE:
        return CANNOT_ACCESS_PRIVATE_LIMIT;
    case CallCompareStatus::CANNOT_USE_VARARGS_AS_NAMED_ARG:
        return CANNOT_USE_VARARGS_AS_NAMED_ARG_LIMIT;
    case CallCompareStatus::FORWARD_VARIADIC_WITH_OTHERS:
        return FORWARD_VARIADIC_WITH_OTHERS_LIMIT;
    default:
        break;
    }

    return score;
}

void acorn::Sema::display_call_ambiguous_info(PointSourceLoc error_loc,
                                              FuncList& candidates,
                                              llvm::SmallVector<Expr*>& args,
                                              bool is_const_object,
                                              const llvm::SmallVector<Type*>& pre_bound_types) {

    llvm::SmallVector<Func*, 16> ambiguous_funcs;
    Func* selected = nullptr;
    uint64_t best_score = 0;

    for (Func* candidate : candidates) {

        uint64_t score = 0;
        bool implicitly_converts_ptr_arg = false;
        bool passes_varargs_along = false;

        llvm::SmallVector<Type*> generic_bindings;
        if (!candidate->is_generic()) {
            auto status = compare_as_call_candidate<false, false>(candidate,
                                                                  args,
                                                                  is_const_object,
                                                                  score,
                                                                  implicitly_converts_ptr_arg,
                                                                  generic_bindings);
            if (status != CallCompareStatus::SUCCESS) {
                continue;
            }
        } else {
            generic_bindings.insert(generic_bindings.begin(), pre_bound_types.begin(), pre_bound_types.end());
            generic_bindings.resize(candidate->generics.size());
            auto status = compare_as_call_candidate<false, true>(candidate,
                                                                 args,
                                                                 is_const_object,
                                                                 score,
                                                                 implicitly_converts_ptr_arg,
                                                                 generic_bindings);
            if (status != CallCompareStatus::SUCCESS) {
                continue;
            }
        }

        if (!selected || best_score > score) {
            selected = candidate;
            best_score = score;
            ambiguous_funcs.clear();
            ambiguous_funcs.push_back(candidate);
        } else if (best_score == score) {
            ambiguous_funcs.push_back(candidate);
        }
    }

    logger.begin_error(error_loc, "Ambiguous function call. Functions:").remove_period();
    logger.add_empty_line();
    display_ambiguous_functions(ambiguous_funcs);
    logger.end_error(ErrCode::SemaAmbiguousFuncCall);

}

std::string acorn::Sema::get_type_with_generics_where_msg(Type* type_with_generics,
                                                          const llvm::SmallVector<Type*>& generic_bindings,
                                                          bool indent) {

    llvm::SmallVector<const GenericType*> generics_used;
    type_with_generics->get_generic_types(generics_used);

    llvm::SmallVector<std::pair<const GenericType*, const Type*>> bound_pairings;
    for (auto generic_type : generics_used) {
        size_t generic_index = generic_type->get_generic_index();
        if (Type* bound_type = generic_bindings[generic_index]) {
            bound_pairings.push_back({ generic_type, bound_type });
        }
    }
    if (!bound_pairings.empty()) {
        std::string generics_msg = "where ";
        for (size_t i = 0; i < bound_pairings.size(); i++) {
            auto generic_type = std::get<0>(bound_pairings[i]);
            auto bound_type   = std::get<1>(bound_pairings[i]);
            generics_msg += generic_type->to_string() + "='" + bound_type->to_string() + "'";
            if (i + 1 != bound_pairings.size()) {
                generics_msg += ", ";
            }
        }

        return generics_msg;
    }
    return "";
}

template<unsigned N>
void acorn::Sema::display_ambiguous_functions(const llvm::SmallVector<Func*, N>& ambiguous_funcs) {
    for (size_t i = 0; i < ambiguous_funcs.size(); i++) {
        auto ambiguous_func = ambiguous_funcs[i];

        logger.add_line([ambiguous_func](auto& logger) {
            logger.print("Defined at: ");
            ambiguous_func->show_location_msg(logger);
        }).remove_period();

        logger.add_line("  '%s'", ambiguous_func->get_decl_string())
            .remove_period();
        logger.add_empty_line();
    }
}

void acorn::Sema::display_call_missing_bindings_info(Expr* call_node,
                                                     Func* called_func,
                                                     const llvm::SmallVector<Type*>& generic_bindings) {
    llvm::SmallVector<Type*> missing_bound_types;
    for (size_t i = 0; i < generic_bindings.size(); i++) {
        Type* bound_type2 = generic_bindings[i];
        Generic* generic = called_func->generics[i];
        if (!bound_type2) {
            missing_bound_types.push_back(generic->type);
        }
    }
    std::string missing_bound_types_str = "";
    for (size_t i = 0; i < missing_bound_types.size(); i++) {
        Type* missing_bound_type = missing_bound_types[i];
        missing_bound_types_str += missing_bound_type->to_string();
        if (i + 1 != missing_bound_types.size()) {
            missing_bound_types_str += ", ";
        }
    }
    error(expand(call_node), "Call to generic function does not bind all types")
        .add_line("Missing bindings for: %s", missing_bound_types_str)
        .end_error(ErrCode::SemaGenericCallHasUnboundTypes);
}

void acorn::Sema::display_generic_bind_named_args_fail_info(const llvm::SmallVector<Expr*>& args,
                                                            const llvm::SmallVector<Generic*>& generics) {

    bool named_args_out_of_order = false;
    size_t named_arg_high_idx = 0;
    for (size_t i = 0; i < args.size(); i++) {
        auto arg = args[i];
        auto genericn = generics[i];

        if (arg->is(NodeKind::NamedValue)) {

            auto named_arg = static_cast<NamedValue*>(arg);

            auto itr = std::ranges::find_if(generics, [name = named_arg->name](auto& genericn) {
                return genericn->name == name;
            });
            if (itr == generics.end()) {
                logger.add_individual_underline(arg->loc);
                logger.add_line("  - could not find generic '%s' for named arg", named_arg->name);
                break;
            }

            genericn = *itr;
            arg = named_arg->assignment;

            if (genericn->index != i) {
                named_args_out_of_order = true;
            }
            named_arg_high_idx = std::max(genericn->index, named_arg_high_idx);
        } else {
            // Cannot determine the order of the arguments if the
            // non-named arguments come after the named arguments
            // and the named arguments are not in order.
            if (named_args_out_of_order || named_arg_high_idx > i) {
                logger.add_individual_underline(expand(arg));
                logger.add_line("  - arg %s: causes the arguments to be out of order", i + 1);
                logger.add_line("    either order arguments to match parameters or place before named arguments");
                break;
            }
        }
    }
}

void acorn::Sema::report_binary_op_cannot_apply(BinOp* bin_op, Expr* expr) {
    auto op_str = token_kind_to_string(bin_op->op, context);
    error(expand(expr), "Operator %s cannot apply to type '%s'.   ('%s' %s '%s')",
            op_str, expr->type, bin_op->lhs->type, op_str, bin_op->rhs->type)
        .end_error(ErrCode::SemaBinOpTypeCannotApply);
}

void acorn::Sema::report_binary_op_mistmatch_types(BinOp* bin_op) {
    error(expand(bin_op), "Invalid operation. Mismatched types ('%s' %s '%s')",
          bin_op->lhs->type, token_kind_to_string(bin_op->op, context), bin_op->rhs->type)
        .end_error(ErrCode::SemaBinOpTypeMismatch);
}

void acorn::Sema::display_interface_func_mismatch_info(Func* interface_func,
                                                       Func* func,
                                                       bool indent,
                                                       bool should_show_invidual_underlines) {

#define err_line(n, fmt, ...) \
add_error_line(n, should_show_invidual_underlines, indent, "%s- " fmt, ##__VA_ARGS__);

    size_t param_count = interface_func->params.size();
    if (param_count != func->params.size()) {
        err_line(nullptr,
                 "incorrect number of parameters. Expected %s but found %s",
                 param_count,
                 func->params.size());
        return;
    }

    if (interface_func->is_constant && !func->is_constant) {
        err_line(nullptr, "expected function to be const");
        return;
    }

    if (!interface_func->is_constant && func->is_constant) {
        err_line(nullptr, "expected function to not be const");
        return;
    }

    if (interface_func->return_type->is_not(func->return_type)) {
        err_line(nullptr, "expected return type '%s' but found '%s'",
                 interface_func->return_type,
                 func->return_type);
    }

    for (size_t i = 0; i < param_count; i++) {
        Var* param1 = interface_func->params[i];
        Var* param2 = func->params[i];
        if (param1->type->is_not(param2->type)) {
            err_line(param2, "param %s: expected type '%s' but found '%s'", i + 1, param1->type, param2->type);
        }
    }

    // report number of raised errors doesnt match (probably bad idea)

    // report which raised errors are not raised/if they raise errors the interface doesn't
    // then after this if that doesnt fail then report them being out of order

    bool raised_errors_match = true;
    for (auto& interface_error : interface_func->raised_errors) {
        auto itr = std::ranges::find_if(func->raised_errors, [interface_error](auto& func_error) {
            return func_error.structn == interface_error.structn;
        });
        if (itr == func->raised_errors.end()) {
            err_line(nullptr, "missing raised error '%s'", interface_error.name);
            raised_errors_match = false;
        }
    }
    for (auto& func_error : func->raised_errors) {
        auto itr = std::ranges::find_if(interface_func->raised_errors, [func_error](auto& interface_error) {
            return func_error.structn == interface_error.structn;
        });
        if (itr == interface_func->raised_errors.end()) {
            if (should_show_invidual_underlines)
                logger.add_individual_underline(func_error.error_loc);
            logger.add_line("%s- raised error '%s' not raised by interface function", indent ? "  " : "", func_error.name);
            raised_errors_match = false;
        }
    }

    // Still verify that the order they are specified in matches.
    if (raised_errors_match) {
        bool order_matches = true;
        for (size_t i = 0; i < interface_func->raised_errors.size(); i++) {
            auto& interface_error = interface_func->raised_errors[i];
            auto& func_error = func->raised_errors[i];
            if (interface_error.structn != func_error.structn) {
                order_matches = false;
                break;
            }
        }
        if (!order_matches) {
            err_line(nullptr, "raised errors do not appear in the same order as interface function");
        }
    }

#undef err_line
}

std::string acorn::Sema::get_type_mismatch_error(Type* to_type, Expr* expr, bool has_implicit_pointer) const {

    if (auto to_enum_type = to_type->get_container_enum_type()) {
        if (expr->type->is_enum()) {
            if (expr->type->is_not(to_enum_type)) {
                return get_type_mismatch_error(to_type, expr->type, has_implicit_pointer);
            }
        } else if (auto from_enum_type = expr->type->get_container_enum_type()) {
            if (from_enum_type->is_not(to_enum_type)) {
                return get_type_mismatch_error(to_type, expr->type, has_implicit_pointer);
            }
        } else {
            return get_type_mismatch_error(to_type, expr->type, has_implicit_pointer);
        }
    }

    if (to_type->is_integer() && expr->trivially_reassignable && expr->type->is_integer()) {
        return get_error_msg_for_value_not_fit_type(to_type);
    } else if (to_type->is_pointer() && expr->type->is_array()) {

        auto from_type = expr->type;
        auto cmp_to_type = to_type;
        if (try_remove_const_for_compare(cmp_to_type, from_type, expr)) {
            auto to_arr_Type   = static_cast<ArrayType*>(cmp_to_type);
            auto from_ptr_type = static_cast<PointerType*>(from_type);

            auto to_elm_type   = to_arr_Type->get_elm_type();
            auto from_elm_type = from_ptr_type->get_elm_type();

            if (to_elm_type->is(from_elm_type) || to_elm_type->is(context.void_type)) {
                if (expr->is(NodeKind::Array)) {
                    return "Cannot an assign array directly to a pointer";
                } else if (expr->is(NodeKind::FuncCall)) {
                    return "Cannot assign an array from a function call to a pointer";
                }
            }
        }

        return get_type_mismatch_error(to_type, expr->type, has_implicit_pointer);
    } else if (expr->is(NodeKind::Array)) {
        // Recreating the array because when assigning to variables it is possible
        // they end up with different dimensions.
        auto arr = static_cast<Array*>(expr);
        Type* expr_type = get_array_type_for_mismatch_error(arr);
        return get_type_mismatch_error(to_type, expr_type, has_implicit_pointer);
    } else {
        return get_type_mismatch_error(to_type, expr->type, has_implicit_pointer);
    }
}

std::string acorn::Sema::get_type_mismatch_error(Type* to_type, Type* from_type, bool has_implicit_pointer) const {

    std::string to_type_str;
    if (has_implicit_pointer && to_type->is_pointer()) {
        auto ptr_type = static_cast<PointerType*>(to_type);
        auto elm_type = ptr_type->get_elm_type();
        to_type_str = elm_type->to_string() + "^";
    } else {
        to_type_str = to_type->to_string();
    }

    if (from_type->is(context.funcs_ref_type)) {
        return std::format("Cannot assign a reference to a function to type '{}'", to_type_str);
    } else if (to_type->is_pointer() && from_type->is_pointer()) {
        auto to_ptr_type = static_cast<PointerType*>(to_type);
        auto to_elm_type = to_ptr_type->get_elm_type();
        if (to_elm_type->is_interface()) {
            auto from_ptr_type = static_cast<PointerType*>(from_type);
            auto from_elm_type = from_ptr_type->get_elm_type();
            if (from_elm_type->is_struct()) {
                auto struct_type = static_cast<StructType*>(from_elm_type);
                auto intr_type = static_cast<InterfaceType*>(to_elm_type);

                auto structn = struct_type->get_struct();
                auto interfacen = intr_type->get_interface();

                if (auto extension = structn->find_interface_extension(interfacen->name)) {
                    if (!extension->is_dynamic) {
                        return std::format("Cannot assign type '{}' to '{}' (Interface extension not dynamic)",
                                           from_type->to_string(), to_type_str);
                    }
                }
            }
        }
    }

    return std::format("Cannot assign type '{}' to '{}'", from_type->to_string(), to_type_str);
}

acorn::Type* acorn::Sema::get_array_type_for_mismatch_error(Array* arr) const {
    llvm::SmallVector<size_t> lengths;
    return get_array_type_for_mismatch_error(arr, lengths, 0);
}

acorn::Type* acorn::Sema::get_array_type_for_mismatch_error(Array* arr,
                                                            llvm::SmallVector<size_t>& lengths,
                                                            size_t depth) const {
    if (lengths.size() == depth) {
        lengths.push_back(arr->elms.size());
    } else if (lengths[depth] < arr->elms.size()) {
        lengths[depth] = arr->elms.size();
    }

    Type* elm_type = nullptr;
    for (auto elm : arr->elms) {
        if (elm->is(NodeKind::Array)) {
            auto elm_arr = static_cast<Array*>(elm);
            elm_type = get_array_type_for_mismatch_error(elm_arr, lengths, depth + 1);
        } else {

            if (!elm_type) {
                elm_type = elm->type;
            } else if (elm_type->is_not(elm->type)) {
                elm_type = elm->type;
            }
        }
    }

    size_t length = lengths[depth];
    return type_table.get_arr_type(elm_type, length);
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
    for (const auto& dep1 : dep_chain) {
        max_name_length = std::max(max_name_length, dep1->name.to_string().size());
    }

    for (auto itr = dep_chain.begin(); itr != dep_chain.end(); ++itr) {
        Decl* dep_lhs = *itr;
        Decl* dep_rhs = (itr + 1) != dep_chain.end() ? *(itr + 1) : start_dep;

        logger.add_line([dep_lhs, dep_rhs, max_name_length](Logger& logger) {
            size_t lhs_pad = max_name_length - dep_lhs->name.to_string().size();
            size_t rhs_pad = max_name_length - dep_rhs->name.to_string().size();

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

void acorn::Sema::report_error_cannot_use_variable_before_assigned(SourceLoc error_loc, Var* var) {
    error(error_loc, "Cannot use variable '%s' before it has been assigned", var->name)
        .end_error(ErrCode::SemaCannotUseVariableBeforeAssigned);
}

template<typename... TArgs>
void acorn::Sema::add_error_line(Node* err_node, bool should_show_invidual_underlines, bool indent, const char* fmt, TArgs&&... args) {
    if (err_node && should_show_invidual_underlines) {
        logger.add_individual_underline(expand(err_node));
    } else {
        logger.still_give_main_location_priority();
    }
    logger.add_line(fmt, indent ? "  " : "", std::forward<TArgs>(args)...);
}


// Utility functions
//--------------------------------------

acorn::Sema::SemScope acorn::Sema::push_scope() {
    SemScope sem_scope;
    sem_scope.parent = cur_scope;
    cur_scope = &sem_scope;
    return sem_scope;
}

void acorn::Sema::pop_scope() {
    cur_scope = cur_scope->parent;
}

void acorn::Sema::add_variable_to_local_scope(Var* var) {
    if (Var* prev_var = cur_scope->find_variable(var->name)) {
        logger.begin_error(var->loc, "Duplicate declaration of variable '%s'", var->name)
                .add_line([prev_var](Logger& l) { prev_var->show_prev_declared_msg(l); })
                .end_error(ErrCode::SemaDuplicateLocVariableDecl);
    } else {
        cur_scope->variables.push_back(var);
    }
}

bool acorn::Sema::is_assignable_to(Type* to_type, Expr* expr) {

    if (auto to_enum_type = to_type->get_container_enum_type()) {
        if (expr->type->is_enum()) {
            // from enum type to enum container type.
            return expr->type->is_ignore_const(to_enum_type);
        } else if (auto from_enum_type = expr->type->get_container_enum_type()) {
            // from enum container type to enum container type.
            return from_enum_type->is_ignore_const(to_enum_type);
        } else {
            return false;
        }
    }

    Type* from_type = expr->type;
    if (!try_remove_const_for_compare(to_type, from_type, expr)) {
        return false;
    }

    if (from_type == context.indeterminate_type) {
        return true;
    }

    switch (to_type->get_kind()) {
    case TypeKind::Int:
    case TypeKind::Int8: case TypeKind::UInt8:
    case TypeKind::Int16: case TypeKind::UInt16:
    case TypeKind::Int32: case TypeKind::UInt32:
    case TypeKind::Int64: case TypeKind::UInt64:
    case TypeKind::USize: case TypeKind::ISize:
    case TypeKind::Char: case TypeKind::Char16: {
        if (to_type->is(from_type)) {
            return true;
        }

        if (expr->trivially_reassignable) {

            // TODO: We want to allow for certain floats to be assignable such
            //       as 1e+9 because those are integers.
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
                case TypeKind::UInt32: case TypeKind::USize:
                    return fits_in_range<uint32_t>(value);
                case TypeKind::UInt64: return fits_in_range<uint64_t>(value);

                default:
                    acorn_fatal("unreachable signed integer type");
                    return false;
                }
            };

            auto get_constant_value = [this](Expr* expr) finline -> uint64_t {
                if (expr->kind == NodeKind::Number) {
                    // Quick pass for common cases.
                    auto number = static_cast<const Number*>(expr);
                    return number->value_u64;
                } else {
                    if (auto ll_value = gen_constant(expr)) {
                        auto ll_integer = llvm::cast<llvm::ConstantInt>(ll_value);
                        return ll_integer->getZExtValue();
                    } else {
                        return 0ull;
                    }
                }
            };

            if (from_type->is_signed()) {
                return does_fit_range((int64_t) get_constant_value(expr));
            } else {
                return does_fit_range(get_constant_value(expr));
            }
        }

        return to_type->is(from_type);
    }
    case TypeKind::Float: case TypeKind::Double: {
        if (from_type->is_float()) {
            return to_type->is(from_type);
        } else if (from_type->is_integer()) {
            return true;
        }

        if (expr->trivially_reassignable) {
            return true;
        }

        return false;
    }
    case TypeKind::Pointer: {
        if (expr->is(NodeKind::String)) {
            if (expr->type->is(context.const_char_ptr_type)) {
                return true;
            }

            return to_type->is(from_type) || to_type->is(context.void_ptr_type);
        } else if (from_type->is_array()) {
            auto to_arr_Type = static_cast<ArrayType*>(to_type);
            auto from_ptr_type = static_cast<PointerType*>(from_type);

            auto to_elm_type = to_arr_Type->get_elm_type();
            auto from_elm_type = from_ptr_type->get_elm_type();

            if (!(to_elm_type->is(from_elm_type) || to_elm_type->is(context.void_type))) {
                return false;
            }

            if (expr->is(NodeKind::Array) || expr->is(NodeKind::FuncCall)) {
                return false;
            }

            return true;
        } else if (from_type->is_pointer()) {
            auto to_ptr_type = static_cast<PointerType*>(to_type);
            auto to_elm_type = to_ptr_type->get_elm_type();
            if (to_elm_type->is_interface()) {
                if (to_type->is(from_type)) {
                    return true;
                }

                auto from_ptr_type = static_cast<PointerType*>(from_type);
                auto from_elm_type = from_ptr_type->get_elm_type();
                if (from_elm_type->is_struct()) {
                    auto struct_type = static_cast<StructType*>(from_elm_type);
                    auto intr_type = static_cast<InterfaceType*>(to_elm_type);

                    auto structn = struct_type->get_struct();
                    auto interfacen = intr_type->get_interface();

                    if (auto extension = structn->find_interface_extension(interfacen->name)) {
                        return extension->is_dynamic;
                    }
                }

                return false;
            } else {
                return to_type->is(from_type) || to_type->is(context.void_ptr_type);
            }
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
    case TypeKind::Slice: {
        if (from_type->is_array()) {
            auto from_arr_type = static_cast<ArrayType*>(from_type);
            auto from_elm_type = from_arr_type->get_elm_type();

            auto to_slice_type = static_cast<SliceType*>(to_type);
            auto to_elm_type   = to_slice_type->get_elm_type();

            return from_elm_type->is(to_elm_type);
        }

        return to_type->is(from_type);
    }
    case TypeKind::Struct: {
        if (context.is_std_any_type(to_type)) {
            if (is_incomplete_type(from_type)) {
                return false;
            }
            return true;
        }

        return to_type->is(from_type);
    }
    default:
        return to_type->is(from_type);
    }
}

bool acorn::Sema::is_castable_to(Type* to_type, Expr* expr) {
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

bool acorn::Sema::is_lvalue(Expr* expr) const {
    if (expr->is(NodeKind::MemoryAccess)) {
        return true;
    }
    if (expr->is(NodeKind::IdentRef) || expr->is(NodeKind::DotOperator)) {
        auto ref = static_cast<const IdentRef*>(expr);
        return ref->is_var_ref();
    }

    if (expr->is(NodeKind::UnaryOp)) {
        auto unary = static_cast<const UnaryOp*>(expr);
        return unary->op == '*';
    }

    return false;
}

bool acorn::Sema::is_readonly_field_without_access(Expr* expr) const {
    if (expr->is(NodeKind::DotOperator)) {
        auto dot = static_cast<DotOperator*>(expr);
        if (dot->is_var_ref() && dot->var_ref->has_modifier(Modifier::Readonly) &&
            (!cur_func || cur_func->non_generic_struct_instance != dot->var_ref->non_generic_struct_instance)) {
            return true;
        }
    }
    return false;
}

bool acorn::Sema::is_incomplete_type(Type* type) const {
    switch (type->get_kind()) {
    case TypeKind::Void:
    case TypeKind::NamespaceRef:
    case TypeKind::EmptyArray:
    case TypeKind::Null:
    case TypeKind::Range:
    case TypeKind::Auto:
    case TypeKind::Expr:
    case TypeKind::FuncsRef:
    case TypeKind::Interface:
    case TypeKind::Enum:
        return true;
    default:
        return false;
    }
}

bool acorn::Sema::is_incomplete_statement(Node* stmt) const {
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
    case NodeKind::RaiseStmt:
    case NodeKind::RecoverStmt:
    case NodeKind::VarList:
        return false;
    case NodeKind::BinOp: {
        auto bin_op = static_cast<const BinOp*>(stmt);

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
            return false;
        default:
            break;
        }

        return true;
    }
    case NodeKind::UnaryOp: {
        auto unary_op = static_cast<const UnaryOp*>(stmt);

        if (unary_op->op == Token::AddAdd || unary_op->op == Token::SubSub ||
            unary_op->op == Token::PostAddAdd || unary_op->op == Token::PostSubSub) {
            return false;
        }

        return true;
    }
    default:
        return true;
    }
}

bool acorn::Sema::may_implicitly_convert_return_ptr(Type* to_type, FuncCall* call) const {
    if (call->called_func && call->called_func->has_implicit_return_ptr) {
        auto ptr_type  = static_cast<PointerType*>(call->type);
        auto from_type = ptr_type->get_elm_type();
        if (try_remove_const_for_compare(to_type, from_type, nullptr)) {
            return to_type->is(from_type);
        }
    }
    return false;
}

bool acorn::Sema::may_implicitly_convert_ptr(PointerType* ptr_type, Expr* from_expr) const {
    auto to_type   = ptr_type->get_elm_type();
    auto from_type = from_expr->type;
    if (try_remove_const_for_compare(to_type, from_type, from_expr)) {
        return to_type->is(from_type);
    }
    return false;
}

void acorn::Sema::create_cast(Expr* expr, Type* to_type) {
    // TODO: Removing all constness here although because function types are
    // pointers it may be possible at some point that you can cast between them
    // but at that point it is unclear if the type needs casting or not?
    if (expr->type->remove_all_const()->is_not(to_type->remove_all_const())) {
        expr->cast_type = to_type;
        if (to_type->is_struct() || to_type->is_enum()) {
            expr->is_foldable = false;
        }
    }
}

bool acorn::Sema::is_condition(Type* type) const {
    return type->is_bool() ||
           type->is_pointer() ||
           type->get_kind() == TypeKind::Null;
}

acorn::Type* acorn::Sema::get_type_of_type_expr(Expr* expr) {
    // TODO: should the expr_type just contain the type to which it is an expression
    // type of?
    if (expr->is(NodeKind::IdentRef)) {
        IdentRef* ref = static_cast<IdentRef*>(expr);
        if (ref->is_composite_ref()) {
            if (ref->composite_ref->is(NodeKind::Struct)) {
                auto structn = static_cast<Struct*>(ref->composite_ref);
                return structn->struct_type;
            } else if (ref->composite_ref->is(NodeKind::Enum)) {
                auto enumn = static_cast<Enum*>(ref->composite_ref);
                return enumn->enum_type;
            } else if (ref->composite_ref->is(NodeKind::Interface)) {
                auto interfacen = static_cast<Interface*>(ref->composite_ref);
                return interfacen->interface_type;
            } else {
                acorn_fatal("unknown composite type");
                return nullptr;
            }
        } else if (ref->is_generic_type_ref()) {
            return fixup_generic_type(ref->generic_type_ref);
        } else {
            acorn_fatal("unreachable");
            return nullptr;
        }
    } else if (expr->is(NodeKind::FuncCall)) {
        FuncCall* call = static_cast<FuncCall*>(expr);
        return call->type_for_type_expr;
    } else {
        auto type_expr = static_cast<TypeExpr*>(expr);
        return type_expr->expr_type;
    }
}

uint64_t acorn::Sema::get_total_number_of_values_in_range(BinOp* range) {
    acorn_assert(range->is_foldable, "Cannot get the total number of values as the range is not foldable");

    auto ll_lhs_value = llvm::cast<llvm::ConstantInt>(gen_constant(range->lhs));
    auto ll_rhs_value = llvm::cast<llvm::ConstantInt>(gen_constant(range->rhs));

    uint64_t lhs_value = ll_lhs_value->getZExtValue();
    uint64_t rhs_value = ll_rhs_value->getZExtValue();

    switch (range->op) {
    case Token::RangeEq:
        if (range->type) {
            int64_t lhs_value_signed = static_cast<int64_t>(lhs_value);
            int64_t rhs_value_signed = static_cast<int64_t>(rhs_value);
            return rhs_value_signed - lhs_value_signed + 1;
        } else {
            return rhs_value - lhs_value + 1;
        }
    case Token::RangeLt:
        if (range->type) {
            int64_t lhs_value_signed = static_cast<int64_t>(lhs_value);
            int64_t rhs_value_signed = static_cast<int64_t>(rhs_value);
            return rhs_value_signed - lhs_value_signed;
        } else {
            return rhs_value - lhs_value;
        }
    default:
        acorn_fatal("Unreachable. Unknown range type");
        return 0ull;
    }
}

acorn::Decl* acorn::Sema::find_composite(Identifier name) {

    if (auto found_composite = file->get_namespace()->find_composite(name)) {
        return found_composite;
    }

    if (auto found_composite = file->find_composite(name)) {
        return found_composite;
    }

    auto& imports = file->get_imports();
    auto itr = imports.find(name);
    if (itr != imports.end()) {
        auto importn = itr->second;
        if (importn->is_imported_composite()) {
            return itr->second->imported_composite;
        }
    }

    return nullptr;
}

llvm::Constant* acorn::Sema::gen_constant(Expr* expr) {
    IRGenerator generator(context);
    auto ll_value = generator.gen_rvalue(expr);
    if (ll_value->getValueID() == llvm::Value::ValueTy::PoisonValueVal) {
        error(expand(expr), "While trying to compile a constant value encountered a value with undefined behavior")
            .end_error(ErrCode::SemaPoisonWhenLLVMConstGen);
        return nullptr;
    }

    return llvm::cast<llvm::Constant>(ll_value);
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
