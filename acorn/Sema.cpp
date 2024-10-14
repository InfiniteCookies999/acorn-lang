#include "Sema.h"

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
    : context(context), modl(file->modl), file(file), logger(logger), type_table(context.type_table) {
}

void acorn::Sema::resolve_global_comptime(Context& context, Module& modl) {
    for (Node* node : modl.get_comptime_control_flows()) {
        if (node->is(NodeKind::ComptimeIfStmt)) {
            auto ifs = as<ComptimeIfStmt*>(node);
            Sema sema(context, ifs->file, ifs->file->logger);
            sema.is_global_comptime = true;
            sema.check_comptime_if(ifs);
        }
    }
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
    // TODO: This will eventually need to take into account that a type
    // may contain an unsolved type in which case it needs to descend
    // the type and resolve any contained types.
    
    if (type->get_kind() == (TypeKind::UnresolvedArrayType)) {

        auto unarr_type = as<UnresolvedArrayType*>(type);
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

            // Everything is okay we can create a new array out of it!
            return type_table.get_arr_type(unarr_type->get_elm_type(), length);

        } else {
            return nullptr;
        }
    }

    return type;
}

// Statement checking
//--------------------------------------

void acorn::Sema::check_for_duplicate_functions(Module& modl) {
    for (const auto& [_, funcs] : modl.get_functions()) {
        for (auto itr = funcs.begin(); itr != funcs.end(); ++itr) {
            for (auto itr2 = itr+1; itr2 != funcs.end(); ++itr2) {
                if (check_for_duplicate_match(*itr, *itr2)) {
                    break;
                }
            }
        }
    }
}

bool acorn::Sema::check_for_duplicate_match(const Func* func1, const Func* func2) {
    if (func1->params.size() != func2->params.size()) {
        return false;
    }
    if (!std::ranges::equal(func1->params, func2->params,
                            [](const Var* p1, const Var* p2) {
                                return p1->type->is(p2->type);
                            })) {
        return false;
    }
    report_redeclaration(func1, func2, "function", ErrCode::SemaDuplicateGlobalFunc);
    return true;
}

void acorn::Sema::check_for_duplicate_variables(Module& modl) {
    for (auto [var1, var2] : modl.get_redecl_global_variables()) {
        report_redeclaration(var1, var2, "variable", ErrCode::SemaDuplicateGlobalVar);
    }
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
                                 BadScopeLocation location,
                                 auto expr_or_stmt_str) finline {
        const char* scope_str = "global"; // TODO: Once there are more kinds of scopes to report this will need to change.
        logger.begin_error(loc, "%s does not belong at %s scope",
                           expr_or_stmt_str, scope_str)
            .end_error(ErrCode::SemaNodeAtWrongScope);
    };

    for (auto [location, node, logger] : modl.get_bad_scope_nodes()) {
        if (location == BadScopeLocation::Global) {
            if (node->is_expression()) {
                Expr* expr = as<Expr*>(node);
                report(logger, expand(expr), location, "Expression");
            } else {
                report(logger, node->loc, location, "Statement");
            }
        }
    }
}

void acorn::Sema::resolve_imports(Context& context, Module& modl) {
    for (auto& entry : modl.get_imports()) {
        resolve_import(context, entry.second);
    }
}

void acorn::Sema::resolve_import(Context& context, ImportStmt* importn) {
    if (auto modl = context.find_module(importn->location_key)) {
        importn->set_imported_module(modl);
    } else {
        auto& logger = importn->file->logger;
        SourceLoc error_loc = {
            .ptr    = importn->location_key.data(),
            .length = as<uint16_t>(importn->location_key.size())
        };
        logger.begin_error(error_loc, "Could not find module '%s'", importn->location_key)
            .end_error(ErrCode::SemaCouldNotResolveImport);
    }
}

void acorn::Sema::check_node(Node* node) {
    switch (node->kind) {
    case NodeKind::Var:
        return check_variable(as<Var*>(node));
    case NodeKind::IdentRef:
        return check_ident_ref(as<IdentRef*>(node), &modl, false);
    case NodeKind::DotOperator:
        return check_dot_operator(as<DotOperator*>(node), false);
    case NodeKind::ReturnStmt:
        return check_return(as<ReturnStmt*>(node));
    case NodeKind::IfStmt: {
        bool _;
        return check_if(as<IfStmt*>(node), _);
    }
    case NodeKind::ComptimeIfStmt:
        return check_comptime_if(as<ComptimeIfStmt*>(node));
    case NodeKind::BinOp:
        return check_binary_op(as<BinOp*>(node));
    case NodeKind::UnaryOp:
        return check_unary_op(as<UnaryOp*>(node));
    case NodeKind::FuncCall:
        return check_function_call(as<FuncCall*>(node));
    case NodeKind::Cast:
        return check_cast(as<Cast*>(node));
    case NodeKind::NamedValue:
        return check_named_value(as<NamedValue*>(node));
    case NodeKind::Number:
    case NodeKind::Bool:
    case NodeKind::String:
    case NodeKind::Null:
        break;
    case NodeKind::ScopeStmt:
        return check_scope(as<ScopeStmt*>(node));
    case NodeKind::Array:
        return check_array(as<Array*>(node));
    case NodeKind::MemoryAccess:
        return check_memory_access(as<MemoryAccess*>(node));
    case NodeKind::PredicateLoopStmt:
        return check_predicate_loop(as<PredicateLoopStmt*>(node));
    case NodeKind::RangeLoopStmt:
        return check_range_loop(as<RangeLoopStmt*>(node));
    case NodeKind::IteratorLoopStmt:
        return check_iterator_loop(as<IteratorLoopStmt*>(node));
    case NodeKind::BreakStmt:
    case NodeKind::ContinueStmt:
        return check_loop_control(as<LoopControlStmt*>(node));
    case NodeKind::SwitchStmt:
        return check_switch(as<SwitchStmt*>(node));
    default:
        acorn_fatal("check_node(): missing case");
    }
}

void acorn::Sema::check_function(Func* func) {
    if (func->has_modifier(Modifier::Native)) return;

    // Logger::debug("checking function: %s", func->name);

    check_modifier_incompatibilities(func);

    func->return_type = fixup_type(func->return_type);
    if (!func->return_type) {
        // Failed to fixup return type so returning early.
        return;
    }

    cur_func = func;

    // If we ever decide to allow nesting functions for some reason then this
    // will possibly be a problem because it will have overriden the current scope.
    SemScope sem_scope = push_scope();
    for (Var* param : func->params) {
        check_variable(param);
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
    };

    check_modifier_incompatibilities(var);

    // Reporting errors if the variable is local to a function and has
    // modifiers.
    if (!var->is_global && var->modifiers) {
        for (uint32_t modifier = Modifier::Start;
                      modifier != Modifier::End; modifier *= 2) {
            if (var->modifiers & modifier) {
                error(var->get_modifier_location(modifier), "Modifier cannot apply to local variable")
                    .end_error(ErrCode::SemaLocalVarHasModifiers);
            }
        }
    }

    if (cur_scope) {
        if (Var* prev_var = cur_scope->find_variable(var->name)) {
            logger.begin_error(var->loc, "Duplicate declaration of variable '%s'", var->name)
                  .add_line([prev_var](Logger& l) { prev_var->show_prev_declared_msg(l); })
                  .end_error(ErrCode::SemaDuplicateLocVariableDecl);
        } else {
            cur_scope->variables.push_back(var);
        }
    }

    if (var->is_global) {
        cur_global_var = var;
    }
    var->is_being_checked = true;

    if (var->assignment) {
        check_node(var->assignment);
        if (!var->assignment->type) {
            return cleanup();
        }
    }

    var->type = fixup_type(var->type);
    if (!var->type) {
        // Failed to fixup the type so returning early.
        return;
    }

    var->is_foldable = (var->type->is_integer() || var->type->is_bool()) &&
                       var->type->is_const() &&
                       var->assignment && var->assignment->is_foldable;

    if (!var->is_foldable) {
        // If not a global variable we need to tell the current function to
       // provide us with stack memory.
        if (cur_func && !var->is_param() && !var->is_global) {
            cur_func->vars_to_alloc.push_back(var);
        } else if (var->is_global) {
            context.queue_gen(var);
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

void acorn::Sema::check_return(ReturnStmt* ret) {
    cur_scope->all_paths_return = true;
    cur_scope->found_terminal = true;

    ++cur_func->num_returns;

    bool is_assignable;
    if (ret->value) {
        check_and_verify_type(ret->value);
        is_assignable = is_assignable_to(cur_func->return_type, ret->value);

        if (cur_func->return_type->is_array() && ret->value->is(NodeKind::IdentRef)) {
            auto ref = as<IdentRef*>(ret->value);
            if (ref->is_var_ref()) {
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
            
        Var* var = as<Var*>(ifs->cond);
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
        Expr* cond = as<Expr*>(ifs->cond);
        if (cond->type) {
            check_condition(cond);
        }
    }
    
    check_scope(ifs->scope, &sem_scope);
    all_paths_return = sem_scope.all_paths_return;
    pop_scope();

    if (ifs->elseif && ifs->elseif->is(NodeKind::IfStmt)) {
        bool all_paths_return2;
        check_if(as<IfStmt*>(ifs->elseif), all_paths_return2);
        all_paths_return &= all_paths_return2;
    } else if (ifs->elseif) {
        check_scope(as<ScopeStmt*>(ifs->elseif));
    } else {
        // If an else does not exist then not all paths return.
        all_paths_return = false;
    }

    cur_scope->all_paths_return = all_paths_return;
    cur_scope->found_terminal = all_paths_return;
}

void acorn::Sema::check_comptime_if(ComptimeIfStmt* ifs) {
    if (ifs->is(NodeKind::Var)) {
        error(ifs->cond, "Comptime if does not allow comparing to variables")
            .end_error(ErrCode::SemaComptimeIfCannotCompareToVar);
        return;
    }
    
    Expr* cond = as<Expr*>(ifs->cond);
    check_and_verify_type(cond);
    if (!check_condition(cond)) {
        return;
    }
    
    if (!cond->is_foldable) {
        error(expand(cond), "Comptime if expects the condition to be determined at compile time")
            .end_error(ErrCode::SemaNotComptimeCompute);
        return;
    }

    // Note: We will only check the scope if the path is taken. If it is not
    //       then we could possibly run into errors that do not actually represent
    //       the state of the problem. For example a variable being declared on one
    //       operating system and trying to access it but not on another.

    // We pass the current scope when calling check_scope because the comptime ifs do not
    // create new scopes.
    auto ll_cond = llvm::cast<llvm::ConstantInt>(gen_constant(expand(cond), cond));
    if (!ll_cond->isZero()) {
        // Path taken!
        ifs->takes_path = true;
        check_scope(ifs->scope, cur_scope);
    } else if (ifs->elseif) {
        ifs->takes_path = false;
        if (ifs->elseif->is(NodeKind::ComptimeIfStmt)) {
            // Onto the next possible comptime condition.
            check_comptime_if(as<ComptimeIfStmt*>(ifs->elseif));
        } else {
            check_scope(as<ScopeStmt*>(ifs->elseif), cur_scope);
        }
    }
}

void acorn::Sema::check_predicate_loop(PredicateLoopStmt* loop) {
    check_node(loop->cond);
    if (loop->cond->type) {
        check_condition(loop->cond);
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
    check_node(loop->var);
    check_node(loop->container);

    if (loop->container->type) {
        if (loop->container->type->is_array() && loop->var->type) {
            auto arr_type = as<ArrayType*>(loop->container->type);
            auto elm_type = arr_type->get_elm_type();

            bool const_must_match = elm_type->is_aggregate() || elm_type->is_pointer();
            bool types_match = const_must_match ? elm_type->is(loop->var->type)
                                                : elm_type->is_ignore_const(loop->var->type);

            if (!types_match) {
                error(loop->container, "Cannot assign type '%s' to variable type '%s'",
                      elm_type, loop->var->type)
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
                auto bool_cond = as<Bool*>(scase.cond);
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
        case NodeKind::FuncCall:
        case NodeKind::IfStmt:
        case NodeKind::ComptimeIfStmt:
        case NodeKind::ScopeStmt:
        case NodeKind::PredicateLoopStmt:
        case NodeKind::RangeLoopStmt:
        case NodeKind::IteratorLoopStmt:
        case NodeKind::BreakStmt:
        case NodeKind::ContinueStmt:
        case NodeKind::SwitchStmt:
            break;
        case NodeKind::BinOp: {
            BinOp* bin_op = as<BinOp*>(stmt);

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
            UnaryOp* unary_op = as<UnaryOp*>(stmt);

            if (unary_op->op == Token::AddAdd || unary_op->op == Token::SubSub ||
                unary_op->op == Token::PostAddAdd || unary_op->op == Token::PostSubSub) {
                goto ContinueToCheckNodeLab;
            }

            goto IncompleteStatementLab;
        }
        default:
        IncompleteStatementLab:
            error(stmt, "Incomplete statement")
                .end_error(ErrCode::SemaIncompleteStmt);
            continue;
        }

    ContinueToCheckNodeLab:
        
        if (stmt->is(NodeKind::Func)) {
            if (is_global_comptime) {
                file->add_function(as<Func*>(stmt));
            } else {
                error(stmt, "Functions cannot be declared within another function")
                    .end_error(ErrCode::SemaNoLocalFuncs);
            }
        } else if (stmt->is(NodeKind::Var) && is_global_comptime) {
            file->add_variable(as<Var*>(stmt));
        } else {
            check_node(stmt);
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
        return get_integer_type(enforce_lhs);
    };

    //lhs, rhs, error_cannot_apply, error_mismatched, valid_number_compare
    auto get_add_sub_mul_type = [=, this](bool enforce_lhs) finline -> Type* {
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
                auto arr_type = as<ArrayType*>(lhs->type);
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
                auto arr_type = as<ArrayType*>(rhs->type);
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

        unary_op->type = as<PointerType*>(expr->type)->get_elm_type();
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

void acorn::Sema::check_ident_ref(IdentRef* ref, Module* search_modl, bool is_for_call) {
    
    bool same_modl = search_modl == &modl;

    auto find_function = [=, this]() finline {
        if (same_modl) {
            if (auto* funcs = file->find_functions(ref->ident)) {
                ref->set_funcs_ref(funcs);
                return;
            }
        }

        if (auto* funcs = search_modl->find_functions(ref->ident)) {
            ref->set_funcs_ref(funcs);
        }
    };

    auto find_variable = [=, this]() finline {
        if (cur_scope) {
            if (auto* var = cur_scope->find_variable(ref->ident)) {
                ref->set_var_ref(var);
                return;
            }
        }
        if (same_modl) {
            if (auto* var = file->find_variable(ref->ident)) {
                ref->set_var_ref(var);
                return;
            }
        }

        if (auto* var = search_modl->find_variable(ref->ident)) {
            ref->set_var_ref(var);
            return;
        }
        if (auto* universal = context.get_universal_constant(ref->ident)) {
            ref->set_universal(universal);
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
    if (!ref->found_ref() && same_modl) {
        if (auto importn = modl.find_import(ref->ident)) {
            ref->set_import(importn);
        }
    }
    
    switch (ref->found_kind) {
    case IdentRef::VarKind: {
        
        Var* var_ref = ref->var_ref;
        if (!var_ref->generated && var_ref->is_global) {
            check_global_variable(ref->loc, ref->var_ref);
        }
        
        if (var_ref->type->is_integer() && var_ref->type->is_const()) {
            ref->is_foldable = true;
        } else {
            ref->is_foldable = false;
        }

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
        ref->type = context.module_ref_type;
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
        IdentRef* site = as<IdentRef*>(dot->site);
        check_ident_ref(site, &modl, false);
        dot->is_foldable = site->is_foldable;
        if (site->type == context.module_ref_type) {
            auto importn = site->import_ref;
            // Special case in which we search in a given module.
            check_ident_ref(dot, importn->imported_modl, is_for_call);
            return;
        } else if (!site->type) {
            return;
        }
    } else {
        check_and_verify_type(dot->site);
        dot->is_foldable = dot->site->is_foldable;
    }

    if (dot->ident == context.length_identifier && dot->site->type->is_array()) {
        dot->is_array_length = true;
        dot->type = context.int_type;
    } else {
        error(expand(dot), "Cannot access field '%s' of type '%s'", dot->ident, dot->site->type)
            .end_error(ErrCode::SemaDotOperatorCannotAccessType);
    }
}

void acorn::Sema::check_function_call(FuncCall* call) {
    
    bool args_have_errors = false;
    for (Expr* arg : call->args) {
        check_node(arg);
        if (!arg->type) args_have_errors = true;
    }
    if (args_have_errors) return;

    bool is_callable = true;
    if (call->site->is(NodeKind::IdentRef)) {
        
        IdentRef* ref = as<IdentRef*>(call->site);
        check_ident_ref(ref, &modl, true);
        yield_if(ref);

        if (ref->found_kind != IdentRef::FuncsKind) {
            is_callable = false;
        }
    } else if (call->site->is(NodeKind::DotOperator)) {
        // This is very similar to check to IdentRef but it is too much
        // small differences to abstract in any meaningfull way as far
        // as I can tell.

        DotOperator* ref = as<DotOperator*>(call->site);
        check_dot_operator(ref, true);
        yield_if(ref);

        if (ref->found_kind != IdentRef::FuncsKind) {
            is_callable = false;
        }
    } else {
        check_and_verify_type(call->site);
        // TODO: Deal with
        is_callable = false;
    }

    if (!is_callable) {
        error(expand(call->site), "Type '%s' is not callable", call->site->type)
            .end_error(ErrCode::SemaTypeNoCallable);
        return;
    }

    IdentRef* ref = as<IdentRef*>(call->site);
    
    auto called_func = find_best_call_canidate(*ref->funcs_ref, call->args);
    if (!called_func) {
        display_call_mismatch_info(expand(call), *ref->funcs_ref, call->args);
        return;
    }

    for (size_t i = 0; i < call->args.size(); i++) {
        auto arg_value = call->args[i];
        Var* param;
        if (arg_value->is(NodeKind::NamedValue)) {
            auto named_arg = as<NamedValue*>(arg_value);
            param = called_func->find_parameter(named_arg->name);
            named_arg->mapped_idx = param->param_idx;
            
            arg_value = named_arg->assignment;
        } else {
            param = called_func->params[i];
        }

        create_cast(arg_value, param->type);
    }

    call->called_func = called_func;
    call->type = called_func->return_type;
    context.queue_gen(call->called_func);

}

acorn::Func* acorn::Sema::find_best_call_canidate(FuncList& canidates,
                                                  llvm::SmallVector<Expr*, 8>& args) {
    
    Func* selected = nullptr;
    uint32_t best_mimatched_types = 0;
    
    auto select = [&selected, &best_mimatched_types](Func* canidate,
                                                     uint32_t mimatched_types) finline{
        selected = canidate;
        best_mimatched_types = mimatched_types;
    };
    for (Func* canidate : canidates) {
        uint32_t mimatched_types = 0;
        if (!compare_as_call_canidate(canidate, args, mimatched_types)) {
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

bool acorn::Sema::compare_as_call_canidate(Func* canidate,
                                           llvm::SmallVector<Expr*, 8>& args,
                                           uint32_t& mimatched_types) {
    if (canidate->params.size() != args.size()) {
        return false;
    }

    bool named_args_out_of_order = false;
    uint32_t named_arg_high_idx = 0;
    for (size_t i = 0; i < canidate->params.size(); i++) {
        Expr* arg_value = args[i];
        Var* param;

        if (arg_value->is(NodeKind::NamedValue)) {
            // Handle named arguments by finding the corresponding parameter

            auto named_arg = as<NamedValue*>(arg_value);
            arg_value = named_arg->assignment;
            param = canidate->find_parameter(named_arg->name);
            
            // Check to make sure we found the parameter by the given name.
            if (!param) {
                return false;
            }

            if (param->param_idx != i) {
                named_args_out_of_order = true;
            }
            named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
        } else {
            param = canidate->params[i];

            // Cannot determine the order of the arguments if the
            // non-named arguments come after the name arguments
            // and the named arguments are not in order.
            if (named_args_out_of_order || named_arg_high_idx > i) {
                return false;
            }
        }
        
        if (!is_assignable_to(param->type, arg_value)) {
            return false;
        }

        if (param->type->is_not(arg_value->type)) {
            ++mimatched_types;
        }
    }

    return true;
}

void acorn::Sema::display_call_mismatch_info(PointSourceLoc error_loc, 
                                             const FuncList& canidates, 
                                             const llvm::SmallVector<Expr*, 8>& args) const {
    
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
    
    if (canidates.size() == 1) {
        
        Func* canidate = canidates[0];

        logger.begin_error(error_loc, "Invalid call to function: %s", function_decl_to_string(canidate));
        logger.add_empty_line();
        display_call_mismatch_info(canidate, args, false);
        logger.end_error(ErrCode::SemaInvalidFuncCallSingle);

    } else {

        logger.begin_error(error_loc, "Could not find a valid overloaded function");
        for (const Func* canidate : canidates) {
            logger.add_empty_line();
            logger.add_line("Could not match: %s", function_decl_to_string(canidate));
            display_call_mismatch_info(canidate, args, true);
        }
        logger.end_error(ErrCode::SemaInvalidFuncCallOverloaded);

    }
}

void acorn::Sema::display_call_mismatch_info(const Func* canidate,
                                             const llvm::SmallVector<Expr*, 8>& args,
                                             bool indent) const {
    
    auto& params = canidate->params;
    
#define err_line(fmt, ...) logger.add_line(("%s- " fmt), indent ? "  " : "", __VA_ARGS__)

    if (canidate->params.size() != args.size()) {
        err_line("Incorrect number of args. Expected %s but found %s",
            params.size(), args.size());
        return;
    }

    bool named_args_out_of_order = false;
    uint32_t named_arg_high_idx = 0;
    for (size_t i = 0; i < params.size(); i++) {
        
        Expr* arg_value = args[i];
        Var* param;

        if (arg_value->is(NodeKind::NamedValue)) {
            
            auto named_arg = as<NamedValue*>(arg_value);
            arg_value = named_arg->assignment;
            param = canidate->find_parameter(named_arg->name);
            
            if (!param) {
                err_line("Could not find param '%s' for named arg", named_arg->name);
                return;
            }

            named_arg_high_idx = std::max(param->param_idx, named_arg_high_idx);
            if (param->param_idx != i) {
                named_args_out_of_order = true;
            }
        } else {
            param = canidate->params[i];

            if (named_args_out_of_order || named_arg_high_idx > i) {
                // Do not continue reporting errors because the arguments
                // being disordered would mean the error messages would not
                // make any sense.
                err_line("Arg %s causes the arguments to be out of order", i + 1);
                logger.add_line("  Order named arguments or place before named arguments");
                return;
            }
        }
        
        if (!is_assignable_to(param->type, arg_value)) {
            err_line("Wrong type for arg %s. Expected '%s' but found '%s'",
                     i + 1, param->type, arg_value->type);
        }
    }

#undef err_line
}

void acorn::Sema::check_cast(Cast* cast) {
    cast->type = cast->explicit_cast_type;
    
    check_and_verify_type(cast->value);
    if (!is_castable_to(cast->explicit_cast_type, cast->value)) {
        error(expand(cast), "Cannot cast to type '%s' from '%s'",
              cast->explicit_cast_type, cast->value->type)
            .end_error(ErrCode::SemaInvalidCast);
    }
}

void acorn::Sema::check_named_value(NamedValue* named_value) {
    check_node(named_value->assignment);
    named_value->type = named_value->assignment->type;
}

void acorn::Sema::check_array(Array* arr) {
    if (arr->elms.empty()) {
        arr->type = context.empty_array_type;
        return;
    }

    Type* elm_type = nullptr;
    Expr* value_for_elm_type;
    bool values_have_errors = false;
    for (Expr* value : arr->elms) {

        check_node(value);

        if (!value->type) {
            values_have_errors = true;
            continue;
        }

        if (!value->is_foldable) {
            arr->is_foldable = false;
        }

        if (!elm_type) {
            elm_type = value->type;
            value_for_elm_type = value;
        } else if (elm_type->is_not(value->type)) {
            if (!is_assignable_to(elm_type, value)) {
                // Check the reverse case.
                if (!is_assignable_to(value->type, value_for_elm_type)) {
                    error(expand(value), "Incompatible element types. First found '%s' but now '%s'",
                          elm_type, value->type)
                        .end_error(ErrCode::SemaIncompatibleArrayElmTypes);
                } else {
                    elm_type = value->type;
                    value_for_elm_type = value;
                }
            }
        }
    }

    if (values_have_errors) {
        return;
    }
    
    for (Expr* value : arr->elms) {
        create_cast(value, elm_type);
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
        auto ctr_type = as<ContainerType*>(access_type);
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

void acorn::Sema::check_global_variable(SourceLoc error_loc, Var* var) {
    if (cur_global_var) {
        cur_global_var->dependency = var;
        
        if (var->is_being_checked) {
            display_circular_dep_error(error_loc, 
                                       cur_global_var,
                                       "Global variables form circular dependency", 
                                       ErrCode::SemaGlobalCircularDependency);
        }
    }
    
    if (!var->is_being_checked) {
        Sema sema(context, var->file, var->get_logger());
        sema.check_variable(var);
    }
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
        
        if (expr->is_foldable && expr->is(NodeKind::Number)) {
            
            Number* number = as<Number*>(expr);

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
    case TypeKind::Pointer: {
        if (expr->is(NodeKind::String)) {
            if (expr->type->is(context.const_char_ptr_type) &&
                (to_type->is(context.const_char16_ptr_type) || to_type->is(context.const_char32_ptr_type))) {
                return true;
            } else if (expr->type->is(context.const_char16_ptr_type) &&
                       to_type->is(context.const_char32_ptr_type)) {
                return true;
            }
        } else if (from_type->is_array()) {
            auto to_arr_Type = as<ArrayType*>(to_type);
            auto from_ptr_type = as<PointerType*>(from_type);

            auto to_elm_type = to_arr_Type->get_elm_type();
            auto from_elm_type = from_ptr_type->get_elm_type();

            return to_elm_type->is(from_elm_type);
        }
        
        return to_type->is(from_type) || expr->is(NodeKind::Null) || to_type->is(context.void_ptr_type);
    }
    case TypeKind::Array: {
        if (expr->is(NodeKind::Array)) {
            auto to_arr_type = as<ArrayType*>(to_type);
            auto from_arr_type = as<ArrayType*>(from_type);

            while (to_arr_type->get_elm_type()->is_array()) {
                if (!from_arr_type->get_elm_type()->is_array()) {
                    // Not the same dimensionality.
                    return false;
                }

                // Allow for zero filling the remainder.
                if (from_arr_type->get_length() > to_arr_type->get_length()) {
                    return false;
                }

                to_arr_type = as<ArrayType*>(to_arr_type->get_elm_type());
                from_arr_type = as<ArrayType*>(from_arr_type->get_elm_type());
            }
            
            if (to_arr_type->get_elm_type()->is_not(from_arr_type->get_elm_type())) {
                return false;
            }
            // Allow for zero filling the remainder.
            return true;
        }
        return to_type->is(from_type);
    }
    default:
        return to_type->is(from_type);
    }
}

bool acorn::Sema::is_castable_to(Type* to_type, Expr* expr) const {
    if ((to_type->is_real_pointer() || to_type->is_integer()) &&
        (expr->type->is_real_pointer() || expr->type->is_integer())) {
        Type* from_type = expr->type;
        if (!try_remove_const_for_compare(to_type, from_type, expr)) {
            return false;
        }
        // Pointers and numbers can cast to each other.
        return true;
    } else {
        return is_assignable_to(to_type, expr);
    }
}

bool acorn::Sema::try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr) const {

    if (to_type->is_array() && from_type->does_contain_const()) {
        if (!from_type->is_array()) {
            if (from_type->is_pointer()) {
                auto to_arr_Type = as<ArrayType*>(to_type);
                auto from_ptr_type = as<PointerType*>(from_type);

                auto to_elm_type = to_arr_Type->get_elm_type();
                auto from_elm_type = from_ptr_type->get_elm_type();

                if (from_elm_type->does_contain_const() && !to_elm_type->does_contain_const()) {
                    return false;
                }
            }

            return false;
        }

        auto to_arr_Type = as<ArrayType*>(to_type);
        Type* to_base_type = to_arr_Type->get_base_type();

        auto from_arr_Type = as<ArrayType*>(from_type);
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

            auto to_ptr_type = as<PointerType*>(to_type_itr);
            auto from_ptr_type = as<PointerType*>(from_type_itr);

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
            as<IdentRef*>(expr)->found_kind == IdentRef::VarKind) {
            error(expand(expr), "Cannot reassign to a const variable")
                .end_error(ErrCode::SemaReassignConstVariable);
        } else {
            error(expand(expr), "Cannot assign to const address")
                .end_error(ErrCode::SemaReassignConstAddress);
        }
    }
}

bool acorn::Sema::is_lvalue(Expr* expr) {
    if (expr->is(NodeKind::IdentRef) || expr->is(NodeKind::MemoryAccess)) {
        return true;
    }

    if (expr->is(NodeKind::UnaryOp)) {
        auto unary = as<UnaryOp*>(expr);
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
    if (expr->type->is_not(to_type)) {
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
        
        return get_error_msg_for_value_not_fit_type(as<Number*>(expr));
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
