#include "Sema.h"

#include "Util.h"
#include "Context.h"
#include "Type.h"
#include "SourceExpansion.h"
#include "ir/IRGen.h"
#include "Module.h"

/* Utility for returning from check functions if an error occures */
#define nvalid(n) !(n->type)
#define yield_if(n) if (nvalid(n)) return;
#define check(n) check_node(n); yield_if(n)

acorn::Sema::Sema(Context& context, Module & modl, Logger& logger)
    : context(context), modl(modl), logger(logger), type_table(context.type_table) {
}

void acorn::Sema::resolve_global_comptime(Context& context, Module& modl) {
    for (Node* node : modl.get_comptime_control_flows()) {
        if (node->is(NodeKind::ComptimeIfStmt)) {
            auto ifs = as<ComptimeIfStmt*>(node);
            Sema sema(context, modl, ifs->file->logger);
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
                              .add_line([prev_main](Logger& l) { prev_main->get_declared_msg(l); })
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

// Statement checking
//--------------------------------------

void acorn::Sema::check_for_duplicate_functions(Module& modl) {
    for (const auto& [_, funcs] : modl.get_global_functions()) {
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
                       .add_line([decl2](Logger& l) { decl2->get_declared_msg(l); })
                       .end_error(error_code);
}

void acorn::Sema::check_node(Node* node) {
    switch (node->kind) {
    case NodeKind::Var:
        return check_variable(as<Var*>(node));
    case NodeKind::IdentRef:
        return check_ident_ref(as<IdentRef*>(node), false);
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
    case NodeKind::Number:
    case NodeKind::Bool:
    case NodeKind::String:
    case NodeKind::Null:
        break;
    case NodeKind::ScopeStmt: {
        SemScope sem_scope;
        return check_scope(as<ScopeStmt*>(node), sem_scope);
    }
    default:
        acorn_fatal("check_node(): missing case");
    }
}

void acorn::Sema::check_function(Func* func) {
    if (func->has_modifier(Modifier::Native)) return;

    // Logger::debug("checking function: %s", func->name);

    check_modifier_incompatibilities(func);

    cur_func = func;

    // If we ever decide to allow nesting functions for some reason then this
    // will possibly be a problem because it will have overriden the current scope.
    SemScope sem_scope;
    cur_scope = &sem_scope;
    for (Var* param : func->params) {
        check_variable(param);
    }
    cur_scope = nullptr;

    check_scope(func->scope, sem_scope);
    if (!sem_scope.all_paths_return && func->return_type->is_not(context.void_type)) {
        error(func, "Not all function paths return")
            .end_error(ErrCode::SemaNotAllFuncPathReturn);
    }
}

void acorn::Sema::check_variable(Var* var) {

    check_modifier_incompatibilities(var);
    
    // If not a global variable we need to tell the current function to
    // provide us with stack memory.
    if (cur_func && !var->is_param() && !var->is_global) {
        cur_func->vars_to_alloc.push_back(var);
    } else if (var->is_global) {
        context.queue_gen(var);
    }

    if (cur_scope) {
        if (Var* prev_var = cur_scope->find_variable(var->name)) {
            logger.begin_error(var->loc, "Duplicate declaration of variable '%s'", var->name)
                  .add_line([prev_var](Logger& l) { prev_var->get_declared_msg(l); })
                  .end_error(ErrCode::SemaDuplicateLocVariableDecl);
        } else {
            cur_scope->variables.push_back(var);
        }
    }

    if (var->assignment) {
        check(var->assignment);
    }

    if (var->type->is(context.void_type)) {
        error(var, "Variables cannot have type 'void'")
            .end_error(ErrCode::SemaVariableCannotHaveVoidType);
        return;
    }

    if (!var->assignment && var->type->is_const() &&
        !var->has_modifier(Modifier::Native) && !var->is_param()) {
        error(var, "Variables declared const must be assigned a value")
            .end_error(ErrCode::SemaVariableConstNoValue);
        return;
    }

    if (var->assignment && !is_assignable_to(var->type, var->assignment)) {
        error(expand(var), get_type_mismatch_error(var->type, var->assignment).c_str())
            .end_error(ErrCode::SemaVariableTypeMismatch);
    } else if (var->assignment) {
        create_cast(var->assignment, var->type);
    }
}

void acorn::Sema::check_return(ReturnStmt* ret) {
    cur_scope->all_paths_return = true;

    bool is_assignable;
    if (ret->value) {
        check(ret->value);
        is_assignable = is_assignable_to(cur_func->return_type, ret->value);
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
    check(ifs->cond);
    check_condition(ifs->cond);
    
    SemScope sem_scope;
    check_scope(ifs->scope, sem_scope);
    all_paths_return = sem_scope.all_paths_return;
    
    if (ifs->elseif && ifs->elseif->is(NodeKind::IfStmt)) {
        bool all_paths_return2;
        check_if(as<IfStmt*>(ifs->elseif), all_paths_return2);
        all_paths_return &= all_paths_return2;
    } else if (ifs->elseif) {
        SemScope sem_scope;
        check_scope(as<ScopeStmt*>(ifs->elseif), sem_scope);
    }

    cur_scope->all_paths_return = all_paths_return;
}

void acorn::Sema::check_comptime_if(ComptimeIfStmt* ifs) {
    check(ifs->cond);
    if (!check_condition(ifs->cond)) {
        return;
    }
    if (!ifs->cond->is_foldable) {
        error(expand(ifs->cond), "Comptime if expects the condition to be determined at compile time")
            .end_error(ErrCode::SemaNotComptimeCompute);
        return;
    }

    // Note: We will only check the scope if the path is taken. If it is not
    //       then we could possibly run into errors that do not actually represent
    //       the state of the problem. For example a variable being declared on one
    //       operating system and trying to access it but not on another.

    auto ll_cond = llvm::cast<llvm::ConstantInt>(gen_constant(expand(ifs->cond), ifs->cond));
    if (!ll_cond->isZero()) {
        // Path taken!
        ifs->takes_path = true;
        SemScope sem_scope;
        check_scope(ifs->scope, sem_scope);
        if (cur_scope) { // Have to check because possible it is a global context.
            cur_scope->all_paths_return = sem_scope.all_paths_return;
        }
    } else if (ifs->elseif) {
        ifs->takes_path = false;
        if (ifs->elseif->is(NodeKind::ComptimeIfStmt)) {
            // Onto the next possible comptime condition.
            check_comptime_if(as<ComptimeIfStmt*>(ifs->elseif));
        } else {
            // Must have failed all other branches.
            SemScope sem_scope;
            check_scope(as<ScopeStmt*>(ifs->elseif), sem_scope);
            if (cur_scope) { // Have to check because possible it is a global context.
                cur_scope->all_paths_return = sem_scope.all_paths_return;
            }
        }
    }
}

void acorn::Sema::check_scope(ScopeStmt* scope, SemScope& new_sem_scope) {
    new_sem_scope.parent = cur_scope;
    cur_scope = &new_sem_scope;

    for (Node* stmt : *scope) {
        if (new_sem_scope.all_paths_return) {
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
                modl.add_global_function(as<Func*>(stmt));
            } else {
                error(stmt, "Functions cannot be declared within another function")
                    .end_error(ErrCode::SemaNoLocalFuncs);
            }
        } else if (stmt->is(NodeKind::Var) && is_global_comptime) {
            modl.add_global_variable(as<Var*>(stmt));
        } else {
            check_node(stmt);
        }
    }

    cur_scope = cur_scope->parent;
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

    auto check_add_sub_mul = [bin_op, this, lhs, rhs, error_cannot_apply, error_mismatched]() finline {
        // valid pointer arithmetic cases:
        // 
        // ptr + int
        // int + ptr
        // 
        // ptr - int
        // ptr - ptr
        
        if (lhs->type->is_pointer()) {
            // Pointer arithmetic
            if (bin_op->op == '+' && !rhs->type->is_integer()) {
                error_mismatched();
                return false;
            } else if (!(rhs->type->is_integer() || rhs->type->is(lhs->type))) {
                error_mismatched();
                return false;
            }

            return true;
        } else if (rhs->type->is_pointer()) {
            // Pointer arithmetic
            if (bin_op->op == '+' && !lhs->type->is_integer()) {
                error_mismatched();
                return false;
            } else if (bin_op->op == '-') {
                // ptr - ptr   case would have already been handled.
                error_mismatched();
                return false;
            }

            return true;
        }

        if (!lhs->type->is_number()) {
            error_cannot_apply(lhs);
            return false;
        }
        if (!rhs->type->is_number()) {
            error_cannot_apply(rhs);
            return false;
        }
        if (!rhs->type->is(lhs->type)) {
            error_mismatched();
            return false;
        }
        return true;
    };

    auto check_div_mod = [this, bin_op, lhs, rhs, error_cannot_apply, error_mismatched]() finline {
        if (!lhs->type->is_number()) {
            error_cannot_apply(lhs);
            return false;
        }   
        if (!rhs->type->is_number()) {
            error_cannot_apply(rhs);
            return false;
        }
            
        check_division_by_zero(expand(bin_op), rhs);
        
        if (!rhs->type->is(lhs->type)) {
            error_mismatched();
            return false;
        }
        return true;
    };

    auto check_logical_bitwise = [this, lhs, rhs, error_cannot_apply, error_mismatched]() finline -> bool{
        if (!(lhs->type->is_integer() || lhs->type->is(context.bool_type))) {
            error_cannot_apply(lhs);
            return false;
        }
        if (!(rhs->type->is_integer() || rhs->type->is(context.bool_type))) {
            error_cannot_apply(rhs);
            return false;
        }
        if (!rhs->type->is(lhs->type)) {
            error_mismatched();
            return false;
        }
        return true;
    };

    auto check_shifts = [this, lhs, rhs, error_cannot_apply, error_mismatched]() finline -> bool {
        if (!lhs->type->is_integer()) {
            error_cannot_apply(lhs);
            return false;
        }
        if (!rhs->type->is_integer()) {
            error_cannot_apply(rhs);
            return false;
        }
        if (!rhs->type->is(lhs->type)) {
            error_mismatched();
            return false;
        }
        return true;
    };

    check(lhs);
    check(rhs);

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
            if (!check_add_sub_mul()) return;
            break;
        }
        case Token::DivEq: case Token::ModEq: {
            if (!check_div_mod()) return;
            break;
        }
        case Token::AndEq: case Token::OrEq: case Token::CaretEq: {
            if (!check_logical_bitwise()) return;
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
            if (!rhs->type->is(lhs->type)) {
                error_mismatched();
                return;
            }
            break;
        case Token::LtLtEq: case Token::GtGtEq: {
            if (check_shifts()) return;
            break;
        }
        }
        }

        bin_op->type = bin_op->lhs->type;
        break;
    }
    case '+': case '-': case '*': {
        if (!check_add_sub_mul()) return;
        // Pointer type takes preference.
        if (lhs->type->is_pointer() && rhs->type->is_pointer()) {
            // This is a unique case of subtracting pointers which
            // results in an integer type.
            bin_op->type = context.isize_type;
        } else if (lhs->type->is_pointer()) {
            bin_op->type = lhs->type;
        } else if (rhs->type->is_pointer()) {
            bin_op->type = rhs->type;
        } else {
            bin_op->type = lhs->type;
        }
        break;
    }
    case '/': case '%': {
        if (!check_div_mod()) return;
        bin_op->type = lhs->type;
        break;
    }
    case '^': case '&': case '|': {
        if (!check_logical_bitwise()) return;
        bin_op->type = lhs->type;
        break;
    }
    case Token::LtLt: case Token::GtGt: {
        if (!check_shifts()) return;
        bin_op->type = lhs->type;
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
        if (!rhs->type->is(lhs->type)) {
            error_mismatched();
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
    check(expr);

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
        if (!expr->type->is(context.bool_type)) {
            error_no_applies();
            return;
        }
        unary_op->type = expr->type;
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

void acorn::Sema::check_ident_ref(IdentRef* ref, bool is_for_call) {
    
    auto find_function = [this, ref]() finline{
        if (auto* funcs = modl.find_global_funcs(ref->ident)) {
            ref->set_funcs_ref(funcs);
        }
    };

    auto find_variable = [this, ref]() finline {
        if (cur_scope) {
            if (auto* var = cur_scope->find_variable(ref->ident)) {
                ref->set_var_ref(var);
                return;
            }
        }
        if (auto* var = modl.find_global_variable(ref->ident)) {
            ref->set_var_ref(var);
            return;
        }
        if (auto* universal = context.get_universal_constant(ref->ident)) {
            ref->set_universal(universal);
        }
    };

    if (is_for_call) {
        find_function();
        if (!ref->found_kind) {
            find_variable();
        }
    } else {
        find_variable();
        if (!ref->found_kind) {
            find_function();
        }
    }
    
    switch (ref->found_kind) {
    case IdentRef::VarKind: {
        ref->is_foldable = false;
        
        Var* var_ref = ref->var_ref;
        if (!var_ref->generated && var_ref->is_global) {
            Sema sema(context, var_ref->get_module(), var_ref->get_logger());
            sema.check_variable(var_ref);
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
    case IdentRef::NoneKind: {
        error(ref, "Could not find %s '%s'", is_for_call ? "function" : "identifier", ref->ident)
            .end_error(!is_for_call ? ErrCode::SemaNoFindIdentRef : ErrCode::SemaNoFindFuncIdentRef);
        break;
    }
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
        check_ident_ref(ref, true);
        yield_if(ref);

        if (ref->found_kind != IdentRef::FuncsKind) {
            is_callable = false;
        }
    } else {
        check(call->site);
        is_callable = true;
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
        create_cast(call->args[i], called_func->params[i]->type);
    }

    call->called_func = called_func;
    call->type = called_func->return_type;
    context.queue_gen(call->called_func);

}

acorn::Func* acorn::Sema::find_best_call_canidate(FuncList& canidates,
                                                  llvm::SmallVector<Expr*, 8>& args) {
    // TODO: Need to check the arguments of the canidate functions.
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

    for (size_t i = 0; i < canidate->params.size(); i++) {
        Expr* arg   = args[i];
        Var*  param = canidate->params[i];
    
        if (!is_assignable_to(param->type, arg)) {
            return false;
        }

        if (param->type->is_not(arg->type)) {
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

    for (size_t i = 0; i < params.size(); i++) {
        Expr* arg = args[i];
        Var* param = params[i];

        if (!is_assignable_to(param->type, arg)) {
            err_line("Wrong type for arg %s. Expected '%s' but found '%s'",
                i + 1, param->type, arg->type);
        }
    }

#undef err_line
}

void acorn::Sema::check_cast(Cast* cast) {
    cast->type = cast->explicit_cast_type;
    
    check(cast->value);
    if (!is_castable_to(cast->explicit_cast_type, cast->value)) {
        error(expand(cast), "Cannot cast to type '%s' from '%s'",
              cast->explicit_cast_type, cast->value)
            .end_error(ErrCode::SemaInvalidCast);
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
        }
        
        return to_type->is(from_type) || expr->is(NodeKind::Null) || to_type->is(context.void_ptr_type);
    }
    default:
        return to_type->is(from_type);
    }
}

bool acorn::Sema::is_castable_to(Type* to_type, Expr* expr) const {
    if ((to_type->is_pointer() || to_type->is_integer()) &&
        (expr->type->is_pointer() || expr->type->is_integer())) {
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
    if (to_type->is_pointer()) {
        if (expr->is(NodeKind::Null)) {
            return true;
        }

        auto to_type_itr = to_type;
        auto from_type_itr = expr->type;
        bool to_contains_const = false, from_contains_const = false;
        do {
            if (!from_type_itr->is_pointer()) {
                // The pointers do not have the same depth.
                return false;
            }

            auto to_ptr_type = as<PointerType*>(to_type_itr);
            auto from_ptr_type = as<PointerType*>(from_type_itr);

            auto to_elm_type = to_ptr_type->get_elm_type();
            auto from_elm_type = from_ptr_type->get_elm_type();

            from_contains_const = from_elm_type->is_const();
            to_contains_const   = to_elm_type->is_const();
            if (from_elm_type->is_const() && !to_elm_type->is_const()) {
                // Underlying memory is const by the assignment's not
                // const memory.
                return false;
            }
            to_type_itr = to_elm_type;
            from_type_itr = from_elm_type;
        } while (to_type_itr->is_pointer());

        if (from_contains_const) {
            from_type = from_type->remove_all_const();
        }
        if (to_contains_const) {
            to_type = to_type->remove_all_const();
        }
    }

    if (to_type->does_contain_const()) {
        to_type = to_type->remove_all_const();
    }
    if (from_type->does_contain_const()) {
        from_type = from_type->remove_all_const();
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
    return expr->is(NodeKind::IdentRef);
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
    if (!is_condition(cond)) {
        error(expand(cond), "Expected condition")
            .end_error(ErrCode::SemaExpectedCondition);
        return false;
    }
    return true;
}

bool acorn::Sema::is_condition(Expr* cond) const {
    return cond->type->is(context.bool_type);
}

void acorn::Sema::check_modifier_incompatibilities(Decl* decl) {
    if (decl->has_modifier(Modifier::DllImport) && !decl->has_modifier(Modifier::Native)) {
        error(decl->get_modifier_location(Modifier::DllImport),
              "Cannot have dllimport modifier without native modifier")
            .end_error(ErrCode::SemaDllImportWithoutNativeModifier);
    }
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
