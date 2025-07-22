#include "GenericReset.h"
#include "Logger.h"

namespace acorn {
    static void reset_node(Node* node) {

        if (node->is_expression()) {
            Expr* expr = static_cast<Expr*>(node);
            switch (expr->kind) {
            // A few nodes have the type set during parsing.
            case NodeKind::Number:
            case NodeKind::Bool:
            case NodeKind::Null:
            case NodeKind::String:
                break;
            default:
                expr->type = nullptr;
                break;
            }
            // Check for nodes which have `trivially_reassignable`
            // set specifically during parsing.
            switch (expr->kind) {
            case NodeKind::BinOp:
            case NodeKind::DotOperator:
            case NodeKind::UnaryOp:
                expr->trivially_reassignable = false;
                break;
            default:
                break;
            }
            expr->cast_type = nullptr;
            expr->is_foldable = true;
            if (expr->tryn) {
                reset_node(expr->tryn);
            }
        }


        switch (node->kind) {
        case NodeKind::Var: {
            auto var = static_cast<Var*>(node);
            var->type = nullptr;
            var->has_been_checked = false;
            var->is_foldable = false;
            var->ll_comptime_value = nullptr;
            var->ll_address = nullptr;
            if (var->assignment) {
                reset_node(var->assignment);
            }
            break;
        }
        case NodeKind::ReturnStmt: {
            auto ret = static_cast<ReturnStmt*>(node);
            reset_node(ret->value);
            break;
        }
        case NodeKind::IfStmt: {
            auto ifn = static_cast<IfStmt*>(node);
            reset_node(ifn->cond);
            if (ifn->post_variable_cond) {
                reset_node(ifn->post_variable_cond);
            }
            if (ifn->elseif) {
                reset_node(ifn->elseif);
            }
            reset_node(ifn->scope);
            break;
        }
        case NodeKind::ScopeStmt: {
            auto scope = static_cast<ScopeStmt*>(node);
            for (Node* stmt : *scope) {
                reset_node(stmt);
            }
            break;
        }
        case NodeKind::PredicateLoopStmt: {
            auto loop = static_cast<PredicateLoopStmt*>(node);
            if (loop->cond) {
                reset_node(loop->cond);
            }
            reset_node(loop->scope);
            break;
        }
        case NodeKind::RangeLoopStmt: {
            auto loop = static_cast<RangeLoopStmt*>(node);
            if (loop->init_node) {
                reset_node(loop->init_node);
            }
            if (loop->cond) {
                reset_node(loop->cond);
            }
            if (loop->inc) {
                reset_node(loop->inc);
            }
            reset_node(loop->scope);
            break;
        }
        case NodeKind::IteratorLoopStmt: {
            auto loop = static_cast<IteratorLoopStmt*>(node);
            if (loop->vars->is(NodeKind::Var)) {
                auto var = static_cast<Var*>(loop->vars);
                reset_node(var);
            }
            reset_node(loop->container);
            reset_node(loop->scope);
            break;
        }
        case NodeKind::ContinueStmt:
        case NodeKind::BreakStmt: {
            // Nothing to reset.
            break;
        }
        case NodeKind::SwitchStmt: {
            auto switchn = static_cast<SwitchStmt*>(node);
            switchn->all_conds_foldable = false;
            if (switchn->on) {
                reset_node(switchn->on);
            }
            if (switchn->default_scope) {
                reset_node(switchn->default_scope);
            }
            for (auto& casen : switchn->cases) {
                reset_node(casen.cond);
                reset_node(casen.scope);
            }
            break;
        }
        case NodeKind::RaiseStmt: {
            auto raise = static_cast<RaiseStmt*>(node);
            reset_node(raise->expr);
            raise->raised_error = nullptr;
            break;
        }
        case NodeKind::RecoverStmt: {
            auto recover = static_cast<RecoverStmt*>(node);
            reset_node(recover->value);
            break;
        }
        case NodeKind::BinOp: {
            auto bin_op = static_cast<BinOp*>(node);
            reset_node(bin_op->lhs);
            reset_node(bin_op->rhs);
            break;
        }
        case NodeKind::UnaryOp: {
            auto unary_op = static_cast<UnaryOp*>(node);
            reset_node(unary_op->expr);
            break;
        }
        case NodeKind::Number:
        case NodeKind::Bool: {
            // Nothing to reset.
            break;
        }
        case NodeKind::IdentRef: {
            auto ref = static_cast<IdentRef*>(node);
            ref->found_kind = IdentRef::NoneKind;
            if (ref->explicitly_binds_generics) {
                auto call = static_cast<GenericBindFuncCall*>(ref);
                call->bound_types.clear();
                for (Expr* arg : call->args) {
                    reset_node(arg);
                }
            }
            break;
        }
        case NodeKind::DotOperator: {
            auto dot = static_cast<DotOperator*>(node);
            dot->found_kind = IdentRef::NoneKind;
            dot->is_array_length = false;
            dot->is_slice_ptr = false;
            dot->is_enum_value = false;
            reset_node(dot->site);
            break;
        }
        case NodeKind::MemoryAccess: {
            auto mem_access = static_cast<MemoryAccess*>(node);
            reset_node(mem_access->site);
            reset_node(mem_access->index);
            break;
        }
        case NodeKind::NamedValue: {
            auto value = static_cast<NamedValue*>(node);
            reset_node(value->assignment);
            break;
        }
        case NodeKind::UninitNewCallStmt: {
            auto new_call = static_cast<UninitNewCallStmt*>(node);
            reset_node(new_call->address);
            reset_node(new_call->value);
            break;
        }
        case NodeKind::FuncCall: {
            auto call = static_cast<FuncCall*>(node);
            reset_node(call->site);
            for (Expr* arg : call->args) {
                reset_node(arg);
            }
            call->called_func = nullptr;
            call->generic_instance = nullptr;
            call->implicitly_converts_return = false;
            call->type_for_type_expr = nullptr;
            call->indeterminate_inferred_default_args.clear();
            break;
        }
        case NodeKind::StructInitializer: {
            auto initializer = static_cast<StructInitializer*>(node);
            reset_node(initializer->site);
            for (Expr* value : initializer->values) {
                reset_node(value);
            }
            initializer->structn = nullptr;
            initializer->called_constructor = nullptr;
            break;
        }
        case NodeKind::String:
        case NodeKind::Null: {
            // Nothing to reset.
            break;
        }
        case NodeKind::Cast: {
            auto cast = static_cast<Cast*>(node);
            reset_node(cast->value);
            break;
        }
        case NodeKind::Array: {
            auto arr = static_cast<Array*>(node);
            for (Expr* elm : arr->elms) {
                reset_node(elm);
            }
            break;
        }
        case NodeKind::This: {
            // Nothing to reset.
            break;
        }
        case NodeKind::SizeOf: {
            auto sof = static_cast<SizeOf*>(node);
            reset_node(sof->value);
            break;
        }
        case NodeKind::Ternary: {
            auto ternary = static_cast<Ternary*>(node);
            reset_node(ternary->cond);
            reset_node(ternary->lhs);
            reset_node(ternary->rhs);
            break;
        }
        case NodeKind::MoveObj: {
            auto moveobj = static_cast<MoveObj*>(node);
            reset_node(moveobj->value);
            break;
        }
        case NodeKind::TypeExpr: {
            auto type_expr = static_cast<TypeExpr*>(node);
            type_expr->expr_type = nullptr;
            break;
        }
        case NodeKind::Reflect: {
            auto reflect = static_cast<Reflect*>(node);
            reset_node(reflect->expr);
            reflect->type_info_type = nullptr;
            break;
        }
        case NodeKind::Try: {
            auto tryn = static_cast<Try*>(node);
            // DONT reset caught expression since it controls
            // cleaning up the try statement.
            //
            // reset_node(tryn->caught_expr);
            tryn->passes_error_along = false;
            if (tryn->caught_var) {
                tryn->caught_var->type = nullptr;
            }
            tryn->ll_error = nullptr;
            tryn->ll_catch_bb = nullptr;
            tryn->ll_end_bb = nullptr;
            break;
        }

        case NodeKind::InvalidExpr:
        case NodeKind::ImportStmt:
        case NodeKind::Enum:
        case NodeKind::Interface:
        case NodeKind::Struct:
        case NodeKind::Generic:
        case NodeKind::VarList:
        case NodeKind::Func:
        case NodeKind::ImplicitFunc:
        default:
            acorn_fatal("Unreachable. All nodes need reset");
            break;
        }
    }
}


void acorn::reset_generic_function(Func* func) {

    // Resetting the parameter types back to not existing.
    //

    // TODO (maddie): Should the functionality of adding the variable to the allocation
    // list be moved to the parser?
    //
    // It is currently being
    func->vars_to_alloc.clear();

    func->structn = nullptr;

    func->return_type = nullptr;
    for (Var* param : func->params) {
        reset_node(param);
    }

    func->num_returns             = 0;
    func->uses_aggr_param         = false;
    func->cannot_use_aggr_ret_var = false;
    func->ll_aggr_int_ret_type    = nullptr;
    func->ll_func                 = nullptr;
    func->aggr_ret_var            = nullptr;

    reset_node(func->scope);

}
