#include "SourceExpansion.h"

#include "Util.h"
#include "Logger.h"

namespace acorn {
#define get(n) if (n) {           \
auto [s1, e1] = get_expansion(n); \
if (s1 < s) { s = s1; }           \
if (e1 > e) { e = e1; }           \
}

    static std::pair<const char*, const char*> get_expansion(Node* node) {

        auto s = node->uses_expanded_loc ? node->expanded_loc.ptr : node->loc.ptr;;
        auto e = node->uses_expanded_loc ? (node->expanded_loc.ptr + node->expanded_loc.length)
                                         : (node->loc.ptr + node->loc.length);

        switch (node->kind) {
        case NodeKind::BinOp: {
            BinOp* bin_op = static_cast<BinOp*>(node);
            get(bin_op->lhs);
            get(bin_op->rhs);
            break;
        }
        case NodeKind::UnaryOp: {
            UnaryOp* unary_op = static_cast<UnaryOp*>(node);
            get(unary_op->expr);
            break;
        }
        case NodeKind::Var: {
            Var* var = static_cast<Var*>(node);
            get(var->assignment);
            break;
        }
        case NodeKind::FuncCall: {
            FuncCall* call = static_cast<FuncCall*>(node);
            get(call->site);
            if (!call->args.empty()) {
                auto loc = expand(call->args.back());
                e = loc.ptr + loc.length;
            } else {
                e = call->loc.ptr + call->loc.length;
            }
            // Include the closing )
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::Cast: {
            // Include the closing )
            auto cast = static_cast<Cast*>(node);
            get(cast->value);
            break;
        }
        case NodeKind::NamedValue: {
            NamedValue* named_value = static_cast<NamedValue*>(node);
            get(named_value->assignment);
            break;
        }
        case NodeKind::Array: {
            Array* arr = static_cast<Array*>(node);
            if (!arr->elms.empty()) {
                get(arr->elms.back());
            }
            // Include the closing ]
            go_until(e, '[', ']');
            break;
        }
        case NodeKind::MemoryAccess: {
            MemoryAccess* mem_access = static_cast<MemoryAccess*>(node);
            get(mem_access->site);
            get(mem_access->index);
            // Include the closing ]
            go_until(e, '[', ']');
            break;
        }
        case NodeKind::DotOperator: {
            DotOperator* dot = static_cast<DotOperator*>(node);
            e += dot->ident.to_string().size();
            get(dot->site);
            break;
        }
        case NodeKind::SizeOf: {
            // Include the closing )
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::StructInitializer: {
            // Include the closing }
            go_until(e, '{', '}');
            break;
        }
        case NodeKind::MoveObj: {
            // Include the closing )
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::Ternary: {
            Ternary* ternary = static_cast<Ternary*>(node);
            get(ternary->cond);
            get(ternary->rhs);
            break;
        }
        case NodeKind::TypeExpr: {
            TypeExpr* type_expr = static_cast<TypeExpr*>(node);
            if (type_expr->prev_node_kind == NodeKind::MemoryAccess) {
                MemoryAccess* mem_access = static_cast<MemoryAccess*>(node);
                get(mem_access->site);
                get(mem_access->index);
                // Include the closing ]
                go_until(e, '[', ']');
            }
            break;
        }
        case NodeKind::Reflect: {
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::RaiseStmt: {
            RaiseStmt* raise = static_cast<RaiseStmt*>(node);
            get(raise->expr);
            break;
        }
        case NodeKind::Try: {
            Try* tryn = static_cast<Try*>(node);
            get(tryn->caught_expr);
            break;
        }
        case NodeKind::Number:
        case NodeKind::IdentRef:
        case NodeKind::String:
        case NodeKind::Null:
        case NodeKind::Bool:
        case NodeKind::This:
            break;
        default:
            acorn_fatal("get_expansion(): missing case");
            break;
        }
        return {s, e};
    }
}

void acorn::go_until(const char*& e, char open, char close) {

    while (*e != '\0') {
        // It is possible that when calling this the end already
        // passed the opening so just check for either.
        if (*e == open) {
            ++e;
            break;
        } else if (*e == close) {
            break;
        }
        ++e;
    }

    int count = 1;

    // Only include '\0' for safety but only it should
    // not be needed.
    while (count > 0 && *e != '\0') {
        if (*e == close) {
            --count;
        }
        if (*e == open) {
            ++count;
        }

        ++e;
    }
    if (*e == '\0') {
        --e;
    }
    //++e;
}

acorn::PointSourceLoc acorn::expand(Node* node) {
    auto [s, e] = get_expansion(node);

    // Balacing parenethsis.
    //
    // Balance forward.
    int paran_count = 0;
    {
        const char* sp = s;
        while ((sp < e || paran_count > 0) && *sp != '\0') {
            if (*sp == '(') {
                ++paran_count;
            } else if (*sp == ')') {
                --paran_count;
            }
            ++sp;
        }
        e = sp;
    }
    // Balance backwards.
    if (paran_count < 0) {
        const char* sp = s;
        while (paran_count < 0 && *sp != '\0') {
            if (*sp == '(') {
                ++paran_count;
            } else if (*sp == ')') {
                --paran_count;
            }
            --sp;
        }
        ++sp;
        s = sp;
    }

    return PointSourceLoc{
        s,
        static_cast<uint16_t>(e - s),
        node->uses_expanded_loc ? node->expanded_loc.point        : node->loc.ptr,
        node->uses_expanded_loc ? node->expanded_loc.point_length : node->loc.length
    };
}
