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
        case NodeKind::BIN_OP: {
            BinOp* bin_op = static_cast<BinOp*>(node);
            get(bin_op->lhs);
            get(bin_op->rhs);
            break;
        }
        case NodeKind::UNARY_OP: {
            UnaryOp* unary_op = static_cast<UnaryOp*>(node);
            get(unary_op->expr);
            break;
        }
        case NodeKind::VAR: {
            Var* var = static_cast<Var*>(node);
            get(var->assignment);
            break;
        }
        case NodeKind::UNINIT_NEW_CALL_STMT: {
            UninitNewCallStmt* new_call = static_cast<UninitNewCallStmt*>(node);
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::FUNC_CALL: {
            FuncCall* call = static_cast<FuncCall*>(node);
            get(call->site);
            // Include the closing )
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::CAST: {
            auto cast = static_cast<Cast*>(node);
            // Include the value being cast on.
            get(cast->value);
            break;
        }
        case NodeKind::BITCAST: {
            auto cast = static_cast<BitCast*>(node);
            // Include the value being cast on.
            get(cast->value);
            break;
        }
        case NodeKind::NAMED_VALUE: {
            NamedValue* named_value = static_cast<NamedValue*>(node);
            get(named_value->assignment);
            break;
        }
        case NodeKind::ARRAY: {
            Array* arr = static_cast<Array*>(node);
            if (!arr->elms.empty()) {
                get(arr->elms.back());
            }
            // Include the closing ]
            go_until(e, '[', ']');
            break;
        }
        case NodeKind::MEMORY_ACCESS: {
            MemoryAccess* mem_access = static_cast<MemoryAccess*>(node);
            get(mem_access->site);
            get(mem_access->index);
            // Include the closing ]
            go_until(e, '[', ']');
            break;
        }
        case NodeKind::DOT_OPERATOR: {
            DotOperator* dot = static_cast<DotOperator*>(node);
            // There can still be whitespace after the .
            //
            // This is valid:
            // a.  b;
            while (is_whitespace(*e)) {
                ++e;
            }
            e += dot->ident.to_string().size();
            // Still have to traverse the site because `expand` doesn't just
            // impact the dot operator alone. Expand gets called recursively
            // and the assumption is that the children will expand to cover
            // their entire expression.
            get(dot->site);
            break;
        }
        case NodeKind::SIZE_OF: {
            // Include the closing )
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::STRUCT_INITIALIZER: {
            // Include the closing }
            go_until(e, '{', '}');
            StructInitializer* initializer = static_cast<StructInitializer*>(node);
            get(initializer->site);
            break;
        }
        case NodeKind::MOVEOBJ: {
            // Include the closing )
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::TERNARY: {
            Ternary* ternary = static_cast<Ternary*>(node);
            get(ternary->cond);
            get(ternary->rhs);
            break;
        }
        case NodeKind::TYPE_EXPR: {
            break;
        }
        case NodeKind::REFLECT: {
            go_until(e, '(', ')');
            break;
        }
        case NodeKind::RAISE_STMT: {
            RaiseStmt* raise = static_cast<RaiseStmt*>(node);
            get(raise->expr);
            break;
        }
        case NodeKind::TRY: {
            Try* tryn = static_cast<Try*>(node);
            get(tryn->caught_expr);
            break;
        }
        case NodeKind::IDENT_REF: {
            IdentRef* ref = static_cast<IdentRef*>(node);
            if (ref->explicitly_binds_generics) {
                GenericBindFuncCall* call = static_cast<GenericBindFuncCall*>(ref);
                // Include the closing )
                go_until(e, '(', ')');
            }
            break;
        }

        case NodeKind::NUMBER:
        case NodeKind::STRING:
        case NodeKind::NULL_EXPR:
        case NodeKind::BOOL_EXPR:
        case NodeKind::THIS_EXPR:
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
