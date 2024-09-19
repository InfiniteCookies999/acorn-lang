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
            BinOp* bin_op = as<BinOp*>(node);
            get(bin_op->lhs);
            get(bin_op->rhs);
            break;
        }
        case NodeKind::UnaryOp: {
            UnaryOp* unary_op = as<UnaryOp*>(node);
            get(unary_op->expr);
            break;
        }
        case NodeKind::Var: {
            Var* var = as<Var*>(node);
            get(var->assignment);
            break;
        }
        case NodeKind::FuncCall: {
            FuncCall* call = as<FuncCall*>(node);
            get(call->site);
            if (!call->args.empty()) {
                auto loc = expand(call->args.back());
                e = loc.ptr + loc.length;
            } else {
                e = call->loc.ptr + call->loc.length;
            }
            // Only include '\0' for safety but only it should
            // not be needed.
            while (*e != ')' && *e != '\0') {
                ++e;
            }
            if (*e == '\0') {
                --e;
            }
            ++e;
            break;
        }
        case NodeKind::Cast: {
            Cast* cast = as<Cast*>(node);
            get(cast->value);
            break;
        }
        case NodeKind::NamedValue: {
            NamedValue* named_value = as<NamedValue*>(node);
            get(named_value->assignment);
            break;
        }
        case NodeKind::Number:
        case NodeKind::IdentRef:
        case NodeKind::String:
        case NodeKind::Null:
        case NodeKind::Bool:
            break;
        default:
            acorn_fatal("get_expansion(): missing case");
            break;
        }
        return {s, e};
    }
}

acorn::PointSourceLoc acorn::expand(Node* node) {
    auto [s, e] = get_expansion(node);
    
    return PointSourceLoc{
        s,
        as<uint16_t>(e - s),
        node->uses_expanded_loc ? node->expanded_loc.ptr    : node->loc.ptr,
        node->uses_expanded_loc ? node->expanded_loc.length : node->loc.length
    };
}
