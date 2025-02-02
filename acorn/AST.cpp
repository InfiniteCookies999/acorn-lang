#include "AST.h"

#include "Logger.h"
#include "SourceFile.h"
#include "Type.h"
#include "SourceExpansion.h"

void acorn::Decl::show_prev_declared_msg(Logger& logger) const {
    logger.print("Previously declared at: ");
    show_location_msg(logger);
}

acorn::Logger& acorn::Decl::get_logger() const { return file->logger; }

acorn::Module& acorn::Decl::get_module() const { return file->modl; }

void acorn::Decl::show_location_msg(Logger& logger) const {
    print_source_location(logger, file, loc);
}

const char* acorn::Decl::get_composite_kind() const {
    switch (kind) {
    case NodeKind::Struct: return "struct";
    case NodeKind::Enum:   return "enum";
    default:
        acorn_fatal("Unknown composite kind");
        return "";
    }
}

const char* acorn::Modifier::to_string(uint32_t modifier) {
    switch (modifier) {
    case Native:    return "native";
    case DllImport: return "dllimport";
    case Public:    return "public";
    case Private:   return "private";
    case Readonly:  return "readonly";
    default:
        acorn_fatal("Fail to implement to_string() for modifier");
        return nullptr;
    }
}

acorn::SourceLoc acorn::Decl::get_modifier_location(uint32_t modifier) const {
    // We have to manually implement rfind because otherwise we would have to
    // create a std::string for the file contents which could potentially be
    // slow.
    const char* ptr = loc.ptr;

    auto str = Modifier::to_string(modifier);
    auto str_len = strlen(str);

    const char* buf_beg = file->buffer.content;
    while (ptr - str_len + 1 >= buf_beg) {
        if (std::memcmp(ptr - str_len + 1, str, str_len) == 0) {
            // Found the source location!
            return SourceLoc{ ptr - str_len + 1, static_cast<uint16_t>(str_len) };
        }
        --ptr;
    }

    // We should not get here as long as the user actually specified a modifier
    // that we have.
    acorn_fatal("unreachable");
    return SourceLoc{};
}

acorn::SourceLoc acorn::Func::get_function_const_location() const {
    SourceLoc loc = this->loc;
    const char* start_ptr = loc.ptr + loc.length;
    go_until(start_ptr, '(', ')');
    // Skip over whitespace until we reach 'const' keyword.
    while (is_whitespace(*start_ptr)) {
        ++start_ptr;
    }

    return SourceLoc::from_ptrs(start_ptr, start_ptr + 5);
}

std::string acorn::Func::get_decl_string() const {
    std::string str = name.to_string().str();
    str += "(";
    size_t count = 0;
    for (Var* param : params) {
        str += param->type->to_string();
        if (count + 1 != params.size()) {
            str += ", ";
        }
        ++count;
    }
    str += ")";
    if (is_constant) {
        str += " const";
    }
    return str;
}

bool acorn::Func::forwards_varargs(Expr* arg_value) const {
    if (!uses_varargs) return false;
    if (arg_value->is_not(NodeKind::IdentRef)) return false;

    auto ref = static_cast<IdentRef*>(arg_value);
    auto last_param = params.back();

    if (!ref->is_var_ref()) return false;

    auto var = ref->var_ref;
    if (!var->is_param()) return false;
    if (ref->var_ref != last_param) return false;

    auto candidate_last_param = params.back();
    auto this_slice_type = last_param->type;
    auto candidate_slice_type = candidate_last_param->type;

    if (!has_valid_constness(this_slice_type, candidate_slice_type)) {
        return false;
    }

    this_slice_type = this_slice_type->remove_all_const();
    candidate_slice_type = candidate_slice_type->remove_all_const();
    return this_slice_type->is(candidate_slice_type);
}

acorn::Var* acorn::Func::find_parameter(Identifier name) const {
    auto itr = std::ranges::find_if(params, [name](Var* param) {
        return param->name == name;
    });
    return itr != params.end() ? *itr : nullptr;
}

acorn::Var* acorn::Struct::find_field(Identifier name) const {
    return nspace->find_variable(name);
}

const acorn::Struct::InterfaceExtension* acorn::Struct::find_interface_extension(Identifier name) const {
    auto itr = std::ranges::find_if(interface_extensions, [name](auto& extension) {
        return extension.interfacen->name == name;;
    });
    return itr != interface_extensions.end() ? itr : nullptr;
}

acorn::SourceLoc acorn::Struct::get_extension_location(Identifier name) const {
    // We have to manually implement rfind because otherwise we would have to
    // create a std::string for the file contents which could potentially be
    // slow.
    const char* ptr = loc.ptr;

    auto str = name.to_string().data();
    auto str_len = name.to_string().size();

    const char* buf_end = file->buffer.content + file->buffer.length;
    while (ptr - str_len + 1 < buf_end) {
        if (std::memcmp(ptr, str, str_len) == 0) {
            // Found the source location!
            return SourceLoc{ ptr, static_cast<uint16_t>(str_len) };
        }
        ++ptr;
    }

    acorn_fatal("unreachable");
    return SourceLoc{};
}

namespace acorn {
    template<typename T>
    void iterate_over_range_values(BinOp* range, T start, T end,
                                    const std::function<void(uint64_t)>& cb) {

        T total_range_values = end - start;
        switch (range->op) {
        case Token::RangeEq: {
            for (T v = start; v <= end; v++) {
                cb(static_cast<uint64_t>(v));
            }
            break;
        }
        case Token::RangeLt: {
            for (T v = start; v < end; v++) {
                cb(static_cast<uint64_t>(v));
            }
            break;
        }
        default:
            acorn_fatal("Unreachable. Unknown range operator");
            break;
        }
    }
}

void acorn::iterate_over_range_values(BinOp* range, const std::function<void(uint64_t)>& cb) {
    auto range_type = static_cast<RangeType*>(range->type);
    auto value_type = range_type->get_value_type();

    auto lnum = static_cast<Number*>(range->lhs);
    auto rnum = static_cast<Number*>(range->rhs);

#define iterate(val) iterate_over_range_values(range, lnum->val, rnum->val, cb);

    switch (value_type->get_kind()) {
    case TypeKind::Int:    iterate(value_s32);   break;
    case TypeKind::Int8:   iterate(value_s8);    break;
    case TypeKind::Int16:  iterate(value_s16);   break;
    case TypeKind::Int32:  iterate(value_s32);   break;
    case TypeKind::Int64:  iterate(value_s64);   break;
    case TypeKind::UInt8:  iterate(value_u8);    break;
    case TypeKind::UInt16: iterate(value_u16);   break;
    case TypeKind::UInt32: iterate(value_u32);   break;
    case TypeKind::UInt64: iterate(value_u64);   break;
    // Just use the biggest size we expect out of these types.
    //
    case TypeKind::ISize:  iterate(value_s64);   break;
    case TypeKind::USize:  iterate(value_u64);   break;
    case TypeKind::Char:   iterate(value_u8);    break;
    case TypeKind::Char16: iterate(value_u16);   break;
    case TypeKind::Char32: iterate(value_u32);   break;
    }

#undef iterate
}

uint64_t acorn::get_total_range_values(BinOp* range) {
    auto lnum = static_cast<Number*>(range->lhs);
    auto rnum = static_cast<Number*>(range->rhs);

    switch (range->op) {
    case Token::RangeEq:
        return rnum->value_u64 - lnum->value_u64 + 1;
    case Token::RangeLt:
        return rnum->value_u64 - lnum->value_u64;
    default:
        acorn_fatal("Unreachable. Unknown range type");
        return 0ull;
    }
}
