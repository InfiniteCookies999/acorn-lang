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

acorn::Var* acorn::Func::find_parameter(Identifier name) const {
    auto itr = std::ranges::find_if(params, [name](Var* param) {
        return param->name == name;
    });
    return itr != params.end() ? *itr : nullptr;
}

acorn::Var* acorn::Struct::find_field(Identifier name) const {
    return nspace->find_variable(name);
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
