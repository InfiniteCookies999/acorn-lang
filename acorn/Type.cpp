#include "Type.h"

#include "PageAllocator.h"
#include "Logger.h"
#include "Util.h"
#include "TypeTable.h"
#include "Context.h"

acorn::Type* acorn::Type::create(PageAllocator& allocator, TypeKind kind, bool is_const) {
    Type* type = allocator.alloc_type<Type>();
    type->contains_const = is_const;
    if (!is_const) {
        type->non_const_version = type;
    }
    return new (type) Type(kind, is_const);
}

bool acorn::Type::is_comparable() const {
    return is_number() || is_pointer() || kind == TypeKind::Null;
}

uint32_t acorn::Type::get_number_of_bits() const {
    switch (kind) {
    case TypeKind::Void:   return 0;
    case TypeKind::Int:    return 32;
    case TypeKind::Int8:   return 8;
    case TypeKind::Int16:  return 16;
    case TypeKind::Int32:  return 32;
    case TypeKind::Int64:  return 64;
    case TypeKind::UInt8:  return 8;
    case TypeKind::UInt16: return 16;
    case TypeKind::UInt32: return 32;
    case TypeKind::UInt64: return 64;
    case TypeKind::Bool:   return 8;
    case TypeKind::Char:   return 8;
    case TypeKind::Char16: return 16;
    case TypeKind::Char32: return 32;
    default:
        acorn_fatal("unimplemented case");
        return 0;
    }
}

std::string acorn::Type::to_string() const {
#define str(s) !is_const() ? s : "const " s;
    switch (kind) {
    case TypeKind::Void:      return str("void");
    case TypeKind::Int:       return str("int");
    case TypeKind::Int8:      return str("int8");
    case TypeKind::Int16:     return str("int16");
    case TypeKind::Int32:     return str("int32");
    case TypeKind::Int64:     return str("int64");
    case TypeKind::UInt8:     return str("uint8");
    case TypeKind::UInt16:    return str("uint16");
    case TypeKind::UInt32:    return str("uint32");
    case TypeKind::UInt64:    return str("uint64");
    case TypeKind::Invalid:   return str("invalid");
    case TypeKind::FuncsRef:  return str("function reference");
    case TypeKind::ModuleRef: return str("module reference");
    case TypeKind::Bool:      return str("bool");
    case TypeKind::Char:      return str("char");
    case TypeKind::Char16:    return str("char16");
    case TypeKind::Char32:    return str("char32");
    case TypeKind::Null:      return str("null");
    case TypeKind::ISize:     return str("isize");
    case TypeKind::USize:     return str("usize");
    case TypeKind::Pointer:   return as<const PointerType*>(this)->to_string();
    default:
        acorn_fatal("Type::to_string() missing to_string case");
        return "";
    }
#undef str
}

acorn::Type* acorn::PointerType::create(PageAllocator& allocator, Type* elm_type, bool is_const) {
    PointerType* ptr_type = allocator.alloc_type<PointerType>();
    ptr_type->contains_const = is_const;
    return new (ptr_type) PointerType(is_const, elm_type);
}

std::string acorn::PointerType::to_string() const {
    return elm_type->to_string() + "*";
}
