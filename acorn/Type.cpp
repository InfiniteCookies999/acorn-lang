#include "Type.h"

#include "PageAllocator.h"
#include "Logger.h"
#include "Util.h"
#include "TypeTable.h"
#include "Context.h"
#include "ir/GenTypes.h"
#include "AST.h"


bool acorn::try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr) {

    if (to_type->is_pointer() && expr && expr->is(NodeKind::Null)) {
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

bool acorn::has_valid_constness(Type* to_type, Type* from_type) {

    // There is nothing that can be violated if the from_type does not
    // even contain const.
    if (!from_type->does_contain_const()) {
        return true;
    }

    while (from_type->is_container()) {

        if (!to_type->is_container()) {
            // This case is like:
            //
            // const int** a;
            // int* b = as(int*) a;
            //      ^
            //      `b` points to the address of the other pointer
            //          but the other pointer isn't specified as having
            //          a constant address, so it is fine.
            //
            return true;
        }

        auto to_ctr_type   = static_cast<ContainerType*>(to_type);
        auto from_ctr_type = static_cast<ContainerType*>(from_type);

        auto to_elm_type   = to_ctr_type->get_elm_type();
        auto from_elm_type = from_ctr_type->get_elm_type();

        if (from_elm_type->is_const() && !to_elm_type->is_const()) {
            return false;
        }

        to_type = to_elm_type;
        from_type = from_elm_type;

    }

    return true;
}

acorn::Type* acorn::Type::create(PageAllocator& allocator, TypeKind kind, bool is_const) {
    Type* type = allocator.alloc_type<Type>();
    type->contains_const = is_const;
    if (!is_const) {
        type->non_const_version = type;
    }
    return new (type) Type(kind, is_const);
}

bool acorn::Type::is_comparable() const {
    switch (kind) {
    case TypeKind::Int:
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::ISize:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::USize:
    case TypeKind::Char:
    case TypeKind::Char16:
    case TypeKind::Pointer:
    case TypeKind::Enum:
    case TypeKind::Null:
    case TypeKind::Bool:
        return true;
    default:
        return false;
    }
}

bool acorn::Type::is_default_foldable() const {
    // TODO (maddie): enums?
    switch (kind) {
    case TypeKind::Int:
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::ISize:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::USize:
    case TypeKind::Char:
    case TypeKind::Char16:
    case TypeKind::Float32:
    case TypeKind::Float64:
    case TypeKind::Bool:
    case TypeKind::Pointer:
        return true;
    case TypeKind::Struct: {
        auto struct_type = static_cast<const StructType*>(this);
        auto field_struct = struct_type->get_struct();
        return field_struct->is_default_foldable;
    }
    case TypeKind::Array: {
        auto arr_type = static_cast<const ArrayType*>(this);
        auto elm_type = arr_type->get_elm_type();
        return elm_type->is_default_foldable();
    }
    default:
        return false;
    }
}

bool acorn::Type::is_sized() const {
    switch (kind) {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::Bool:
    case TypeKind::Char:
    case TypeKind::Char16:
    case TypeKind::Float32:
    case TypeKind::Float64:
    case TypeKind::Pointer:
    case TypeKind::Function:
    case TypeKind::Array:
    case TypeKind::ISize:
    case TypeKind::USize:
        return true;
    default:
        return false;
    }
}

uint32_t acorn::Type::get_number_of_bits() const {
    switch (kind) {
    case TypeKind::Void:    return 0;
    case TypeKind::Int:     return 32;
    case TypeKind::Int8:    return 8;
    case TypeKind::Int16:   return 16;
    case TypeKind::Int32:   return 32;
    case TypeKind::Int64:   return 64;
    case TypeKind::UInt8:   return 8;
    case TypeKind::UInt16:  return 16;
    case TypeKind::UInt32:  return 32;
    case TypeKind::UInt64:  return 64;
    case TypeKind::Bool:    return 8;
    case TypeKind::Char:    return 8;
    case TypeKind::Char16:  return 16;
    case TypeKind::Float32: return 32;
    case TypeKind::Float64: return 64;
    case TypeKind::Enum: {
        auto enum_type = static_cast<const EnumType*>(this);
        return enum_type->get_index_type()->get_number_of_bits();
    }
    default:
        acorn_fatal("unimplemented case");
        return 0;
    }
}

bool acorn::Type::needs_destruction() const {
    if (kind == TypeKind::Struct) {
        auto struct_type = static_cast<const StructType*>(this);
        auto structn = struct_type->get_struct();
        return structn->needs_destruction;
    } else if (kind == TypeKind::Array) {
        auto arr_type = static_cast<const ArrayType*>(this);
        auto base_type = arr_type->get_base_type();
        if (base_type->is_struct()) {
            auto struct_type = static_cast<const StructType*>(base_type);
            auto structn = struct_type->get_struct();
            return structn->needs_destruction;
        }
    }

    return false;
}

std::string acorn::Type::to_string() const {

    if (container_enum_type) {
        auto enumn = container_enum_type->get_enum();
        std::string s = enumn->name.to_string().str();
        if (is_const()) {
            s = "const " + s;
        }
        auto contained_type_str = container_enum_type->get_values_type()->to_string();
        return s + "[" + contained_type_str + "]";
    }

#define str(s) !is_const() ? (s) : std::string("const ") + (s);
#define str2(s) !is_const() ? (s) : std::string("const (") + s + ")";
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
    case TypeKind::NamespaceRef: return str("module reference");
    case TypeKind::Bool:      return str("bool");
    case TypeKind::Char:      return str("char");
    case TypeKind::Char16:    return str("char16");
    case TypeKind::Null:      return str("null");
    case TypeKind::ISize:     return str("isize");
    case TypeKind::USize:     return str("usize");
    case TypeKind::Float32:   return str("float32");
    case TypeKind::Float64:   return str("float64");
    case TypeKind::EmptyArray: return str("[]");
    case TypeKind::Expr:      return str("expr type");
    case TypeKind::Auto: {
        if (is_const()) {
            return "const";
        } else {
            return "auto";
        }
    }
    case TypeKind::Range:     return str2(static_cast<const RangeType*>(this)->to_string());
    case TypeKind::Pointer:   return str2(static_cast<const PointerType*>(this)->to_string());
    case TypeKind::Array:     return str2(static_cast<const ArrayType*>(this)->to_string());
    case TypeKind::Slice: return str2(static_cast<const SliceType*>(this)->to_string());
    case TypeKind::Function:  return str2(static_cast<const FunctionType*>(this)->to_string());
    case TypeKind::Struct:    return str(static_cast<const StructType*>(this)->to_string());
    case TypeKind::Enum:      return str(static_cast<const EnumType*>(this)->to_string());
    case TypeKind::AssignDeterminedArray:
                              return str2(static_cast<const AssignDeterminedArrayType*>(this)->to_string());
    case TypeKind::Interface: return str(static_cast<const InterfaceType*>(this)->to_string());
    default:
        acorn_fatal_fmt("Type::to_string() missing to_string case. Kind=%s", static_cast<int>(kind));
        return "";
    }
#undef str
#undef str2
}

acorn::Type* acorn::ContainerType::get_base_type() const {
    Type* type_itr = elm_type;
    TypeKind our_kind = get_kind();
    while (type_itr->get_kind() == our_kind) {
        auto ctr_type = static_cast<ContainerType*>(type_itr);
        type_itr = ctr_type->elm_type;
    }
    return type_itr;
}

size_t acorn::ContainerType::get_depth() const {
    size_t depth = 1;

    Type* type_itr = elm_type;
    TypeKind our_kind = get_kind();
    while (type_itr->get_kind() == our_kind) {
        auto ctr_type = static_cast<ContainerType*>(type_itr);
        type_itr = ctr_type->elm_type;

        ++depth;
    }

    return depth;
}

acorn::PointerType* acorn::PointerType::create(PageAllocator& allocator, Type* elm_type, bool is_const) {
    PointerType* ptr_type = allocator.alloc_type<PointerType>();
    ptr_type->contains_const = is_const;
    return new (ptr_type) PointerType(is_const, elm_type);
}

std::string acorn::PointerType::to_string() const {
    return elm_type->to_string() + "*";
}

acorn::Type* acorn::UnresolvedBracketType::create(PageAllocator& allocator,
                                                Type* elm_type,
                                                Expr* expr,
                                                bool is_const) {
    UnresolvedBracketType* unresolved_type = allocator.alloc_type<UnresolvedBracketType>();
    new (unresolved_type) UnresolvedBracketType(is_const, expr, elm_type);
    unresolved_type->contains_const = is_const;
    return unresolved_type;
}

acorn::ArrayType* acorn::ArrayType::create(PageAllocator& allocator, Type* elm_type,
                                           uint32_t length, bool is_const) {
    ArrayType* arr_type = allocator.alloc_type<ArrayType>();
    new (arr_type) ArrayType(is_const, elm_type, length);
    arr_type->contains_const = is_const;
    return arr_type;
}

uint64_t acorn::ArrayType::get_total_linear_length() const {
    uint64_t length = this->length;
    Type* type_itr = elm_type;
    while (type_itr->is_array()) {
        auto arr_type = static_cast<ArrayType*>(type_itr);
        length *= arr_type->length;
        type_itr = arr_type->elm_type;
    }
    return length;
}

namespace acorn {
    static std::string array_like_type_to_string(const ContainerType* container_type) {
        std::string s = "";

        Type* base_type = nullptr;
        {
            auto elm_type = container_type->get_elm_type();
            Type* type_itr = elm_type;
            while (type_itr->get_kind() == TypeKind::Array ||
                   type_itr->get_kind() == TypeKind::AssignDeterminedArray) {
                auto ctr_type = static_cast<const ContainerType*>(type_itr);
                type_itr = ctr_type->get_elm_type();
            }
            base_type = type_itr;
        }

        s += base_type->to_string();

        while (true) {
            if (container_type->is_array()) {
                auto arr_type = static_cast<const ArrayType*>(container_type);
                s += "[" + std::to_string(arr_type->get_length()) + "]";
            } else {
                auto assign_det_arr = static_cast<const AssignDeterminedArrayType*>(container_type);
                s += "[]";
            }

            auto elm_type = container_type->get_elm_type();
            if (elm_type->is_array() || elm_type->get_kind() == TypeKind::AssignDeterminedArray) {
                container_type = static_cast<const ContainerType*>(elm_type);
            } else {
                break;
            }
        }
        return s;
    }
}

std::string acorn::ArrayType::to_string() const {
    return array_like_type_to_string(this);
}

acorn::SliceType* acorn::SliceType::create(PageAllocator& allocator, Type* elm_type, bool is_const) {
    auto slice_type = allocator.alloc_type<SliceType>();
    new (slice_type) SliceType(is_const, elm_type);
    slice_type->contains_const = is_const;
    return slice_type;
}

std::string acorn::SliceType::to_string() const {
    return elm_type->to_string() + "[..]";
}

acorn::Type* acorn::AssignDeterminedArrayType::create(PageAllocator& allocator,
                                                      Type* elm_type,
                                                      bool is_const) {
    auto arr_type = allocator.alloc_type<AssignDeterminedArrayType>();
    new (arr_type) AssignDeterminedArrayType(is_const, elm_type);
    arr_type->contains_const = is_const;
    return arr_type;
}

std::string acorn::AssignDeterminedArrayType::to_string() const {
    return array_like_type_to_string(this);
}

acorn::RangeType* acorn::RangeType::create(PageAllocator& allocator,
                                           Type* value_type,
                                           bool is_const) {
    auto range_type = allocator.alloc_type<RangeType>();
    new (range_type) RangeType(is_const, value_type);
    range_type->contains_const = is_const;
    return range_type;
}

std::string acorn::RangeType::to_string() const {
    return "range";
}

acorn::FunctionType* acorn::FunctionType::create(PageAllocator& allocator,
                                                 FunctionTypeKey* key,
                                                 bool is_const) {
    auto function_type = allocator.alloc_type<FunctionType>();
    new (function_type) FunctionType(is_const, key);
    function_type->contains_const = is_const;
    return function_type;
}

std::string acorn::FunctionType::to_string() const {
    std::string str = key->return_type->to_string() + "$";
    str += "(";
    size_t count = 0;
    for (Type* type : key->param_types) {
        str += type->to_string();
        if (count != key->param_types.size() - 1) {
            str += ", ";
        }
        ++count;
    }
    if (key->uses_native_varargs) {
        if (count != 0) {
            str += ", ";
        }
        str += "...";
    }
    str += ")";
    if (!key->raised_errors.empty()) {
        str += "(";
        str += "raises ";
        for (size_t i = 0; i < key->raised_errors.size(); i++) {
            str += key->raised_errors[i].name.to_string();
            if (i + 1 != key->raised_errors.size()) {
                str += ", ";
            }
        }
        str += ")";
    }
    return str;
}

acorn::Type* acorn::UnresolvedCompositeType::create(PageAllocator& allocator,
                                                    Identifier name,
                                                    SourceLoc name_location,
                                                    bool is_const) {
    auto composite_type = allocator.alloc_type<UnresolvedCompositeType>();
    new (composite_type) UnresolvedCompositeType(is_const, name, name_location);
    composite_type->contains_const = is_const;
    return composite_type;
}

acorn::StructType* acorn::StructType::create(PageAllocator& allocator,
                                             Struct* structn,
                                             bool is_const) {
    auto struct_type = allocator.alloc_type<StructType>();
    new (struct_type) StructType(is_const, structn);
    struct_type->contains_const = is_const;
    if (!is_const) {
        struct_type->non_const_version = struct_type;
    }
    return struct_type;
}

std::string acorn::StructType::to_string() const {
    return structn->name.to_string().str();
}

acorn::EnumType* acorn::EnumType::create(PageAllocator& allocator,
                                         Enum* enumn,
                                         bool is_const) {
    auto enum_type = allocator.alloc_type<EnumType>();
    new(enum_type) EnumType(is_const, enumn);
    enum_type->contains_const = is_const;
    if (!is_const) {
        enum_type->non_const_version = enum_type;
    }
    return enum_type;
}

std::string acorn::EnumType::to_string() const {
    return enumn->name.to_string().str();
}

acorn::InterfaceType* acorn::InterfaceType::create(PageAllocator& allocator,
                                                   Interface* interfacen,
                                                   bool is_const) {
    auto intr_type = allocator.alloc_type<InterfaceType>();
    new (intr_type) InterfaceType(is_const, interfacen);
    intr_type->contains_const = is_const;
    if (!is_const) {
        intr_type->non_const_version = intr_type;
    }
    return intr_type;
}

std::string acorn::InterfaceType::to_string() const {
    return interfacen->name.to_string().str();
}