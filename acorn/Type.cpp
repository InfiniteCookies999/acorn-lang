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
            // a: const int**;
            // b: int* = as(int*) a;
            //   ^
            //   `b` points to the address of the other pointer
            //       but the other pointer isn't specified as having
            //       a constant address, so it is fine.
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
    case TypeKind::Float:
    case TypeKind::Double:
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
    case TypeKind::Float:
    case TypeKind::Double:
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
    case TypeKind::Float:   return 32;
    case TypeKind::Double:  return 64;
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
        return s + "$";
    }

#define str(s) !is_const() ? (s) : std::string("const ") + (s);
#define str2(s) !is_const() ? (s) : std::string("const (") + s + ")";
    switch (kind) {
    case TypeKind::Void:         return str("void");
    case TypeKind::Int:          return str("int");
    case TypeKind::Int8:         return str("int8");
    case TypeKind::Int16:        return str("int16");
    case TypeKind::Int32:        return str("int32");
    case TypeKind::Int64:        return str("int64");
    case TypeKind::UInt8:        return str("uint8");
    case TypeKind::UInt16:       return str("uint16");
    case TypeKind::UInt32:       return str("uint32");
    case TypeKind::UInt64:       return str("uint64");
    case TypeKind::Invalid:      return str("invalid");
    case TypeKind::FuncsRef:     return str("function reference");
    case TypeKind::NamespaceRef: return str("module reference");
    case TypeKind::Bool:         return str("bool");
    case TypeKind::Char:         return str("char");
    case TypeKind::Char16:       return str("char16");
    case TypeKind::Null:         return str("null");
    case TypeKind::ISize:        return str("isize");
    case TypeKind::USize:        return str("usize");
    case TypeKind::Float:        return str("float");
    case TypeKind::Double:       return str("double");
    case TypeKind::EmptyArray:   return str("[]");
    case TypeKind::Expr:         return str("expr type");
    case TypeKind::Auto:         return str("auto");
    case TypeKind::Inderminate:  return str("indeterminate");
    case TypeKind::Range:        return str2(static_cast<const RangeType*>(this)->to_string());
    case TypeKind::Pointer:      return str2(static_cast<const PointerType*>(this)->to_string());
    case TypeKind::Array:        return str2(static_cast<const ArrayType*>(this)->to_string());
    case TypeKind::Slice:        return str2(static_cast<const SliceType*>(this)->to_string());
    case TypeKind::Function:     return str2(static_cast<const FunctionType*>(this)->to_string());
    case TypeKind::Struct:       return str(static_cast<const StructType*>(this)->to_string());
    case TypeKind::Enum:         return str(static_cast<const EnumType*>(this)->to_string());
    case TypeKind::Interface:    return str(static_cast<const InterfaceType*>(this)->to_string());
    case TypeKind::AssignDeterminedArray:
                                 return str2(static_cast<const AssignDeterminedArrayType*>(this)->to_string());
    case TypeKind::Generic:      return str(static_cast<const GenericType*>(this)->to_string());
    default:
        acorn_fatal_fmt("Type::to_string() missing to_string case. Kind=%s", static_cast<int>(kind));
        return "";
    }
#undef str
#undef str2
}

void acorn::Type::get_generic_types(llvm::SmallVector<const GenericType*>& generics) const {
    // TODO (maddie): once composites can contain generics it will need to handle
    // that as well.
    switch (kind) {
    case TypeKind::Generic: {
        auto generic_type = static_cast<const GenericType*>(this);
        generics.push_back(generic_type);
        return;
    }
    case TypeKind::Pointer:
    case TypeKind::Array:
    case TypeKind::Slice: {
        auto ptr_type = static_cast<const ContainerType*>(this);
        auto elm_type = ptr_type->get_base_type();
        elm_type->get_generic_types(generics);
        return;
    }
    case TypeKind::Function: {
        auto func_type = static_cast<const FunctionType*>(this);
        for (auto param_type : func_type->get_key()->param_types) {
            param_type->get_generic_types(generics);
        }
        func_type->get_key()->return_type->get_generic_types(generics);
        return;
    }
    case TypeKind::Range: {
        auto range_type = static_cast<const RangeType*>(this);
        auto value_type = range_type->get_value_type();
        value_type->get_generic_types(generics);
        return;
    }
    default:
        return;
    }
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
    new (ptr_type) PointerType(is_const, elm_type);
    ptr_type->contains_const = is_const;
    ptr_type->contains_generics = elm_type->does_contain_generics();
    return ptr_type;
}

std::string acorn::PointerType::to_string() const {
    return elm_type->to_string() + "*";
}

acorn::Type* acorn::UnresolvedArrayType::create(PageAllocator& allocator,
                                                Type* elm_type,
                                                Expr* expr,
                                                bool is_const) {
    UnresolvedArrayType* unresolved_type = allocator.alloc_type<UnresolvedArrayType>();
    new (unresolved_type) UnresolvedArrayType(is_const, expr, elm_type);
    unresolved_type->contains_const = is_const;
    unresolved_type->contains_generics = elm_type->does_contain_generics();
    return unresolved_type;
}

acorn::ArrayType* acorn::ArrayType::create(PageAllocator& allocator,
                                           Type* elm_type,
                                           uint32_t length,
                                           bool is_const) {
    ArrayType* arr_type = allocator.alloc_type<ArrayType>();
    new (arr_type) ArrayType(is_const, elm_type, length);
    arr_type->contains_const = is_const;
    arr_type->contains_generics = elm_type->does_contain_generics();
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
    slice_type->contains_generics = elm_type->does_contain_generics();
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
    arr_type->contains_generics = elm_type->does_contain_generics();
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
    range_type->contains_generics = value_type->does_contain_generics();
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
    // Check for if the type contains generics.
    for (Type* param_type : key->param_types) {
        if (param_type->does_contain_generics()) {
            function_type->contains_generics = true;
            break;
        }
    }
    function_type->contains_generics |= key->return_type->does_contain_generics();
    return function_type;
}

std::string acorn::FunctionType::to_string() const {
    std::string str = "fn(";
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
    if (key->return_type->get_kind() != TypeKind::Void) {
        str += " -> " + key->return_type->to_string();
    }

    if (!key->raised_errors.empty()) {
        str += " | ";
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
                                                    SourceLoc error_location,
                                                    bool is_const) {
    auto composite_type = allocator.alloc_type<UnresolvedCompositeType>();
    new (composite_type) UnresolvedCompositeType(is_const, name, error_location);
    composite_type->contains_const = is_const;
    return composite_type;
}

acorn::Type* acorn::UnresolvedGenericCompositeType::create(PageAllocator& allocator,
                                                           Identifier name,
                                                           SourceLoc  error_location,
                                                           llvm::SmallVector<Expr*> bound_exprs,
                                                           size_t non_named_generic_args_offsets,
                                                           bool is_const) {
    auto composite_type = allocator.alloc_type<UnresolvedGenericCompositeType>();
    new (composite_type) UnresolvedGenericCompositeType(is_const,
                                                        name,
                                                        error_location,
                                                        std::move(bound_exprs),
                                                        non_named_generic_args_offsets);
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
    std::string str = structn->name.to_string().str();
    if (structn->is_generic) {
        auto struct_instance = static_cast<GenericStructInstance*>(structn);
        str += "(";
        for (size_t i = 0; i < struct_instance->bound_types.size(); i++) {
            str += struct_instance->bound_types[i]->to_string();
            if (i + 1 != struct_instance->bound_types.size()) {
                str += ", ";
            }
        }
        str += ")";
    }
    return str;
}

acorn::PartiallyBoundStructType* acorn::PartiallyBoundStructType::create(PageAllocator& allocator,
                                                                         UnboundGenericStruct* unbound_generic_struct,
                                                                         llvm::SmallVector<Type*> partially_bound_types,
                                                                         bool is_const) {
    auto struct_type = allocator.alloc_type<PartiallyBoundStructType>();
    new (struct_type) PartiallyBoundStructType(is_const, unbound_generic_struct, std::move(partially_bound_types));
    struct_type->contains_const = is_const;
    if (!is_const) {
        struct_type->non_const_version = struct_type;
    }
    struct_type->contains_generics = true;
    return struct_type;
}

std::string acorn::PartiallyBoundStructType::to_string() const {
    std::string str = unbound_generic_struct->name.to_string().str();
    str += "(";
    for (size_t i = 0; i < partially_bound_types.size(); i++) {
        str += partially_bound_types[i]->to_string();
        if (i + 1 != partially_bound_types.size()) {
            str += ", ";
        }
    }
    str += ")";
    return str;
}

acorn::EnumType* acorn::EnumType::create(PageAllocator& allocator,
                                         Enum* enumn,
                                         bool is_const) {
    auto enum_type = allocator.alloc_type<EnumType>();
    new (enum_type) EnumType(is_const, enumn);
    enum_type->contains_const = is_const;
    if (!is_const) {
        enum_type->non_const_version = enum_type;
    }
    return enum_type;
}

std::string acorn::EnumType::to_string() const {
    return enumn->name.to_string().str();
}

acorn::Type* acorn::UnresolvedEnumValueType::create(PageAllocator& allocator,
                                                    Identifier enum_name,
                                                    SourceLoc error_location,
                                                    bool is_const) {
    auto un_type = allocator.alloc_type<UnresolvedEnumValueType>();
    new(un_type) UnresolvedEnumValueType(is_const, enum_name, error_location);
    un_type->contains_const = is_const;
    if (!is_const) {
        un_type->non_const_version = un_type;
    }
    return un_type;
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

acorn::GenericType* acorn::GenericType::create(PageAllocator& allocator, Generic* generic, bool is_const) {
    auto generic_type = allocator.alloc_type<GenericType>();
    new (generic_type) GenericType(generic, is_const);
    generic_type->contains_const = is_const;
    if (!is_const) {
        generic_type->non_const_version = generic_type;
    }
    generic_type->contains_generics = true;
    return generic_type;
}

std::string acorn::GenericType::to_string() const {
    return generic->name.to_string().str();
}

size_t acorn::GenericType::get_generic_index() const {
    return generic->index;
}
