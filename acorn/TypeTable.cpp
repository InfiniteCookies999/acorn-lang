#include "TypeTable.h"

#include "PageAllocator.h"
#include "Context.h"
#include "Logger.h"

acorn::TypeTable::TypeTable(PageAllocator& allocator, Context& context)
    : allocator(allocator), context(context) {
}

acorn::Type* acorn::TypeTable::get_const_type(Type* type) {
    if (type->is_const()) {
        return type;
    }

    std::lock_guard lock(const_types_mtx);

    auto itr = const_types.find(type);
    if (itr != const_types.end()) {
        return itr->second;
    }

    Type* const_type = create_type_from_type(type, true);

    const_types.insert({ type, const_type });
    inv_const_types.insert({ const_type, type });
    const_type->non_const_version = type->does_contain_const() ? type->non_const_version : type;
    const_type->container_enum_type = type->container_enum_type;

    return const_type;
}

acorn::Type* acorn::TypeTable::remove_const(Type* type) {
    std::lock_guard lock(const_types_mtx);
    return inv_const_types[type];
}

acorn::Type* acorn::TypeTable::get_enum_container_type(EnumType* enum_type) {
    std::lock_guard lock(enum_container_types_mtx);

    auto itr = enum_container_types.find(enum_type);
    if (itr != enum_container_types.end()) {
        return itr->second;
    }

    Type* container_type = create_type_from_type(enum_type->get_values_type(), false);
    container_type->non_const_version = container_type;
    container_type->container_enum_type = enum_type;

    enum_container_types.insert({ enum_type, container_type });

    return container_type;
}

acorn::Type* acorn::TypeTable::create_type_from_type(Type* type, bool is_const) {

    Type* new_type;
    switch (type->get_kind()) {
    case TypeKind::Pointer: {
        auto ptr_type = static_cast<PointerType*>(type);
        new_type = PointerType::create(allocator, ptr_type->get_elm_type(), is_const);
        break;
    }
    case TypeKind::Struct: {
        auto struct_type = static_cast<StructType*>(type);
        auto new_struct_type = StructType::create(allocator, struct_type->get_struct(), is_const);
        new_struct_type->set_ll_struct_type(struct_type->get_ll_struct_type());
        new_type = new_struct_type;
        break;
    }
    case TypeKind::Enum: {
        auto enum_type = static_cast<EnumType*>(type);
        auto new_enum_type = EnumType::create(allocator, enum_type->get_enum(), is_const);
        new_enum_type->set_default_index(enum_type->get_default_index());
        new_enum_type->set_values_type(enum_type->get_values_type());
        new_enum_type->set_index_type(enum_type->get_index_type());
        new_type = new_enum_type;
        break;
    }
    case TypeKind::Interface: {
        auto intr_type = static_cast<InterfaceType*>(type);
        auto new_intr_type = InterfaceType::create(allocator, intr_type->get_interface(), is_const);
        new_type = new_intr_type;
        break;
    }
    case TypeKind::Array: {
        auto arr_type = static_cast<ArrayType*>(type);
        new_type = ArrayType::create(allocator, arr_type->get_elm_type(), arr_type->get_length(), is_const);
        break;
    }
    case TypeKind::Slice: {
        auto slice_type = static_cast<SliceType*>(type);
        auto new_slice_type = SliceType::create(allocator, slice_type->get_elm_type(), is_const);
        new_slice_type->set_ll_struct_type(slice_type->get_ll_struct_type());
        new_type = new_slice_type;
        break;
    }
    case TypeKind::Function: {
        auto func_type = static_cast<FunctionType*>(type);
        new_type = FunctionType::create(allocator, func_type->get_key(), is_const);
        break;
    }
    case TypeKind::Range: {
        auto range_type = static_cast<RangeType*>(type);
        new_type = RangeType::create(allocator, range_type->get_value_type());
        break;
    }
    case TypeKind::UnresolvedArray: {
        auto un_arr_type = static_cast<UnresolvedArrayType*>(type);
        new_type = UnresolvedArrayType::create(allocator, un_arr_type->get_elm_type(), un_arr_type->get_expr(), is_const);
        break;
    }
    case TypeKind::UnresolvedComposite: {
        auto un_composite_type = static_cast<UnresolvedCompositeType*>(type);
        new_type = UnresolvedCompositeType::create(allocator,
                                                   un_composite_type->get_composite_name(),
                                                   un_composite_type->get_error_location(),
                                                   is_const);
        break;
    }
    case TypeKind::UnresolvedGenericComposite: {
        auto un_composite_type = static_cast<UnresolvedGenericCompositeType*>(type);
        new_type = UnresolvedGenericCompositeType::create(allocator,
                                                          un_composite_type->get_composite_name(),
                                                          un_composite_type->get_error_location(),
                                                          un_composite_type->get_bound_exprs(),
                                                          un_composite_type->get_non_named_generic_args_offsets(),
                                                          is_const);
        break;
    }

    case TypeKind::UnresolvedEnumValueType: {
        auto un_enum_value_type = static_cast<UnresolvedEnumValueType*>(type);
        new_type = UnresolvedEnumValueType::create(allocator,
                                                   un_enum_value_type->get_enum_name(),
                                                   un_enum_value_type->get_error_location(),
                                                   is_const);
        break;
    }
    case TypeKind::AssignDeterminedArray: {
        auto assign_det_arr_type = static_cast<AssignDeterminedArrayType*>(type);
        new_type = AssignDeterminedArrayType::create(allocator, assign_det_arr_type->get_elm_type(), is_const);
        break;
    }
    case TypeKind::Generic: {
        auto generic_type = static_cast<GenericType*>(type);
        new_type = GenericType::create(allocator, generic_type->get_generic(), is_const);
        break;
    }
    default:
        new_type = Type::create(allocator, type->get_kind(), is_const);
        break;
    }

    return new_type;
}

acorn::PointerType* acorn::TypeTable::get_ptr_type(Type* elm_type) {
    std::lock_guard lock(ptr_types_mtx);

    auto itr = ptr_types.find(elm_type);
    if (itr != ptr_types.end()) {
        return itr->second;
    }

    auto ptr_type = PointerType::create(allocator, elm_type);
    ptr_types.insert({ elm_type, ptr_type });

    // Examples for while this process works.
    // Let get_const(type) represent requesting the const
    // version and get_ptr(type) be this function.
    //
    // Say we want const int* then the the following steps
    // are taken:
    //
    // 1. int  (initialized at startup)
    // 2. const int = get_const(int)
    //    2.a  (const int).non_const_version => int
    // 3. const int* = get_ptr(const int)
    //    3.a (const int*).non_const_version       => get_ptr(elm_type.non_const_version)
    //                                             => getPtr((const int).non_const_version)
    //                                       (2.a) => get_ptr(int)
    //                                             => int*
    //
    // Say we want const int* const* (assuming c++ syntax):
    //
    // 4. const int* const = get_const(const int*)
    //    4.a (const int* const).non_const_version       => type.non_const_version
    //                                                   => (const int*).non_const_version
    //                                             (3.a) => int*
    // 5. const int* const* = get_ptr(const int* const)
    //    5.a (const int* const*).non_const_version       => get_ptr(elm_type.non_const_version)
    //                                                    => get_ptr((const int* const).non_const_version)
    //                                              (4.a) => get_ptr(int*)
    //                                                    => int**

    if (elm_type->does_contain_const()) {
        ptr_type->contains_const = true;
        Type* non_const_elm_type = elm_type->non_const_version;

        auto itr1 = ptr_types.find(non_const_elm_type);
        if (itr1 != ptr_types.end()) {
            ptr_type->non_const_version = itr1->second;
        } else {
            auto new_non_const_ptr_type = PointerType::create(allocator, non_const_elm_type);
            new_non_const_ptr_type->non_const_version = new_non_const_ptr_type;
            ptr_types.insert({ non_const_elm_type, new_non_const_ptr_type });
            ptr_type->non_const_version = new_non_const_ptr_type;
        }
    } else {
        ptr_type->non_const_version = ptr_type;
    }

    return ptr_type;
}

acorn::ArrayType* acorn::TypeTable::get_arr_type(Type* elm_type, uint32_t length) {
    std::lock_guard lock(arr_types_mtx);

    auto itr = arr_types.find({ elm_type, length });
    if (itr != arr_types.end()) {
        return itr->second;
    }

    auto arr_type = ArrayType::create(allocator, elm_type, length);
    arr_types.insert({ { elm_type, length }, arr_type });

    // Read comment under get_ptr_type for explaination as to what is happening here.
    if (elm_type->does_contain_const()) {
        arr_type->contains_const = true;
        Type* non_const_elm_type = elm_type->non_const_version;

        auto itr1 = arr_types.find({ non_const_elm_type, length });
        if (itr1 != arr_types.end()) {
            arr_type->non_const_version = itr1->second;
        } else {
            auto new_non_const_ptr_type = ArrayType::create(allocator, non_const_elm_type, length);
            arr_types.insert({ { non_const_elm_type, length }, new_non_const_ptr_type });
            arr_type->non_const_version = new_non_const_ptr_type;
        }
    } else {
        arr_type->non_const_version = arr_type;
    }

    return arr_type;
}

acorn::SliceType* acorn::TypeTable::get_slice_type(Type* elm_type) {
    std::lock_guard lock(slice_types_mtx);

    auto itr = slice_types.find(elm_type);
    if (itr != slice_types.end()) {
        return itr->second;
    }

    auto slice_type = SliceType::create(allocator, elm_type);
    slice_types.insert({ elm_type, slice_type });

    if (elm_type->does_contain_const()) {
        slice_type->contains_const = true;
        auto non_const_elm_type = elm_type->non_const_version;

        auto itr1 = slice_types.find(non_const_elm_type);
        if (itr1 != slice_types.end()) {
            slice_type->non_const_version = itr1->second;
        } else {
            auto new_non_const_slice_type = SliceType::create(allocator, non_const_elm_type);
            new_non_const_slice_type->non_const_version = new_non_const_slice_type;
            slice_types.insert({ non_const_elm_type, new_non_const_slice_type });
            slice_type->non_const_version = new_non_const_slice_type;
        }
    } else {
        slice_type->non_const_version = slice_type;
    }

    return slice_type;
}

acorn::Type* acorn::TypeTable::get_assigned_det_arr_type(Type* elm_type) {
    return AssignDeterminedArrayType::create(allocator, elm_type);
}

acorn::RangeType* acorn::TypeTable::get_range_type(Type* value_type) {
    std::lock_guard lock(range_types_mtx);

    // Ranges do not care about constness of the type.
    value_type = value_type->remove_all_const();

    auto itr = range_types.find(value_type);
    if (itr != range_types.end()) {
        return itr->second;
    }

    auto range_type = RangeType::create(allocator, value_type);
    range_types.insert({ value_type, range_type });

    return range_type;
}

acorn::FunctionType* acorn::TypeTable::get_function_type(Type* return_type,
                                                         llvm::SmallVector<Type*> param_types,
                                                         llvm::SmallVector<RaisedError> raised_errors,
                                                         bool uses_native_varargs) {
    std::lock_guard lock(func_types_mtx);

    FunctionTypeKey cmp_key = FunctionTypeKey(return_type, std::move(param_types), std::move(raised_errors), uses_native_varargs);

    auto itr = func_types.find(&cmp_key);
    if (itr != func_types.end()) {
        return itr->second;
    }

    auto new_key = allocator.alloc_type<FunctionTypeKey>();
    new (new_key) FunctionTypeKey(return_type, std::move(cmp_key.param_types), std::move(cmp_key.raised_errors), uses_native_varargs);
    auto func_type = FunctionType::create(allocator, new_key);
    func_type->non_const_version = func_type;
    func_types.insert({ new_key, func_type });

    return func_type;
}
