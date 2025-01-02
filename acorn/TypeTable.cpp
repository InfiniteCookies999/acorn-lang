#include "TypeTable.h"

#include "PageAllocator.h"
#include "Context.h"
#include "Logger.h"

acorn::TypeTable::TypeTable(PageAllocator& allocator, Context& context)
    : allocator(allocator), context(context) {
}

acorn::Type* acorn::TypeTable::get_const_type(Type* type) {
    std::lock_guard lock(const_types_mtx);

    auto itr = const_types.find(type);
    if (itr != const_types.end()) {
        return itr->second;
    }

    Type* const_type = create_type_from_type(type, true);

    const_types.insert({ type, const_type });
    const_type->non_const_version = type->does_contain_const() ? type->non_const_version : type;
    const_type->container_enum_type = type->container_enum_type;

    return const_type;
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
        new_type = StructType::create(allocator, struct_type->get_struct(), is_const);
        break;
    }
    case TypeKind::Enum: {
        auto enum_type = static_cast<EnumType*>(type);
        new_type = EnumType::create(allocator, enum_type->get_enum(), is_const);
        break;
    }
    case TypeKind::Array: {
        auto arr_type = static_cast<ArrayType*>(type);
        new_type = ArrayType::create(allocator, arr_type->get_elm_type(), arr_type->get_length(), is_const);
        break;
    }
    case TypeKind::Function: {
        auto func_type = static_cast<FunctionType*>(type);
        new_type = FunctionType::create(allocator, func_type->get_key(), is_const);
        break;
    }
    default:
        new_type = Type::create(allocator, type->get_kind(), is_const);
        break;
    }

    return new_type;
}

acorn::Type* acorn::TypeTable::get_ptr_type(Type* elm_type) {
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

        auto itr = ptr_types.find(non_const_elm_type);
        if (itr != ptr_types.end()) {
            ptr_type->non_const_version = itr->second;
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

acorn::Type* acorn::TypeTable::get_arr_type(Type* elm_type, uint32_t length) {
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
        Type* non_const_elm_ptr_version = elm_type->non_const_version;

        auto itr = arr_types.find({ non_const_elm_ptr_version, length });
        if (itr != arr_types.end()) {
            arr_type->non_const_version = itr->second;
        } else {
            auto new_non_const_ptr_type = ArrayType::create(allocator, non_const_elm_ptr_version, length);
            arr_types.insert({ { elm_type, length }, new_non_const_ptr_type });
            arr_type->non_const_version = new_non_const_ptr_type;
        }
    } else {
        arr_type->non_const_version = arr_type;
    }

    return arr_type;
}

acorn::Type* acorn::TypeTable::get_assigned_det_arr_type(Type* elm_type) {
    return AssignDeterminedArrayType::create(allocator, elm_type);
}

acorn::Type* acorn::TypeTable::get_range_type(Type* value_type) {
    switch (value_type->get_kind()) {
    case TypeKind::Int: return context.int_range_type;
    case TypeKind::Int8: return context.int8_range_type;
    case TypeKind::Int16: return context.int16_range_type;
    case TypeKind::Int32: return context.int32_range_type;
    case TypeKind::Int64: return context.int64_range_type;
    case TypeKind::UInt8: return context.uint8_range_type;
    case TypeKind::UInt16: return context.uint16_range_type;
    case TypeKind::UInt32: return context.uint32_range_type;
    case TypeKind::UInt64: return context.uint64_range_type;
    case TypeKind::ISize: return context.isize_range_type;
    case TypeKind::USize: return context.usize_range_type;
    case TypeKind::Char: return context.char_range_type;
    case TypeKind::Char16: return context.char16_range_type;
    case TypeKind::Char32: return context.char32_range_type;
    default:
        acorn_fatal("Tried to create a range type from an invalid value type");
        return nullptr;
    }
}

acorn::Type* acorn::TypeTable::get_function_type(Type* return_type, llvm::SmallVector<Type*> param_types) {
    std::lock_guard lock(func_types_mtx);
    
    FunctionTypeKey cmp_key = FunctionTypeKey(return_type, param_types);

    auto itr = func_types.find(&cmp_key);
    if (itr != func_types.end()) {
        return itr->second;
    }
    
    auto new_key = allocator.alloc_type<FunctionTypeKey>();
    new (new_key) FunctionTypeKey(return_type, std::move(cmp_key.param_types));
    auto func_type = FunctionType::create(allocator, new_key);
    func_types.insert({ new_key, func_type });
    
    return func_type;
}