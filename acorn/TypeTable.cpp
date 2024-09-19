#include "TypeTable.h"

#include "PageAllocator.h"

acorn::TypeTable::TypeTable(PageAllocator& allocator)
    : allocator(allocator) {
}

acorn::Type* acorn::TypeTable::get_const_type(Type* type) {
    std::scoped_lock<std::mutex> lock(const_types_mtx);

    auto itr = const_types.find(type);
    if (itr != const_types.end()) {
        return itr->second;
    }

    auto const_type = Type::create(allocator, type->get_kind(), true);
    const_types.insert({ type, const_type });
    const_type->non_const_version = type->does_contain_const() ? type->non_const_version : type;

    return const_type;
}

acorn::Type* acorn::TypeTable::get_ptr_type(Type* elm_type) {
    std::scoped_lock<std::mutex> lock(ptr_types_mtx);

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
        Type* non_const_elm_ptr_version = elm_type->non_const_version;

        auto itr = ptr_types.find(non_const_elm_ptr_version);
        if (itr != ptr_types.end()) {
            ptr_type->non_const_version = itr->second;
        } else {
            auto new_non_const_ptr_type = PointerType::create(allocator, non_const_elm_ptr_version);
            ptr_types.insert({ elm_type, new_non_const_ptr_type });
            ptr_type->non_const_version = new_non_const_ptr_type;
        }
    } else {
        ptr_type->non_const_version = ptr_type;
    }

    return ptr_type;
}

acorn::Type* acorn::TypeTable::get_arr_type(Type* elm_type, uint32_t length) {
    std::scoped_lock<std::mutex> lock(arr_types_mtx);
    
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
