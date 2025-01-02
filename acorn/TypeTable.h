#ifndef TYPE_TABLE_H
#define TYPE_TABLE_H

#include <llvm/ADT/DenseMap.h>
#include <mutex>

#include "Type.h"

namespace acorn {

    class PageAllocator;
    class Context;

    class TypeTable {
    public:

        TypeTable(PageAllocator& allocator, Context& context);

        Type* get_const_type(Type* type);

        Type* get_enum_container_type(EnumType* enum_type);

        Type* get_ptr_type(Type* elm_type);

        Type* get_arr_type(Type* elm_type, uint32_t length);

        Type* get_assigned_det_arr_type(Type* elm_type);

        Type* get_range_type(Type* value_type);

        Type* get_function_type(Type* return_type, llvm::SmallVector<Type*> param_types);

    private:
        PageAllocator& allocator;
        Context&       context;

        std::mutex const_types_mtx;
        llvm::DenseMap<Type*, Type*> const_types;
        
        std::mutex enum_container_types_mtx;
        llvm::DenseMap<Type*, Type*> enum_container_types;

        std::mutex ptr_types_mtx;
        llvm::DenseMap<Type*, Type*> ptr_types;
        
        std::mutex arr_types_mtx;
        llvm::DenseMap<std::pair<Type*, uint32_t>, Type*> arr_types;

        std::mutex func_types_mtx;
        llvm::DenseMap<FunctionTypeKey*, Type*> func_types;

        Type* create_type_from_type(Type* type, bool is_const);

    };
}

#endif // TYPE_TABLE_H