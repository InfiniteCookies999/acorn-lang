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

        Type* get_ptr_type(Type* elm_type);

        Type* get_arr_type(Type* elm_type, uint32_t length);

        Type* get_assigned_det_arr_type(Type* elm_type);

        Type* get_range_type(Type* value_type);

    private:
        PageAllocator& allocator;
        Context&       context;

        std::mutex const_types_mtx;
        llvm::DenseMap<Type*, Type*> const_types;
        
        std::mutex ptr_types_mtx;
        llvm::DenseMap<Type*, Type*> ptr_types;
        
        std::mutex arr_types_mtx;
        llvm::DenseMap<std::pair<Type*, uint32_t>, Type*> arr_types;
    };
}

#endif // TYPE_TABLE_H