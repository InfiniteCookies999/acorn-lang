#ifndef GENERIC_AST_H
#define GENERIC_AST_H

#include <llvm/ADT/SmallVector.h>

namespace llvm {
    class Function;
}

namespace acorn {

    class Type;
    class StructType;

    struct GenericInstance {
    };

    struct GenericFuncInstance : public GenericInstance {
        llvm::SmallVector<Type*> generic_bindings;
        // Parameters have their types fully qualified so that future
        // calls to the generic function know how to properly call the
        // function.
        //
        // TODO (maddie): do we really need this? IR gen seems to rely
        // very little on it outside of some stuff to do with variadics
        // and generating the function declaration.
        // ^^ it is also used to initialize the parameters atm.
        //
        // Index 0 is the return type.
        llvm::SmallVector<Type*> qualified_types;

        llvm::Function* ll_func = nullptr;

        StructType* struct_type;
    };

    struct GenericStructInstance : public GenericInstance {
        llvm::SmallVector<Type*> generic_bindings;
        // Qualified field types.
        llvm::SmallVector<Type*> qualified_types;

        llvm::Function* ll_default_constructor = nullptr;
        llvm::Function* ll_destructor          = nullptr;
        llvm::Function* ll_copy_constructor    = nullptr;
        llvm::Function* ll_move_constructor    = nullptr;
        llvm::Function* ll_init_vtable_func    = nullptr;

        bool is_being_checked        = false;
        bool has_been_checked        = false;
        bool has_errors              = false;
        bool needs_default_call      = false;
        bool needs_destruction       = false;
        bool needs_copy_call         = false;
        bool needs_move_call         = false;
        bool fields_need_destruction = false;
        bool fields_need_copy_call   = false;
        bool fields_need_move_call   = false;
        bool uses_vtable             = false;
        bool default_foldable        = true;

    };
}

#endif // GENERIC_AST_H
