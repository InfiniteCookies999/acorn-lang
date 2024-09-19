#ifndef GEN_TYPES_H
#define GEN_TYPES_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "../Type.h"

namespace acorn {
    
    std::string  to_string(llvm::Type* type);

    llvm::Type* gen_type(Type* type, llvm::LLVMContext& ll_context, llvm::Module& ll_module);

    llvm::Type* gen_ptrsize_int_type(llvm::LLVMContext& ll_context, llvm::Module& ll_module);

}

#endif // GEN_TYPES_H