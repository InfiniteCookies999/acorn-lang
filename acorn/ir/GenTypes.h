#ifndef GEN_TYPES_H
#define GEN_TYPES_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "../Type.h"

namespace acorn {
    std::string  to_string(llvm::Type* type);

    llvm::Type* gen_type(Type* type, llvm::LLVMContext& ll_context);
}

#endif // GEN_TYPES_H