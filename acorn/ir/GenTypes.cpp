#include "GenTypes.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Module.h>

#include "../Logger.h"

std::string acorn::to_string(llvm::Type* type) {
    std::string ll_str;
    llvm::raw_string_ostream rso(ll_str);
    type->print(rso);
    return rso.str();
}

llvm::Type* acorn::gen_type(Type* type, llvm::LLVMContext& ll_context, llvm::Module& ll_module) {
    switch (type->get_kind()) {
    case TypeKind::Pointer: return llvm::PointerType::get(ll_context, 0);

    case TypeKind::Void: return llvm::Type::getVoidTy(ll_context);

    case TypeKind::Int8: case TypeKind::UInt8: case TypeKind::Char:
        return llvm::Type::getInt8Ty(ll_context);
    case TypeKind::Int16: case TypeKind::UInt16: case TypeKind::Char16:
        return llvm::Type::getInt16Ty(ll_context);
    case TypeKind::Int: case TypeKind::Int32: case TypeKind::UInt32: case TypeKind::Char32:
        return llvm::Type::getInt32Ty(ll_context);
    case TypeKind::Int64: case TypeKind::UInt64:
        return llvm::Type::getInt64Ty(ll_context);
    case TypeKind::USize: case TypeKind::ISize:
        return gen_ptrsize_int_type(ll_context, ll_module);
    case TypeKind::Bool: return llvm::Type::getInt1Ty(ll_context);
    default:
        acorn_fatal("gen_type: Unknown type");
        return nullptr;
    }
}

llvm::Type* acorn::gen_ptrsize_int_type(llvm::LLVMContext& ll_context, llvm::Module& ll_module) {
    return llvm::Type::getIntNTy(ll_context, ll_module.getDataLayout().getPointerSizeInBits());
}
