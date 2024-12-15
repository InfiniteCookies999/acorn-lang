#include "GenTypes.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Module.h>

#include "../Logger.h"
#include "../AST.h"
#include "Namespace.h"

std::string acorn::to_string(llvm::Type* type) {
    std::string ll_str;
    llvm::raw_string_ostream rso(ll_str);
    type->print(rso);
    return rso.str();
}

llvm::Type* acorn::gen_type(Type* type, llvm::LLVMContext& ll_context, llvm::Module& ll_module) {
    switch (type->get_kind()) {
    case TypeKind::Pointer:
    case TypeKind::Function:
        return llvm::PointerType::get(ll_context, 0);

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
    case TypeKind::Float32:
        return llvm::Type::getFloatTy(ll_context);
    case TypeKind::Float64:
        return llvm::Type::getDoubleTy(ll_context);
    case TypeKind::Array: {
        auto arr_type = static_cast<ArrayType*>(type);
        auto ll_elm_type = gen_type(arr_type->get_elm_type(), ll_context, ll_module);
        return llvm::ArrayType::get(ll_elm_type, arr_type->get_length());
    }
    case TypeKind::Struct: {
        auto struct_type = static_cast<StructType*>(type);
        return gen_struct_type(struct_type, ll_context, ll_module);
    }
    default:
        acorn_fatal("gen_type: Unknown type");
        return nullptr;
    }
}

llvm::StructType* acorn::gen_struct_type(StructType* struct_type, llvm::LLVMContext& ll_context, llvm::Module& ll_module) {
    auto ll_struct_type = struct_type->get_ll_struct_type();
    if (ll_struct_type) {
        return ll_struct_type;
    }
        
    auto structn = struct_type->get_struct();

    ll_struct_type = llvm::StructType::create(ll_context);

    llvm::SmallVector<llvm::Type*> ll_field_types;

    for (Var* field : structn->fields) {
        auto ll_field_type = gen_type(field->type, ll_context, ll_module);
        ll_field_types.push_back(ll_field_type);
    }

    if (ll_field_types.empty()) {
        ll_field_types.push_back(llvm::Type::getInt8Ty(ll_context));
    }

    ll_struct_type->setBody(ll_field_types);
    ll_struct_type->setName(structn->name.reduce());

    struct_type->set_ll_struct_type(ll_struct_type);
    return ll_struct_type;
}

llvm::Type* acorn::gen_ptrsize_int_type(llvm::LLVMContext& ll_context, llvm::Module& ll_module) {
    return llvm::Type::getIntNTy(ll_context, ll_module.getDataLayout().getPointerSizeInBits());
}
