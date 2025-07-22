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
    case TypeKind::POINTER:
    case TypeKind::FUNCTION:
        return llvm::PointerType::get(ll_context, 0);

    case TypeKind::VOID_T: return llvm::Type::getVoidTy(ll_context);

    case TypeKind::INT8: case TypeKind::UINT8: case TypeKind::CHAR:
        return llvm::Type::getInt8Ty(ll_context);
    case TypeKind::INT16: case TypeKind::UINT16: case TypeKind::CHAR16:
        return llvm::Type::getInt16Ty(ll_context);
    case TypeKind::INT: case TypeKind::INT32: case TypeKind::UINT32:
        return llvm::Type::getInt32Ty(ll_context);
    case TypeKind::INT64: case TypeKind::UINT64:
        return llvm::Type::getInt64Ty(ll_context);
    case TypeKind::USIZE: case TypeKind::ISIZE:
        return gen_ptrsize_int_type(ll_context, ll_module);
    case TypeKind::BOOL_T: return llvm::Type::getInt1Ty(ll_context);
    case TypeKind::FLOAT:
        return llvm::Type::getFloatTy(ll_context);
    case TypeKind::DOUBLE:
        return llvm::Type::getDoubleTy(ll_context);
    case TypeKind::ARRAY: {
        auto arr_type = static_cast<ArrayType*>(type);
        auto ll_elm_type = gen_type(arr_type->get_elm_type(), ll_context, ll_module);
        return llvm::ArrayType::get(ll_elm_type, arr_type->get_length());
    }
    case TypeKind::STRUCT: {
        auto struct_type = static_cast<StructType*>(type);
        return gen_struct_type(struct_type, ll_context, ll_module);
    }
    case TypeKind::ENUM: {
        auto enum_type = static_cast<EnumType*>(type);
        return gen_type(enum_type->get_index_type(), ll_context, ll_module);
    }
    case TypeKind::SLICE: {
        auto slice_type = static_cast<SliceType*>(type);
        if (auto ll_slice_type = slice_type->get_ll_struct_type()) {
            return ll_slice_type;
        }
        auto ll_struct_type = llvm::StructType::create(ll_context);
        slice_type->set_ll_struct_type(ll_struct_type);

        llvm::SmallVector<llvm::Type*> ll_field_types;

        ll_field_types.push_back(llvm::PointerType::get(ll_context, 0));
        ll_field_types.push_back(llvm::Type::getInt32Ty(ll_context));

        ll_struct_type->setBody(ll_field_types);
        ll_struct_type->setName("slice.type");

        return ll_struct_type;
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
    size_t ll_num_fields = structn->fields.size();

    if (structn->uses_vtable) {
        for (auto& extension : structn->interface_extensions) {
            if (extension.is_dynamic) {
                ++ll_num_fields;
            }
        }
    }
    ll_field_types.reserve(ll_num_fields);

    // Add pointer fields that will point to each vtable for each dynamic
    // interface implemented.
    //
    if (structn->uses_vtable) {
        for (auto& extension : structn->interface_extensions) {
            if (extension.is_dynamic) {
                ll_field_types.push_back(llvm::PointerType::get(ll_context, 0));
            }
        }
    }

    for (Var* field : structn->fields) {
        auto ll_field_type = gen_type(field->type, ll_context, ll_module);
        ll_field_types.push_back(ll_field_type);
    }

    if (ll_field_types.empty()) {
        ll_field_types.push_back(llvm::Type::getInt8Ty(ll_context));
    }

    ll_struct_type->setBody(ll_field_types);
    ll_struct_type->setName(structn->name.to_string());

    struct_type->set_ll_struct_type(ll_struct_type);
    return ll_struct_type;
}

llvm::Type* acorn::gen_ptrsize_int_type(llvm::LLVMContext& ll_context, llvm::Module& ll_module) {
    return llvm::Type::getIntNTy(ll_context, ll_module.getDataLayout().getPointerSizeInBits());
}
