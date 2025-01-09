#include "IRGen.h"

#include "Logger.h"
#include "Context.h"

llvm::Value* acorn::IRGenerator::gen_reflect(Reflect* reflect) {
    switch (reflect->reflect_kind) {
    case ReflectKind::TypeInfo:
        return gen_reflect_type_info(reflect->type_info_type);
    default:
        acorn_fatal("Unknown reflect kind");
        return nullptr;
    }
}

llvm::GlobalVariable* acorn::IRGenerator::gen_reflect_type_info(Type* type) {

    auto itr = context.ll_type_info_global_addresses.find(type);
    if (itr != context.ll_type_info_global_addresses.end()) {
        return itr->second;
    }

    auto ll_struct_type = gen_struct_type(context.std_type_struct->struct_type);

    auto ll_global_name = llvm::Twine("global.type.info.") + llvm::Twine(context.global_counter++);
    auto ll_global = gen_global_variable(ll_global_name, ll_struct_type, true, nullptr);

    context.ll_type_info_global_addresses.insert({ type, ll_global });

    auto ll_type_info = gen_reflect_type_of_type_info(type);
    ll_global->setInitializer(ll_type_info);

    return ll_global;
}

llvm::Constant* acorn::IRGenerator::gen_reflect_type_of_type_info(Type* type) {

    auto ll_struct_type = gen_struct_type(context.std_type_struct->struct_type);
    auto ll_type_info_type = gen_type(type);

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(context.std_type_struct->struct_type->get_struct()->fields.size());
    { // TypeId id
        auto ll_type_id = builder.getInt32(static_cast<uint32_t>(type->get_kind()));
        ll_values.push_back(ll_type_id);
    }
    { // int size_in_bytes
        auto ll_size_in_bytes = builder.getInt32(static_cast<uint32_t>(sizeof_type_in_bytes(ll_type_info_type)));
        ll_values.push_back(ll_size_in_bytes);
    }
    { // const Type* element_type
        if (type->is_pointer() || type->is_array()) {
            auto ctr_type = static_cast<ContainerType*>(type);
            ll_values.push_back(gen_reflect_type_info(ctr_type->get_elm_type()));
        } else {
            ll_values.push_back(llvm::Constant::getNullValue(builder.getPtrTy()));
        }
    }
    { // int array_length
        if (type->is_array()) {
            auto arr_type = static_cast<ArrayType*>(type);
            ll_values.push_back(builder.getInt32(arr_type->get_length()));
        } else {
            ll_values.push_back(builder.getInt32(0));
        }
    }
    { // const StructTypeInfo* struct_info
        if (type->is_struct()) {
            auto struct_type = static_cast<StructType*>(type);
            ll_values.push_back(gen_reflect_type_info_struct_info(struct_type));
        } else {
            ll_values.push_back(llvm::Constant::getNullValue(builder.getPtrTy()));
        }
    }

    return llvm::ConstantStruct::get(ll_struct_type, ll_values);
}

llvm::Constant* acorn::IRGenerator::gen_reflect_type_info_struct_info(StructType* struct_type) {

    auto ll_struct_type = gen_struct_type(context.std_struct_type_info_struct->struct_type);
    auto structn = struct_type->get_struct();

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(2);
    { // const char* name
        auto ll_string_name = llvm::Twine("global.str.") + llvm::Twine(context.global_counter++);
        auto ll_name = builder.CreateGlobalString(structn->name.to_string(), ll_string_name);
        ll_values.push_back(ll_name);
    }
    { // int num_fields
        auto ll_num_fields = builder.getInt32(static_cast<uint32_t>(structn->fields.size()));
        ll_values.push_back(ll_num_fields);
    }
    { // const Field* fields

        auto ll_type_struct_type = gen_struct_type(structn->struct_type);
        auto ll_struct_layout = context.get_ll_module().getDataLayout().getStructLayout(ll_type_struct_type);

        llvm::SmallVector<llvm::Constant*> ll_fields;
        ll_fields.reserve(structn->fields.size());
        for (Var* field : structn->fields) {
            uint64_t offset_in_bytes = ll_struct_layout->getElementOffset(field->field_idx);
            ll_fields.push_back(gen_reflect_type_info_field_info(field, offset_in_bytes));
        }

        auto ll_elm_type = gen_struct_type(context.std_field_type_info_struct->struct_type);
        auto ll_array_type = llvm::ArrayType::get(ll_elm_type, ll_fields.size());
        auto ll_fields_array = llvm::ConstantArray::get(ll_array_type, ll_fields);

        auto ll_global_name = llvm::Twine("global.type.field.info.array.") + llvm::Twine(context.global_counter++);
        auto ll_global_fields_array = gen_global_variable(ll_global_name, ll_array_type, true, ll_fields_array);
        ll_values.push_back(ll_global_fields_array);
    }

    auto ll_global_name = llvm::Twine("global.type.struct.info.") + llvm::Twine(context.global_counter++);
    auto ll_global_data = llvm::ConstantStruct::get(ll_struct_type, ll_values);
    auto ll_global = gen_global_variable(ll_global_name, ll_struct_type, true, ll_global_data);

    return ll_global;
}

llvm::Constant* acorn::IRGenerator::gen_reflect_type_info_field_info(Var* field,
                                                                     uint64_t offset_in_bytes) {

    auto ll_struct_type = gen_struct_type(context.std_field_type_info_struct->struct_type);

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(3);
    { // const char* name
        auto ll_string_name = llvm::Twine("global.str.") + llvm::Twine(context.global_counter++);
        auto ll_name = builder.CreateGlobalString(field->name.to_string(), ll_string_name);
        ll_values.push_back(ll_name);
    }
    { // const Type* type
        ll_values.push_back(gen_reflect_type_info(field->type));
    }
    { // int offset_in_bytes
        ll_values.push_back(builder.getInt32(static_cast<uint32_t>(offset_in_bytes)));
    }

    return llvm::ConstantStruct::get(ll_struct_type, ll_values);
}