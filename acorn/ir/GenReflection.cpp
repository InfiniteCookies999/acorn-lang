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
        if (type->get_kind() != TypeKind::Void) {
            auto ll_size_in_bytes = builder.getInt32(static_cast<uint32_t>(sizeof_type_in_bytes(ll_type_info_type)));
            ll_values.push_back(ll_size_in_bytes);
        } else {
            ll_values.push_back(builder.getInt32(0));
        }
    }
    { // const Type* elm_type
        if (type->is_container()) {
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
    { // const EnumTypeInfo* enum_info
        if (type->is_enum()) {
            auto enum_type = static_cast<EnumType*>(type);
            ll_values.push_back(gen_reflect_type_info_enum_info(enum_type));
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
    { // name: const char*
        auto ll_string_name = llvm::Twine("global.str.") + llvm::Twine(context.global_counter++);
        auto ll_name = builder.CreateGlobalString(structn->name.to_string(), ll_string_name);
        ll_values.push_back(ll_name);
    }
    { // num_fields: int
        auto ll_num_fields = builder.getInt32(static_cast<uint32_t>(structn->fields.size()));
        ll_values.push_back(ll_num_fields);
    }
    { // fields: const Field*

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

    return gen_const_global_struct_variable("global.type.struct.info.", ll_struct_type, ll_values);
}

llvm::Constant* acorn::IRGenerator::gen_reflect_type_info_field_info(Var* field,
                                                                     uint64_t offset_in_bytes) {

    auto ll_struct_type = gen_struct_type(context.std_field_type_info_struct->struct_type);

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(3);
    { // name: const char*
        auto ll_string_name = llvm::Twine("global.str.") + llvm::Twine(context.global_counter++);
        auto ll_name = builder.CreateGlobalString(field->name.to_string(), ll_string_name);
        ll_values.push_back(ll_name);
    }
    { // type: const Type*
        ll_values.push_back(gen_reflect_type_info(field->type));
    }
    { // offset_in_bytes: int
        ll_values.push_back(builder.getInt32(static_cast<uint32_t>(offset_in_bytes)));
    }

    return llvm::ConstantStruct::get(ll_struct_type, ll_values);
}

llvm::Constant* acorn::IRGenerator::gen_reflect_type_info_enum_info(EnumType* enum_type) {

    auto enumn = enum_type->get_enum();

    auto ll_struct_type = gen_struct_type(context.std_enum_type_info_struct->struct_type);

    uint64_t num_values = static_cast<uint64_t>(enumn->values.size());
    uint64_t num_buckets = next_pow2(num_values * 2);
    uint64_t index_mapping_mask = num_buckets - 1;

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(5);
    { // num_values: int
        ll_values.push_back(builder.getInt32(static_cast<uint32_t>(enumn->values.size())));
    }
    { // index_mapping_mask: uint64
        ll_values.push_back(builder.getInt64(index_mapping_mask));
    }
    { // index_mappings: uint64[2]*

        llvm::SmallVector<llvm::Constant*> ll_buckets(num_buckets);

        auto ll_bucket_type = llvm::ArrayType::get(builder.getInt64Ty(), 2);

        // TODO: Could calculate highest cluster count and retry with a larger
        //       hash table if the count exceeds a certain value.
        uint64_t value_count = 0;
        for (const auto& value : enumn->values) {
             uint64_t hash = value.index & index_mapping_mask;
             uint64_t bucket_index = hash;

             while (true) {
                 if (!ll_buckets[bucket_index]) {

                     auto ll_bucket = llvm::ConstantArray::get(ll_bucket_type, {
                        builder.getInt64(value.index),
                        builder.getInt64(value_count)
                     });

                     ll_buckets[bucket_index] = ll_bucket;
                     break;
                 }

                 ++bucket_index;
                 bucket_index &= index_mapping_mask;
             }

             ++value_count;
        }

        for (size_t i = 0; i < ll_buckets.size(); i++) {
            if (!ll_buckets[i]) {
                auto ll_bucket = llvm::ConstantArray::get(ll_bucket_type, {
                    builder.getInt64(UINT64_MAX),
                    builder.getInt64(UINT64_MAX)
                });

                ll_buckets[i] = ll_bucket;
            }
        }

        auto ll_buckets_type = llvm::ArrayType::get(ll_bucket_type, num_buckets);
        auto ll_buckets_array = llvm::ConstantArray::get(ll_buckets_type, ll_buckets);

        auto ll_global_name = llvm::Twine("global.type.enum.info.mapping.") + llvm::Twine(context.global_counter++);
        auto ll_global_array = gen_global_variable(ll_global_name, ll_buckets_type, true, ll_buckets_array);

        ll_values.push_back(ll_global_array);
    }
    { // value_names: const char**

        llvm::SmallVector<llvm::Constant*> ll_value_names;
        ll_value_names.reserve(enumn->values.size());
        for (const auto& value : enumn->values) {
            auto ll_string_name = llvm::Twine("global.str.") + llvm::Twine(context.global_counter++);
            auto ll_name = builder.CreateGlobalString(value.name.to_string(), ll_string_name);
            ll_value_names.push_back(ll_name);
        }

        auto ll_names_array_type  = llvm::ArrayType::get(builder.getPtrTy(), enumn->values.size());
        auto ll_names_array_value = llvm::ConstantArray::get(ll_names_array_type, ll_value_names);

        auto ll_global_name = llvm::Twine("global.enum.info.value_names.") + llvm::Twine(context.global_counter++);
        auto ll_global_names_array = gen_global_variable(ll_global_name, ll_names_array_type, true, ll_names_array_value);

        ll_values.push_back(ll_global_names_array);
    }
    { // index_type: const Type*
        ll_values.push_back(gen_reflect_type_info(enum_type->get_index_type()));
    }

    return gen_const_global_struct_variable("global.type.enum.info.", ll_struct_type, ll_values);
}