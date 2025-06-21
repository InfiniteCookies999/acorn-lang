#include "IRGen.h"

llvm::Constant* acorn::IRGenerator::gen_constant_value(Expr* value) {
    if (value->type->is_struct()) {
        return gen_constant_struct_initializer(static_cast<StructInitializer*>(value));
    } else if (value->type->is_array()) {
        return gen_constant_array(static_cast<Array*>(value));
    } else {
        return llvm::cast<llvm::Constant>(gen_rvalue(value));
    }
}

llvm::Constant* acorn::IRGenerator::gen_constant_default_value(Type* type) {
    if (type->is_struct()) {
        auto struct_type = static_cast<StructType*>(type);
        return gen_constant_default_struct(struct_type);
    } else if (type->is_array()) {
        auto arr_type = static_cast<ArrayType*>(type);
        return gen_constant_default_array(arr_type);
    } else {
        return gen_zero(type);
    }
}

llvm::Constant* acorn::IRGenerator::gen_constant_default_struct(StructType* struct_type) {

    bool all_values_initialized = true;

    auto structn = struct_type->get_struct();

    llvm::SmallVector<llvm::Constant*, 16> ll_field_values;
    ll_field_values.reserve(structn->fields.size());

    for (Var* field : structn->fields) {
        if (field->assignment) {
            ll_field_values.push_back(gen_constant_value(field->assignment));
        } else {
            ll_field_values.push_back(gen_constant_default_value(field->type));
        }
    }

    // LLVM expects non empty struct so we need to give it the dummy struct value.
    if (ll_field_values.empty()) {
        ll_field_values.push_back(builder.getInt8(0));
    }

    auto ll_struct_type = gen_struct_type(struct_type);
    return llvm::ConstantStruct::get(ll_struct_type, ll_field_values);
}

llvm::Constant* acorn::IRGenerator::gen_constant_default_array(ArrayType* arr_type) {

    auto ll_arr_type = llvm::cast<llvm::ArrayType>(gen_type(arr_type));

    bool elms_are_arrays = arr_type->get_elm_type()->is_array();
    auto ll_elm_type = ll_arr_type->getElementType();

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(ll_arr_type->getNumElements());

    auto elm_type = arr_type->get_elm_type();
    for (uint32_t i = 0; i < arr_type->get_length(); i++) {
        ll_values.push_back(gen_constant_default_value(elm_type));
    }

    return llvm::ConstantArray::get(ll_arr_type, ll_values);
}

llvm::Constant* acorn::IRGenerator::gen_constant_array(Array* arr) {
    // If we ever allow assigning an array directly to a pointer then this code
    // will need changed since first it would need to create a constant global
    // array then point to that array.
    auto arr_type = static_cast<ArrayType*>(arr->get_final_type());
    auto ll_arr_type = llvm::cast<llvm::ArrayType>(gen_type(arr_type));
    return gen_constant_array(arr, arr_type, ll_arr_type);
}

llvm::Constant* acorn::IRGenerator::gen_constant_array(Array* arr, ArrayType* arr_type, llvm::ArrayType* ll_arr_type) {

    bool elms_are_arrays = arr_type->get_elm_type()->is_array();
    auto ll_elm_type = ll_arr_type->getElementType();

    llvm::SmallVector<llvm::Constant*> ll_values;
    ll_values.reserve(ll_arr_type->getNumElements());

    auto get_element = [this, elms_are_arrays, ll_elm_type](Expr* elm) finline {
        if (elms_are_arrays) {
            auto elm_arr = static_cast<Array*>(elm);
            auto elm_arr_type = static_cast<ArrayType*>(elm_arr->get_final_type());
            return gen_constant_array(elm_arr, elm_arr_type, llvm::cast<llvm::ArrayType>(ll_elm_type));
        } else if (elm->type->is_struct()) {
            auto initializer = static_cast<StructInitializer*>(elm);
            return gen_constant_struct_initializer(initializer);
        } else {
            return llvm::cast<llvm::Constant>(gen_rvalue(elm));
        }
    };

    for (Expr* elm : arr->elms) {
        if (elm) {
            ll_values.push_back(get_element(elm));
        } else {
            ll_values.push_back(gen_constant_default_value(arr_type->get_elm_type()));
        }
    }
    // Zero fill the rest.
    for (size_t i = arr->elms.size(); i < ll_arr_type->getNumElements(); ++i) {
        ll_values.push_back(gen_constant_default_value(arr_type->get_elm_type()));
    }

    return llvm::ConstantArray::get(ll_arr_type, ll_values);
}