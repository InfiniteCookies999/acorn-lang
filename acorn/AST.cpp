#include "AST.h"

#include "Logger.h"
#include "SourceFile.h"
#include "Type.h"
#include "SourceExpansion.h"
#include "GenericReset.h"
#include "DeepCopyAST.h"

void acorn::Decl::show_prev_declared_msg(Logger& logger) const {
    logger.print("Previously declared at: ");
    show_location_msg(logger);
}

acorn::Logger& acorn::Decl::get_logger() const { return file->logger; }

acorn::Module& acorn::Decl::get_module() const { return file->modl; }

void acorn::Decl::show_location_msg(Logger& logger) const {
    print_source_location(logger, file, loc);
}

const char* acorn::Decl::get_composite_kind() const {
    switch (kind) {
    case NodeKind::Struct: return "struct";
    case NodeKind::Enum:   return "enum";
    default:
        acorn_fatal("Unknown composite kind");
        return "";
    }
}

const char* acorn::Modifier::to_string(uint32_t modifier) {
    switch (modifier) {
    case Native:    return "native";
    case DllImport: return "dllimport";
    case Public:    return "public";
    case Private:   return "private";
    case Readonly:  return "readonly";
    default:
        acorn_fatal("Fail to implement to_string() for modifier");
        return nullptr;
    }
}

acorn::SourceLoc acorn::Decl::get_modifier_location(uint32_t modifier) const {
    // We have to manually implement rfind because otherwise we would have to
    // create a std::string for the file contents which could potentially be
    // slow.
    const char* ptr = loc.ptr;

    auto str = Modifier::to_string(modifier);
    auto str_len = strlen(str);

    const char* buf_beg = file->buffer.content;
    while (ptr - str_len + 1 >= buf_beg) {
        if (std::memcmp(ptr - str_len + 1, str, str_len) == 0) {
            // Found the source location!
            return SourceLoc{ ptr - str_len + 1, static_cast<uint16_t>(str_len) };
        }
        --ptr;
    }

    // We should not get here as long as the user actually specified a modifier
    // that we have.
    acorn_fatal("unreachable");
    return SourceLoc{};
}

acorn::SourceLoc acorn::Func::get_function_const_location() const {
    SourceLoc loc = this->loc;
    const char* ptr = loc.ptr;
    while (ptr - 5 >= file->buffer.content) {
        if (memcmp(ptr - 5, "const", 5) == 0) {
            break;
        }
        --ptr;
    }
    if (memcmp(ptr - 5, "const", 5) != 0) {
        acorn_fatal("Expected to find const for function");
    }
    return SourceLoc::from_ptrs(ptr - 5, ptr);
}

acorn::PointSourceLoc acorn::Func::get_function_first_default_param_location() const {
    Var* found_param = nullptr;
    for (Var* param : params) {
        if (param->assignment) {
            found_param = param;
            break;
        }
    }

    if (!found_param) {
        acorn_fatal("Failed to find parameter");
        return {};
    }

    const char* eq_ptr = found_param->assignment->loc.ptr;
    while (*eq_ptr != '=' && *eq_ptr != ':' && eq_ptr > loc.ptr) {
        --eq_ptr;
    }

    if (eq_ptr <= loc.ptr) {
        acorn_fatal("Should have hit = or :");
        return {};
    }

    auto expanded_assignment_loc = expand(found_param->assignment);

    const char* end_ptr   = expanded_assignment_loc.end();
    const char* start_ptr = found_param->loc.ptr;

    return PointSourceLoc{
        .ptr = start_ptr,
        .length = static_cast<uint16_t>(end_ptr - start_ptr),
        .point = eq_ptr,
        .point_length = 1
    };
}

std::string acorn::Func::get_decl_string() const {
    std::string str;
    if (is_generic()) {
        str += "generics[";
        size_t generic_count = 0;
        for (Generic* genericn : generics) {
            str += genericn->type->to_string();
            if (generic_count + 1 != generics.size()) {
                str += ", ";
            }
            ++generic_count;
        }
        str += "] ";
    }
    if (is_constant) {
        str += "const ";
    }
    str += name.to_string().str();
    str += "(";
    size_t count = 0;
    for (Var* param : params) {
        bool is_last = count + 1 == params.size();
        if (is_last && uses_varargs) {
            auto slice_type = static_cast<SliceType*>(param->type);
            auto elm_type = slice_type->get_elm_type();
            str += elm_type->to_string() + "...";
        } else {
            if (param->has_implicit_ptr) {
                auto ptr_type = static_cast<PointerType*>(param->type);
                auto elm_type = ptr_type->get_elm_type();
                str += elm_type->to_string() + "^";
            } else {
                str += param->type->to_string();
            }
        }
        if (!is_last) {
            str += ", ";
        }
        ++count;
    }
    if (uses_native_varargs) {
        if (!params.empty()) {
            str += ", ";
        }
        str += "...";
    }
    str += ")";
    return str;
}

bool acorn::Func::forwards_varargs(Expr* arg_value) const {
    if (!uses_varargs) return false;
    if (arg_value->is_not(NodeKind::IdentRef)) return false;

    auto ref = static_cast<IdentRef*>(arg_value);
    auto last_param = params.back();

    if (!ref->is_var_ref()) return false;

    auto var = ref->var_ref;
    if (!var->is_param()) return false;
    if (ref->var_ref != last_param) return false;

    auto candidate_last_param = params.back();
    auto this_slice_type = last_param->type;
    auto candidate_slice_type = candidate_last_param->type;

    if (!has_valid_constness(this_slice_type, candidate_slice_type)) {
        return false;
    }

    this_slice_type = this_slice_type->remove_all_const();
    candidate_slice_type = candidate_slice_type->remove_all_const();
    return this_slice_type->is(candidate_slice_type);
}

acorn::Var* acorn::Func::find_parameter(Identifier name) const {
    auto itr = std::ranges::find_if(params, [name](Var* param) {
        return param->name == name;
    });
    return itr != params.end() ? *itr : nullptr;
}

acorn::GenericFuncInstance* acorn::Func::get_generic_instance(PageAllocator& allocator,
                                                              llvm::SmallVector<Type*> bound_types,
                                                              llvm::SmallVector<Type*> qualified_param_types,
                                                              Struct* parent_struct) {
    // Check to see if the instance already exists.
    for (auto* instance : generic_instances) {
        if (instance->bound_types == bound_types) {
            return instance;
        }
    }

    auto generic_instance = allocator.alloc_type<GenericFuncInstance>();
    new (generic_instance) GenericFuncInstance();
    generic_instance->bound_types = std::move(bound_types);
    generic_instance->qualified_decl_types  = std::move(qualified_param_types);
    generic_instance->structn = parent_struct;

    generic_instances.push_back(generic_instance);
    return generic_instance;
}

void acorn::Func::bind_generic_instance(GenericFuncInstance* generic_instance) {

    // Reset the current state of the function to be able to process the function
    // in sema/irgen as if it was just parsed.
    reset_generic_function(this);

    this->generic_instance = generic_instance;
    this->structn = generic_instance->structn;

    // Bind the types bound to this instance of the generic function.
    return_type = generic_instance->qualified_decl_types[0];
    for (size_t i = 0; i < params.size(); i++) {
        auto qualified_type = generic_instance->qualified_decl_types[i + 1];
        params[i]->type = qualified_type;
    }
}

acorn::Var* acorn::Struct::find_field(Identifier name) const {
    return nspace->find_variable(name);
}

const acorn::Struct::InterfaceExtension* acorn::Struct::find_interface_extension(Identifier name) const {
    auto itr = std::ranges::find_if(interface_extensions, [name](auto& extension) {
        return extension.interfacen->name == name;;
    });
    return itr != interface_extensions.end() ? itr : nullptr;
}

acorn::GenericStructInstance* acorn::UnboundGenericStruct::get_generic_instance(PageAllocator& allocator, llvm::SmallVector<Type*> bound_types) {
    // Check to see if the instance already exists.
    for (auto* instance : generic_instances) {
        if (instance->bound_types == bound_types) {
            return instance;
        }
    }

    auto new_struct_instance = deep_copy_struct(allocator, this);
    new_struct_instance->is_struct_instance_copy = true;
    new_struct_instance->bound_types = std::move(bound_types);
    generic_instances.push_back(new_struct_instance);

    auto new_struct_type = StructType::create(allocator, new_struct_instance);
    new_struct_instance->struct_type = new_struct_type;

    return new_struct_instance;
}

acorn::PointSourceLoc acorn::ImportStmt::get_key_location(bool center_by_last) const {
    const auto& last_key_part  = key.back();
    const auto& first_key_part =  key.front();
    const char* start_loc = first_key_part.error_loc.ptr;
    const char* end_loc   = last_key_part.error_loc.ptr + last_key_part.error_loc.length;

    auto loc = SourceLoc::from_ptrs(start_loc, end_loc).to_point_source();
    if (center_by_last) {
        loc.point        = last_key_part.error_loc.ptr;
        loc.point_length = last_key_part.error_loc.length;
    }

    return loc;
}

acorn::PointSourceLoc acorn::DotOperator::expand_access_only() const {
    // There can still be whitespace after the .
    //
    // This is valid:
    // a.  b;
    const char* end_ptr = loc.end();
    while (is_whitespace(*end_ptr)) {
        ++end_ptr;
    }
    end_ptr += ident.to_string().size();

    return PointSourceLoc{
        .ptr = loc.ptr,
        .length = static_cast<uint16_t>(end_ptr - loc.ptr),
        .point = loc.ptr,
        .point_length = 1
    };
}

acorn::SourceLoc acorn::NamedValue::get_name_location() const {
    const char* ptr = assignment->loc.ptr;
    while (*ptr != '=' || is_whitespace(*ptr)) {
        --ptr;
    }
    acorn_assert(*ptr == '=', "should hit =");
    --ptr; // back off =.

    while (is_whitespace(*ptr)) {
        --ptr;
    }
    auto is_ident_char = [](char c) {
        return (c >= '0' && c <= '9') ||
               (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
                c == '_';
    };
    const char* name_end = ptr + 1;
    acorn_assert(is_ident_char(*ptr), "should hit identifier character");
    while (is_ident_char(*ptr)) {
        --ptr;
    }
    ++ptr;
    return SourceLoc::from_ptrs(ptr, name_end);
}
