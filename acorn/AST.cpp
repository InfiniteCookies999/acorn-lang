#include "AST.h"

#include "Logger.h"
#include "SourceFile.h"
#include "Type.h"
#include "SourceExpansion.h"
#include "PageAllocator.h"

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
    const char* start_ptr = loc.ptr + loc.length;
    go_until(start_ptr, '(', ')');
    // Skip over whitespace until we reach 'const' keyword.
    while (is_whitespace(*start_ptr)) {
        ++start_ptr;
    }

    return SourceLoc::from_ptrs(start_ptr, start_ptr + 5);
}

acorn::PointSourceLoc acorn::Func::get_function_first_default_param_location() const {
    const char* eq_ptr = nullptr;
    Var* found_param = nullptr;
    for (Var* param : params) {
        SourceLoc loc = param->loc;
        const char* ptr = loc.ptr + loc.length;
        while (*ptr != ',' && *ptr != '=' && *ptr == ')') {
            ++ptr;
        }
        if (*ptr == ')') {
            acorn_fatal("This function should only be called if there is a default parameter");
        }
        if (*ptr == '=') {
            found_param = param;
            eq_ptr = ptr;
            break;
        }
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
        for (size_t i = 0; i < generics.size(); i++) {
            auto& generic = generics[i];
            str += generic->type->to_string();
            if (i + 1 != generics.size()) {
                str += ", ";
            }
        }
        str += "] ";
    }
    str += name.to_string().str();
    str += "(";
    size_t count = 0;
    for (Var* param : params) {
        Type* param_type = param->type;
        if (is_generic()) {
            param_type = partially_qualified_types[param->param_idx + 1];
        }

        str += param_type->to_string();
        if (count + 1 != params.size()) {
            str += ", ";
        }
        ++count;
    }
    str += ")";
    if (is_constant) {
        str += " const";
    }
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
                                                              llvm::SmallVector<Type*> generic_bindings,
                                                              llvm::SmallVector<Type*> qualified_param_types) {

    // TODO (maddie): This should check if an instance already exists and if it does it shouldn't
    // create a new one.

    auto generic_instance = allocator.alloc_type<GenericFuncInstance>();
    new (generic_instance) GenericFuncInstance();
    generic_instance->generic_bindings      = std::move(generic_bindings);
    generic_instance->qualified_types = std::move(qualified_param_types);

    generic_instances.push_back(generic_instance);
    return generic_instance;
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