#include "Module.h"

#include "Context.h"

acorn::Namespace* acorn::Module::find_namespace(Identifier name) {
    auto itr = namespaces.find(name);
    return itr == namespaces.end() ? nullptr : itr->second;
}

acorn::ImportStmt* acorn::Module::try_add_import(ImportStmt* importn) {
    auto [itr, success] = imports.try_emplace(importn->key.back(), importn);
    return success ? nullptr : itr->second;
}

acorn::ImportStmt* acorn::Module::find_import(Identifier import_key) {
    auto itr = imports.find(import_key);
    return itr == imports.end() ? nullptr : itr->second;
}

void acorn::Module::add_duplicate_variable(Var* var, Var* prev_var) {
    auto itr = std::ranges::find_if(redecl_global_variables, [var, prev_var](auto pair) {
        const auto [var1, var2] = pair;
        return (var == var1 && prev_var == var2) ||
                (var == var2 && prev_var == var1);
    });
    if (itr == redecl_global_variables.end()) {
        redecl_global_variables.push_back({ var, prev_var });
    }
}

void acorn::Module::add_global_comptime_control_flow(Node* control_flow) {
    comptime_control_flows.push_back(control_flow);
}

void acorn::Module::mark_bad_scope(BadScopeLocation location, Node* node, Logger& logger) {
    bad_scope_nodes.push_back({ location, node, logger });
}

acorn::Namespace* acorn::Module::get_or_create_namespace(Context& context, Identifier ident) {
    auto itr = namespaces.find(ident);
    if (itr != namespaces.end()) {
        return itr->second;
    }

    auto nspace = context.get_allocator().alloc_type<Namespace>();
    namespaces.insert({ ident, nspace });
    return nspace;
}