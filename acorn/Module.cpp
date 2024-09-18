#include "Module.h"

acorn::ImportStmt* acorn::Module::try_add_import(ImportStmt* importn) {
    auto [itr, success] = imports.try_emplace(importn->import_key, importn);
    return success ? nullptr : itr->second;
}

acorn::ImportStmt* acorn::Module::find_import(Identifier import_key) {
    auto itr = imports.find(import_key);
    return itr == imports.end() ? nullptr : itr->second;
}

void acorn::Module::add_global_function(Func* func) {
    functions[func->name].push_back(func);
}

void acorn::Module::add_global_comptime_control_flow(Node* control_flow) {
    comptime_control_flows.push_back(control_flow);
}

void acorn::Module::add_global_variable(Var* var) {
    auto [prev_itr, success] = variables.try_emplace(var->name, var);
    if (!success) {
        Var* prev_var = prev_itr->second;
        auto itr = std::ranges::find_if(redecl_global_variables, [var, prev_var](auto pair) {
            const auto [var1, var2] = pair;
            return (var == var1 && prev_var == var2) ||
                   (var == var2 && prev_var == var1);
        });
        if (itr == redecl_global_variables.end()) {
            redecl_global_variables.push_back({ var, prev_var });
        }
    }
}

void acorn::Module::mark_bad_scope(BadScopeLocation location, Node* node, Logger& logger) {
    bad_scope_nodes.push_back({ location, node, logger });
}

acorn::FuncList* acorn::Module::find_global_funcs(Identifier name) {
    auto itr = functions.find(name);
    if (itr != functions.end()) {
        return &itr->second;
    }
    return nullptr;
}

acorn::Var* acorn::Module::find_global_variable(Identifier name) {
    auto itr = variables.find(name);
    if (itr != variables.end()) {
        return itr->second;
    }
    return nullptr;
}