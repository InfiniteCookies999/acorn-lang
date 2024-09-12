#include "Module.h"

void acorn::Module::add_global_function(Func* func) {
    functions[func->name].push_back(func);
}

void acorn::Module::add_global_comptime_control_flow(Node* control_flow) {
    comptime_control_flows.push_back(control_flow);
}

void acorn::Module::add_global_variable(Var* var) {
    variables[var->name] = var;
}

void acorn::Module::mark_bad_scope(BadScopeLocation location, Node* node) {
    bad_scope_nodes.push_back({ location, node });
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