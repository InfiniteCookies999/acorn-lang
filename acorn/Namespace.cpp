#include "Namespace.h"

#include "Module.h"

void acorn::Namespace::add_function(Func* func) {
    functions[func->name].push_back(func);
}

void acorn::Namespace::add_variable(Var* var) {
    auto [prev_itr, success] = variables.try_emplace(var->name, var);
    if (!success) {
        modl.add_duplicate_variable(var, prev_itr->second);
    }
}

acorn::FuncList* acorn::Namespace::find_functions(Identifier name) {
    auto itr = functions.find(name);
    return itr != functions.end() ? &itr->second : nullptr;
}

acorn::Var* acorn::Namespace::find_variable(Identifier name) {
    auto itr = variables.find(name);
    return itr != variables.end() ? itr->second : nullptr;
}
