#include "Namespace.h"

#include "Module.h"
#include "Logger.h"

void acorn::Namespace::add_function(Func* func) {
    functions[func->name].push_back(func);
}

void acorn::Namespace::add_variable(Var* var) {
    auto [prev_itr, success] = variables.try_emplace(var->name, var);
    if (!success) {
        modl.add_duplicate_decl(var, prev_itr->second, scope_location);
    }
}

void acorn::Namespace::add_composite(Decl* composite) {
    auto [prev_itr, success] = composites.try_emplace(composite->name, composite);
    if (!success) {
        modl.add_duplicate_decl(composite, prev_itr->second, scope_location);
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

acorn::Decl* acorn::Namespace::find_composite(Identifier name) {
    auto itr = composites.find(name);
    return itr != composites.end() ? itr->second : nullptr;
}