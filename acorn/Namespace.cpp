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

void acorn::Namespace::add_struct(Struct* nstruct) {
    auto [prev_itr, success] = structs.try_emplace(nstruct->name, nstruct);
    if (!success) {
        modl.add_duplicate_decl(nstruct, prev_itr->second, scope_location);
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

acorn::Struct* acorn::Namespace::find_struct(Identifier name) {
    auto itr = structs.find(name);
    return itr != structs.end() ? itr->second : nullptr;
}