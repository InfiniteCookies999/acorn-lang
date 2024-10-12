#include "SourceFile.h"

#include "Module.h"

void acorn::SourceFile::add_function(Func* func) {
    if (func->has_modifier(Modifier::Public)){
        modl.add_function(func);
    } else {
        Namespace::add_function(func);
    }
}

void acorn::SourceFile::add_variable(Var* var) {
    if (var->has_modifier(Modifier::Public)) {
        modl.add_variable(var);
    } else {
        Namespace::add_variable(var);
    }
}