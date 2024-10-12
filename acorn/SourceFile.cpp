#include "SourceFile.h"

#include "Module.h"

void acorn::SourceFile::add_function(Func* func) {
    if (has_public_access(func)) {
        modl.add_function(func);
    } else {
        Namespace::add_function(func);
    }
}

void acorn::SourceFile::add_variable(Var* var) {
    if (has_public_access(var)) {
        modl.add_variable(var);
    } else {
        Namespace::add_variable(var);
    }
}

bool acorn::SourceFile::has_public_access(Decl* decl) const {
    return decl->has_modifier(Modifier::Public) ||
          (!decl->has_modifier(Modifier::Private) && default_access == Modifier::Public);
}