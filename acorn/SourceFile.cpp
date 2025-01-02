#include "SourceFile.h"

#include "Module.h"

acorn::SourceFile::SourceFile(Context& context, std::wstring path, std::wstring full_path, Buffer buffer, Module& modl)
    : logger(context, *this),
      path(std::move(path)),
      full_path(std::move(full_path)),
      line_table(buffer.content, buffer.length),
      buffer(buffer),
      modl(modl),
      nspace(&modl),
      Namespace(modl) {
}


void acorn::SourceFile::add_function(Func* func) {
    if (has_public_access(func)) {
        nspace->add_function(func);
    } else {
        Namespace::add_function(func);
    }
}

void acorn::SourceFile::add_variable(Var* var) {
    var->is_global = true;
    if (has_public_access(var)) {
        nspace->add_variable(var);
    } else {
        Namespace::add_variable(var);
    }
}

void acorn::SourceFile::add_composite(Decl* composite) {
    if (has_public_access(composite)) {
        nspace->add_composite(composite);
    } else {
        Namespace::add_composite(composite);
    }
}

bool acorn::SourceFile::has_public_access(Decl* decl) const {
    return decl->has_modifier(Modifier::Public) ||
          (!decl->has_modifier(Modifier::Private) && default_access == Modifier::Public);
}

acorn::ImportStmt* acorn::SourceFile::try_add_import(ImportStmt* importn) {
    auto final_key_part = importn->key.back();
    auto [itr, success] = imports.try_emplace(final_key_part.name, importn);
    return success ? nullptr : itr->second;
}

acorn::ImportStmt* acorn::SourceFile::find_import(Identifier import_key) {
    auto itr = imports.find(import_key);
    return itr == imports.end() ? nullptr : itr->second;
}

acorn::FuncList* acorn::SourceFile::find_static_import_functions(Identifier name) {
    for (Namespace* nspace : static_imports) {
        if (auto funcs = nspace->find_functions(name)) {
            return funcs;
        }
    }
    return nullptr;
}

acorn::Var* acorn::SourceFile::find_static_import_variable(Identifier name) {
    for (Namespace* nspace : static_imports) {
        if (auto var = nspace->find_variable(name)) {
            return var;
        }
    }
    return nullptr;
}