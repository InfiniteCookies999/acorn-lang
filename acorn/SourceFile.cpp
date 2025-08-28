#include "SourceFile.h"

#include "Module.h"

acorn::SourceFile::SourceFile(Context& context, std::string path, std::string full_path, Buffer buffer, Module& modl)
    : logger(context, *this)
    , path(std::move(path))
    , full_path(std::move(full_path))
    , line_table(buffer.content, buffer.length)
    , buffer(buffer)
    , modl(modl)
    , nspace(&modl)
    , Namespace(modl) {}

void acorn::SourceFile::add_declaration(Decl* decl, PageAllocator& allocator) {
    if (has_public_access(decl)) {
        nspace->add_declaration(decl, allocator);
    } else {
        Namespace::add_declaration(decl, allocator);
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
