#include "Namespace.h"

#include "Module.h"
#include "Logger.h"

void acorn::Namespace::add_declaration(Decl* decl, PageAllocator& allocator) {

    if (auto prev_decl = find_declaration(decl->name)) {
        if (decl->is(NodeKind::FUNC) && prev_decl->is(NodeKind::FUNC_LIST)) {
            auto func_list = static_cast<FuncList*>(prev_decl);
            func_list->push_back(static_cast<Func*>(decl));
            return;
        }

        Decl* dup_decl;
        if (prev_decl->is(NodeKind::FUNC_LIST)) {
            auto func_list = static_cast<FuncList*>(prev_decl);
            dup_decl = (*func_list)[0];
        } else {
            dup_decl = static_cast<Decl*>(prev_decl);
        }

        modl.add_duplicate_decl(decl, dup_decl, scope_location);
        return;
    }

    if (decl->is(NodeKind::FUNC)) {
        auto func_list = allocator.alloc_type<FuncList>();
        new (func_list) FuncList();
        func_list->push_back(static_cast<Func*>(decl));
        declarations.insert({ decl->name, func_list });
    } else {
        declarations.insert({ decl->name, decl });
    }
}

acorn::Node* acorn::Namespace::find_declaration(Identifier name) {
    auto itr = declarations.find(name);
    return itr != declarations.end() ? itr->second : nullptr;
}

llvm::SmallVector<acorn::Decl*> acorn::Namespace::get_composites() const {
    llvm::SmallVector<Decl*> composites;
    for (const auto& [_, decl] : declarations) {
        if (decl->is_composite()) {
            composites.push_back(static_cast<Decl*>(decl));
        }
    }
    return composites;
}
