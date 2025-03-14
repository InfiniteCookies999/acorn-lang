#include "Module.h"

#include "Context.h"

acorn::Namespace* acorn::Module::find_namespace(Identifier name) {
    auto itr = namespaces.find(name);
    return itr == namespaces.end() ? nullptr : itr->second;
}

void acorn::Module::mark_bad_scope(ScopeLocation location, Node* node, Logger& logger) {
    bad_scope_nodes.push_back({ location, node, logger });
}

acorn::Namespace* acorn::Module::get_or_create_namespace(Context& context, Identifier ident) {
    auto itr = namespaces.find(ident);
    if (itr != namespaces.end()) {
        return itr->second;
    }

    auto nspace = context.get_allocator().alloc_type<Namespace>();
    new (nspace) Namespace(*this, ScopeLocation::Global);
    namespaces.insert({ ident, nspace });
    return nspace;
}

void acorn::Module::add_duplicate_decl(Decl* decl, Decl* prev_decl,
                                       ScopeLocation scope_location) {
    auto itr = std::ranges::find_if(redecls, [decl, prev_decl](auto tup) {
        const auto [_, decl1, decl2] = tup;
        return (decl == decl1 && prev_decl == decl2) ||
                (decl == decl2 && prev_decl == decl1);
    });
    if (itr == redecls.end()) {
        redecls.push_back({ scope_location, decl, prev_decl });
    }
}