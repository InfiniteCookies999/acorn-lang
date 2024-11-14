#ifndef MODULE_H
#define MODULE_H

#include "AST.h"
#include "Namespace.h"

#include <llvm/ADT/SmallVector.h>

namespace acorn {

    class Logger;

    class Module : public Namespace {
    public:
        struct BadScopeNode {
            ScopeLocation location;
            Node*         node;
            Logger&       logger;
        };
        using BadScopeList = llvm::SmallVector<BadScopeNode>;
        using DupDeclList = llvm::SmallVector<std::tuple<ScopeLocation, Decl*, Decl*>>;

        Module() : Namespace(*this) {
        }

        // If it fails it returns the previous import.
        ImportStmt* try_add_import(ImportStmt* importn);
        ImportStmt* find_import(Identifier import_key);
        llvm::DenseMap<Identifier, ImportStmt*>& get_imports() { return imports; }

        Namespace* find_namespace(Identifier name);

        void add_global_comptime_control_flow(Node* control_flow);

        void mark_bad_scope(ScopeLocation location, Node* node, Logger& logger);

        void add_duplicate_decl(Decl* decl, Decl* prev_decl, 
                                ScopeLocation scope_location);

        Namespace* get_or_create_namespace(Context& context, Identifier ident);

        void add_source_file(SourceFile* file) {
            source_files.push_back(file);
        }

        const BadScopeList& get_bad_scope_nodes() const {
            return bad_scope_nodes;
        }

        const llvm::SmallVector<Node*>& get_comptime_control_flows() const {
            return comptime_control_flows;
        }

        const DupDeclList& get_declaration_duplicates() const {
            return redecls;
        }

    private:
        llvm::SmallVector<SourceFile*> source_files;
        llvm::DenseMap<Identifier, Namespace*> namespaces;

        llvm::DenseMap<Identifier, ImportStmt*> imports;

        // Global comptime control flow such as #if
        llvm::SmallVector<Node*>             comptime_control_flows;
        // Nodes that belong in the wrong scope.
        BadScopeList bad_scope_nodes;

        // Placed in this list if the declaration was declared more than once.
        DupDeclList redecls;
    };
}

#endif // MODULE_H