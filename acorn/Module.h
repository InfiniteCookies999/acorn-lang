#ifndef MODULE_H
#define MODULE_H

#include "AST.h"
#include "Namespace.h"

#include <llvm/ADT/SmallVector.h>

namespace acorn {

    class Logger;

    enum class BadScopeLocation {
        Global
    };

    class Module : public Namespace {
    public:
        struct BadScopeNode {
            BadScopeLocation location;
            Node*            node;
            Logger&          logger;
        };
        using BadScopeList = llvm::SmallVector<BadScopeNode>;

        Module() : Namespace(*this) {
        }

        // If it fails it returns the previous import.
        ImportStmt* try_add_import(ImportStmt* importn);
        ImportStmt* find_import(Identifier import_key);
        llvm::DenseMap<Identifier, ImportStmt*>& get_imports() { return imports; }

        void add_duplicate_variable(Var* var, Var* prev_var);

        void add_global_comptime_control_flow(Node* control_flow);

        void mark_bad_scope(BadScopeLocation location, Node* node, Logger& logger);

        void add_source_file(SourceFile* file) {
            source_files.push_back(file);
        }

        const BadScopeList& get_bad_scope_nodes() const {
            return bad_scope_nodes;
        }

        const llvm::SmallVector<Node*>& get_comptime_control_flows() const {
            return comptime_control_flows;
        }

        const llvm::SmallVector<std::pair<Var*, Var*>>& get_redecl_global_variables() const {
            return redecl_global_variables;
        }

    private:
        llvm::SmallVector<SourceFile*> source_files;

        llvm::DenseMap<Identifier, ImportStmt*> imports;

        // Global functions
        llvm::DenseMap<Identifier, FuncList> functions;
        // Global comptime control flow such as #if
        llvm::SmallVector<Node*>             comptime_control_flows;
        // Global variables
        llvm::DenseMap<Identifier, Var*>     variables;
        // Nodes that belong in the wrong scope.
        BadScopeList bad_scope_nodes;
        // Placed in this list if the variable was declared more than once
        // along with the other declaration.
        llvm::SmallVector<std::pair<Var*, Var*>> redecl_global_variables;
    };
}

#endif // MODULE_H