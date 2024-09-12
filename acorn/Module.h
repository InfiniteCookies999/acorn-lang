#ifndef MODULE_H
#define MODULE_H

#include "AST.h"

#include <llvm/ADT/SmallVector.h>

namespace acorn {

    enum class BadScopeLocation {
        Global
    };

    class Module {
    public:
        using BadScopeList = llvm::SmallVector<std::pair<BadScopeLocation, Node*>>;

        void add_global_function(Func* func);
        void add_global_variable(Var* var);
        void add_global_comptime_control_flow(Node* control_flow);

        void mark_bad_scope(BadScopeLocation location, Node* node);

        FuncList* find_global_funcs(Identifier name);

        Var* find_global_variable(Identifier name);

        void add_source_file(SourceFile* file) {
            source_files.push_back(file);
        }

        const BadScopeList& get_bad_scope_nodes() const {
            return bad_scope_nodes;
        }

        const llvm::DenseMap<Identifier, Var*>& get_global_variables() const {
            return variables;
        }

        const llvm::DenseMap<Identifier, FuncList>& get_global_functions() const {
            return functions;
        }

        const llvm::SmallVector<Node*>& get_comptime_control_flows() const {
            return comptime_control_flows;
        }

        const llvm::SmallVector<std::pair<Var*, Var*>>& get_redecl_global_variables() const {
            return redecl_global_variables;
        }

    private:
        llvm::SmallVector<SourceFile*> source_files;

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