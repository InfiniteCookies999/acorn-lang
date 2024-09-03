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

        void mark_bad_scope(BadScopeLocation location, Node* node);

        FuncList* find_global_funcs(Identifier name);

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

    private:
        llvm::SmallVector<SourceFile*> source_files;

        // Global functions
        llvm::DenseMap<Identifier, FuncList> functions;
        // Global variables
        llvm::DenseMap<Identifier, Var*>     variables;
        // Nodes that belong in the wrong scope.
        BadScopeList bad_scope_nodes;
    };
}

#endif // MODULE_H