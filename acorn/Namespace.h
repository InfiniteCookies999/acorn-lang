#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "AST.h"

#include <llvm/ADT/DenseMap.h>

namespace acorn {

    class Module;

    class Namespace {
    public:

        Namespace(Module& modl) : modl(modl) {
        }

        void add_function(Func* func);
        void add_variable(Var* var);

        FuncList* find_functions(Identifier name);
        Var* find_variable(Identifier name);

        const llvm::DenseMap<Identifier, Var*>& get_variables() const {
            return variables;
        }

        const llvm::DenseMap<Identifier, FuncList>& get_functions() const {
            return functions;
        }

    private:
        Module& modl;

        llvm::DenseMap<Identifier, FuncList> functions;
        llvm::DenseMap<Identifier, Var*>     variables;
    };
}

#endif // NAMESPACE_H