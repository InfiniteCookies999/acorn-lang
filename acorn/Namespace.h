#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "AST.h"

#include <llvm/ADT/DenseMap.h>

namespace acorn {

    class Module;

    enum class ScopeLocation {
        Global,
        Struct
    };

    class Namespace {
    public:

        Namespace(Module& modl, ScopeLocation scope_location = ScopeLocation::Global) :
            modl(modl), scope_location(scope_location) {
        }

        void add_function(Func* func);
        void add_variable(Var* var);
        void add_struct(Struct* nstruct);

        FuncList* find_functions(Identifier name);
        Var*      find_variable(Identifier name);
        Struct*   find_struct(Identifier name);

        const llvm::DenseMap<Identifier, Var*>& get_variables() const {
            return variables;
        }

        const llvm::DenseMap<Identifier, FuncList>& get_functions() const {
            return functions;
        }

        const llvm::DenseMap<Identifier, Struct*>& get_structs() const {
            return structs;
        }

        void set_duplicates_checked() {
            duplicates_checked = true;
        }

        bool have_duplicates_been_checked() const {
            return duplicates_checked;
        }

        Module& get_module() const {
            return modl;
        }

    private:
        Module&       modl;
        ScopeLocation scope_location;
        bool          duplicates_checked = false;

        llvm::DenseMap<Identifier, FuncList> functions;
        llvm::DenseMap<Identifier, Var*>     variables;
        llvm::DenseMap<Identifier, Struct*>  structs;
    };
}

#endif // NAMESPACE_H