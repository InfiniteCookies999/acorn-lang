#ifndef NAMESPACE_H
#define NAMESPACE_H

#include "AST.h"

#include <llvm/ADT/DenseMap.h>

namespace acorn {

    class Module;

    enum class ScopeLocation {
        GLOBAL,
        STRUCT,
        INTERFACE
    };

    class Namespace {
    public:

        Namespace(Module& modl, ScopeLocation scope_location = ScopeLocation::GLOBAL) :
            modl(modl), scope_location(scope_location) {
        }

        void add_declaration(Decl* decl, PageAllocator& allocator);

        Node* find_declaration(Identifier name);

        void set_duplicates_checked() {
            duplicates_checked = true;
        }

        bool have_duplicates_been_checked() const {
            return duplicates_checked;
        }

        Module& get_module() const {
            return modl;
        }

        const llvm::DenseMap<Identifier, Node*>& get_declarations() const {
            return declarations;
        }

        // This function constructs a list of just the composites from the
        // declarations in the `Namespace` but should not be used except
        // when error reporting.
        llvm::SmallVector<Decl*> get_composites() const;

    private:
        Module&       modl;
        ScopeLocation scope_location;
        bool          duplicates_checked = false;

        llvm::DenseMap<Identifier, Node*> declarations;
    };
}

#endif // NAMESPACE_H
