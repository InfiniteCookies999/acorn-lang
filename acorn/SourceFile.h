#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include "Source.h"
#include "LineTable.h"
#include "Logger.h"
#include "Namespace.h"

namespace acorn {
    class Module;
    
    class SourceFile : public Namespace {
    public:
        std::wstring path;
        Buffer       buffer;
        LineTable    line_table;
        Logger       logger;
        Module&      modl;

        SourceFile(const SourceFile&) = delete;

        SourceFile(SourceFile&&) = default;

        void set_namespace(Namespace* nspace) {
            this->nspace = nspace;
        }

        Namespace* get_namespace() const {
            return nspace;
        }

        void add_function(Func* func);
        void add_variable(Var* var);
        void add_struct(Struct* structn);

        void set_default_access(uint32_t default_access) {
            this->default_access = default_access;
        }
        uint32_t get_default_access() const {
            return default_access;
        }

        // If it fails it returns the previous import.
        ImportStmt* try_add_import(ImportStmt* importn);
        ImportStmt* find_import(Identifier import_key);
        llvm::DenseMap<Identifier, ImportStmt*>& get_imports() { return imports; }

        void add_static_import(Namespace* nspace) {
            static_imports.push_back(nspace);
        }

        FuncList* find_static_import_functions(Identifier name);
        Var* find_static_import_variable(Identifier name);

        SourceFile(Context& context, std::wstring path, Buffer buffer, Module& modl);

    private:
        uint32_t default_access = Modifier::Private;

        // A namespace shared between this file and other files.
        // Defaults to the namespace of the module, the default
        // namespace.
        Namespace* nspace;

        llvm::DenseMap<Identifier, ImportStmt*> imports;
        llvm::SmallVector<Namespace*> static_imports;
    
        bool has_public_access(Decl* decl) const;

    };
}

#endif // SOURCE_FILE_H