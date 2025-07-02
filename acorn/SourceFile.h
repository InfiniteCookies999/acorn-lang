#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include "Source.h"
#include "LineTable.h"
#include "Logger.h"
#include "Namespace.h"

namespace llvm {
    class DICompileUnit;
}

namespace acorn {
    class Module;
    class DebugInfoEmitter;

    class SourceFile : public Namespace {
    public:
        // Non-full path used for error reporting.
        std::string       path;
        // The full path to the file.
        std::string       full_path;
        Buffer            buffer;
        LineTable         line_table;
        Logger            logger;
        Module&           modl;
        DebugInfoEmitter* di_emitter;

        SourceFile(const SourceFile&) = delete;

        SourceFile(SourceFile&&) = default;

        SourceFile(Context& context, std::string path, std::string full_path, Buffer buffer, Module& modl);

        void set_namespace(Namespace* nspace) {
            this->nspace = nspace;
        }

        Namespace* get_namespace() const {
            return nspace;
        }

        void add_function(Func* func);
        void add_variable(Var* var);
        void add_composite(Decl* composite);

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

        const llvm::SmallVector<Namespace*>& get_static_imports() const {
            return static_imports;
        }

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