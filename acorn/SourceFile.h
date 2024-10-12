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

        void add_function(Func* func);
        void add_variable(Var* var);

        void set_default_access(uint32_t default_access) {
            this->default_access = default_access;
        }
        uint32_t get_default_access() const {
            return default_access;
        }

        SourceFile(Context& context, std::wstring path, Buffer buffer, Module& modl)
            : logger(context, *this),
              path(std::move(path)),
              line_table(buffer.content, buffer.length),
              buffer(buffer),
              modl(modl),
              Namespace(modl) {
        }

    private:
        uint32_t default_access = Modifier::Private;
    
        bool has_public_access(Decl* decl) const;

    };
}

#endif // SOURCE_FILE_H