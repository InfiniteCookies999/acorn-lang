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

        SourceFile(Context& context, std::wstring path, Buffer buffer, Module& modl)
            : logger(context, *this),
              path(std::move(path)),
              line_table(buffer.content, buffer.length),
              buffer(buffer),
              modl(modl),
              Namespace(modl) {
        }
    };
}

#endif // SOURCE_FILE_H