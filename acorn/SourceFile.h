#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include "Source.h"
#include "LineTable.h"
#include "Logger.h"

namespace acorn {
    class Module;
    
    struct SourceFile {
        std::wstring path;
        Buffer       buffer;
        LineTable    line_table;
        Logger       logger;
        Module&      modl;

        SourceFile(const SourceFile&) = delete;

        SourceFile(SourceFile&&) = default;

        SourceFile(Context& context, std::wstring path, Buffer buffer, Module& modl)
            : logger(context, *this),
              path(path),
              line_table(buffer.content, buffer.length),
              buffer(buffer),
              modl(modl) {
        }
    };
}

#endif // SOURCE_FILE_H