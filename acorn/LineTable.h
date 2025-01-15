#ifndef LINE_TABLE_H
#define LINE_TABLE_H

#include <llvm/ADT/SmallVector.h>

#include "Source.h"

namespace acorn {

    class LineTable {
    public:
        LineTable(const char* content, size_t buf_length)
            : content(content), buf_length(buf_length) {
        }

        void build_table();

        size_t get_line_number(SourceLoc location);
        size_t get_line_number(const char* ptr);

        std::pair<size_t, size_t> get_line_and_column_number(SourceLoc location);
        std::pair<size_t, size_t> get_line_and_column_number(const char* ptr);

    private:
        const char* content;
        size_t      buf_length;
        llvm::SmallVector<size_t> line_offsets; // Storing the offsets of the line numbers.
    };
}

#endif // LINE_TABLE_H