#include "LineTable.h"

void acorn::LineTable::build_table() {

    // First line starts at beginning of buffer.
    line_offsets.push_back(0);

    for (size_t i = 0; i < buf_length; ++i) {
        if (content[i] == '\n') {
            line_offsets.push_back(i + 1);
        } else if (content[i] == '\r' && i+1 < buf_length && content[i+1] != '\n') {
            line_offsets.push_back(i + 1);
        }
    }
}

size_t acorn::LineTable::get_line_number(SourceLoc location) {
    auto [line_number, _] = get_line_and_column_number(location);
    return line_number;
}

size_t acorn::LineTable::get_line_number(const char* ptr) {
    auto [line_number, _] = get_line_and_column_number(ptr);
    return line_number;
}

std::pair<size_t, size_t> acorn::LineTable::get_line_and_column_number(SourceLoc location) {
    return get_line_and_column_number(location.ptr);
}

std::pair<size_t, size_t> acorn::LineTable::get_line_and_column_number(const char* ptr) {
    if (line_offsets.empty()) {
        build_table();
    }

    size_t offset = ptr - content;

    // Uses binary search to find the offset in the table.
    auto itr = std::lower_bound(line_offsets.begin(), line_offsets.end(), offset);
    if (itr == line_offsets.end() || *itr > offset) {
        --itr;
    }

    size_t line_number = std::distance(line_offsets.begin(), itr) + 1;
    size_t column_number = offset - *itr + 1;

    return { line_number, column_number };
}

size_t acorn::LineTable::get_file_offset(size_t line_number) const {
    return line_offsets[line_number];
}