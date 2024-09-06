#include "AST.h"

#include "Logger.h"

void acorn::Decl::get_declared_msg() const {
    auto [line_number, column_number] =
        file->line_table.get_line_and_column_number(loc.ptr);

    Logger::print(Stream::StdErr, "Previously declared at: ");
    Logger::fmt_print(Stream::StdErr, "%s%s%s:", Color::BrightCyan, file->path, Color::BrightWhite);
    Logger::fmt_print(Stream::StdErr, "%s%s%s:", Color::BrightYellow, line_number, Color::BrightWhite);
    Logger::fmt_print(Stream::StdErr, "%s%s%s", Color::BrightYellow, column_number, Color::BrightWhite);
}
