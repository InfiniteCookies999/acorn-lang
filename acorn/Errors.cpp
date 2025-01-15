#include "Errors.h"

#include <format>

#include "AST.h"
#include "Type.h"
#include "SourceFile.h"
#include "Logger.h"

std::string acorn::get_error_msg_for_value_not_fit_type(Type* type) {
    return std::format("The value could not fit into a {} bit {} integer",
                       type->get_number_of_bits(), type->is_signed() ? "signed" : "unsigned");
}

void acorn::print_source_location(Logger& logger, SourceFile* file, SourceLoc location) {
    auto [line_number, _] =
        file->line_table.get_line_and_column_number(location.ptr);

    logger.fmt_print("%s%s%s:", Color::BrightCyan, file->path, Color::BrightWhite);
    logger.fmt_print("%s%s%s", Color::BrightYellow, line_number, Color::BrightWhite);
}