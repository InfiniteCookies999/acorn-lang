#include "Errors.h"

#include <format>
#include <llvm/IR/Module.h>

#include "AST.h"
#include "Type.h"
#include "SourceFile.h"
#include "Logger.h"

std::string acorn::get_error_msg_for_value_not_fit_type(Type* type, llvm::Module& ll_module) {
    return std::format("The value could not fit into a {} bit {} integer",
                       type->get_number_of_bits(ll_module), type->is_signed() ? "signed" : "unsigned");
}

void acorn::print_source_location(Logger& logger, SourceFile* file, SourceLoc location) {
    auto [line_number, _] =
        file->line_table.get_line_and_column_number(location.ptr);

    logger.fmt_print("%s%s%s:", Color::BrightCyan, file->path, Color::BrightWhite);
    logger.fmt_print("%s%s%s", Color::BrightYellow, line_number, Color::BrightWhite);
}

void acorn::report_cannot_specify_auto_ptr_and_type_for_variable(Logger& logger, Var* var) {
    auto ptr = var->loc.end();
    while (is_whitespace(*ptr)) {
        ++ptr;
    }

    logger.begin_error(var->loc, "Cannot specify pointer for inference because the variable also specifies a type")
          .end_error(ErrCode::ParseVariableWithPtrAutoSpecifiesType);
}
