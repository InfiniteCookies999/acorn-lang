#include "Errors.h"

#include "AST.h"
#include "Type.h"

#include <format>

std::string acorn::get_error_msg_for_value_not_fit_type(Number* number) {
    return std::format("The value could not fit into a {} bit integer",
                       number->type->get_number_of_bits());
}
