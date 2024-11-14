#include "AST.h"

#include "Logger.h"
#include "SourceFile.h"

void acorn::Decl::show_prev_declared_msg(Logger& logger) const {
    logger.print("Previously declared at: ");
    show_location_msg(logger);
}

acorn::Logger& acorn::Decl::get_logger() const { return file->logger; }

acorn::Module& acorn::Decl::get_module() const { return file->modl; }

void acorn::Decl::show_location_msg(Logger& logger) const {
    auto [line_number, column_number] =
        file->line_table.get_line_and_column_number(loc.ptr);
    
    logger.fmt_print("%s%s%s:", Color::BrightCyan, file->path, Color::BrightWhite);
    logger.fmt_print("%s%s%s", Color::BrightYellow, line_number, Color::BrightWhite);
}

const char* acorn::Modifier::to_string(uint32_t modifier) {
    switch (modifier) {
    case Native:    return "native";
    case DllImport: return "dllimport";
    case Public:    return "pub";
    case Private:   return "prv";
    default:
        acorn_fatal("Fail to implement to_string() for modifier");
        return nullptr;
    }
}

acorn::SourceLoc acorn::Decl::get_modifier_location(uint32_t modifier) {
    // We have to manually implement rfind because otherwise we would have to
    // create a std::string for the file contents which could potentially be
    // slow.
    const char* ptr = loc.ptr;

    auto str = Modifier::to_string(modifier);
    auto str_len = strlen(str);

    const char* buf_beg = file->buffer.content;
    while (ptr - str_len + 1 >= buf_beg) {
        if (std::memcmp(ptr - str_len + 1, str, str_len) == 0) {
            // Found the source location!
            return SourceLoc{ ptr - str_len + 1, static_cast<uint16_t>(str_len) };
        }
        --ptr;
    }

    // We should not get here as long as the user actually specified a modifier
    // that we have.
    acorn_fatal("unreachable");
    return SourceLoc{};
}

acorn::Var* acorn::Func::find_parameter(Identifier name) const {
    auto itr = std::ranges::find_if(params, [name](Var* param) {
        return param->name == name;
    });
    return itr != params.end() ? *itr : nullptr;
}

acorn::Var* acorn::Struct::find_field(Identifier name) const {
    return nspace->find_variable(name);
}