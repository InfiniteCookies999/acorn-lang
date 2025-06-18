#ifndef COMMAND_PROCESSOR_H
#define COMMAND_PROCESSOR_H

#include <llvm/ADT/SmallVector.h>
#include <functional>
#include <codecvt>

#include "Compiler.h"

class CommandLineProcessor;

class CommandConsumer {
public:

    enum class Parse {
        Str,
        Int
    };

    void next(const std::function<void(std::string)>& then, const char* missing_error_msg);
    void next(void(acorn::Compiler::*then)(std::string), const char* missing_error_msg);

    void next_eql_pair(const std::function<void(std::string)>& then, const char* missing_error_msg);
    void next_eql_pair(void(acorn::Compiler::* then)(std::string), const char* missing_error_msg);

    [[nodiscard]] CommandConsumer& next_eql_pair(const char* missing_error_msg);

    void parse_int(const std::function<void(int)>& then);

    std::string get_flag_name() const { return flag_name; }

private:
    friend CommandLineProcessor;

    CommandConsumer(acorn::Compiler& compiler, int argc, char* argv[]) :
        compiler(compiler), argc(argc), argv(argv) {
    }

    void report_error_missing_arg(const char* missing_error_msg);

    acorn::Compiler& compiler;

    // The current flag being processed.
    std::string flag_name;

    std::string parsed_value;

    // Offset into the consumed arguments.
    int    offset;
    int    argc;
    char** argv;

};

class CommandLineProcessor {
private:
    using AcornSetterCallback = void(acorn::Compiler::*)();
    using Callback            = std::function<void(CommandConsumer&)>;
    using AliasList           = llvm::SmallVector<llvm::StringRef>;
public:

    struct Flag {
        llvm::StringRef     flag_name;
        AliasList           aliases;
        AcornSetterCallback acorn_setter = nullptr;
        Callback            callback;
        bool                only_starts_with = false;
    };

    CommandLineProcessor(acorn::Compiler& compiler, int argc, char* argv[]);

    void add_flag(llvm::StringRef flag_name, AcornSetterCallback setter);
    void add_flag(llvm::StringRef flag_name, AliasList aliases, AcornSetterCallback setter);

    Flag& add_flag(llvm::StringRef flag_name,
                   const Callback& callback,
                   bool only_starts_with = false);

    Flag& add_flag(llvm::StringRef flag_name,
                   AliasList aliases,
                   const Callback& callback,
                   bool only_starts_with = false);

    // Returns true if the next argv value should be skipped.
    int process(llvm::StringRef flag_name, int idx);

private:
    llvm::SmallVector<Flag> flags;
    acorn::Compiler& compiler;

    CommandConsumer consumer;
    int argc;
};

#endif // COMMAND_PROCESSOR_H