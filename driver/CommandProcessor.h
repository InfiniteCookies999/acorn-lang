#ifndef COMMAND_PROCESSOR_H
#define COMMAND_PROCESSOR_H

#include <llvm/ADT/SmallVector.h>
#include <functional>
#include <Acorn.h>

class CommandLineProcessor {
private:
    using AcornSetterCallback = void(acorn::AcornLang::*)();
    using Callback = std::function<void(char* [])>;
    using AliasList = llvm::SmallVector<llvm::StringRef>;
public:

    struct Flag {
        llvm::StringRef     flag_name;
        AliasList           aliases;
        AcornSetterCallback acorn_setter = nullptr;
        Callback            callback;
        bool                requires_value = false;
        const char*         value_error_msg;

        Flag& req_value(const char* error_msg) {
            requires_value = true;
            value_error_msg = error_msg;
            return *this;
        }
    };

    CommandLineProcessor(acorn::AcornLang& acorn, int argc);

    void add_flag(llvm::StringRef flag_name, AcornSetterCallback setter);
    void add_flag(llvm::StringRef flag_name, AliasList aliases, AcornSetterCallback setter);

    Flag& add_flag(llvm::StringRef flag_name,
                   const Callback& callback);
    Flag& add_flag(llvm::StringRef flag_name,
                   AliasList aliases,
                   const Callback& callback);

    // Returns true if the next argv value should be skipped.
    bool process(llvm::StringRef flag_name, char* rest[], int idx);

private:
    llvm::SmallVector<Flag> flags;
    acorn::AcornLang& acorn;
    int argc;
};

#endif // COMMAND_PROCESSOR_H