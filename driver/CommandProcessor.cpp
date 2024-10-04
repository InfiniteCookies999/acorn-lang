#include "CommandProcessor.h"

#include "Logger.h"

CommandLineProcessor::CommandLineProcessor(acorn::AcornLang& acorn, int argc)
    : acorn(acorn), argc(argc) {
}

void CommandLineProcessor::add_flag(llvm::StringRef flag_name, AcornSetterCallback setter) {
    flags.push_back({
        .flag_name = flag_name,
        .acorn_setter = setter,
    });
}

void CommandLineProcessor::add_flag(llvm::StringRef flag_name, AliasList aliases, AcornSetterCallback setter) {
    flags.push_back({
        .flag_name = flag_name,
        .aliases = std::move(aliases),
        .acorn_setter = setter,
    });
}

CommandLineProcessor::Flag& CommandLineProcessor::add_flag(llvm::StringRef flag_name,
                                                           const Callback& callback) {
    flags.push_back({
        .flag_name = flag_name,
        .callback = callback
    });
    return flags.back();
}

CommandLineProcessor::Flag& CommandLineProcessor::add_flag(llvm::StringRef flag_name,
                                                           AliasList aliases,
                                                           const Callback& callback) {
    flags.push_back({
        .flag_name = flag_name,
        .aliases = std::move(aliases),
        .callback = callback,
    });
    return flags.back();
}

bool CommandLineProcessor::process(llvm::StringRef flag_name, char* rest[], int idx) {
    for (Flag& flag : flags) {
        if (!(flag.flag_name == flag_name ||
            std::ranges::find(flag.aliases, flag_name) != flag.aliases.end()
            )) {
            continue;
        }

        if (flag.acorn_setter) {
            (acorn.*flag.acorn_setter)();
        } else {
            if (flag.requires_value && idx + 1 == argc) {
                acorn::Logger::global_error(*acorn.get_context(), "%s for flag -%s", flag.value_error_msg, flag_name)
                    .end_error(acorn::ErrCode::GlobalMissingArgumentForFlag);
                return false;
            }

            flag.callback(rest);
            return flag.requires_value;
        }
        return false;
    }
    acorn::Logger::global_error(*acorn.get_context(), "Unknown flag: -%s", flag_name)
        .end_error(acorn::ErrCode::GlobalUnknownCompilerFlag);
    return false;
}
