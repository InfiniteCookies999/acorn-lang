#include "CommandProcessor.h"

#include "Logger.h"

void CommandConsumer::next(const std::function<void(std::wstring)>& then, const char* missing_error_msg) {
    if (offset == argc) {
        acorn::Logger::global_error(*compiler.get_context(), "%s for flag -%s", missing_error_msg, flag_name)
            .end_error(acorn::ErrCode::GlobalMissingArgumentForFlag);
        return;
    }

    char* arg = argv[offset++];
    then(wconverter.from_bytes(arg));
}

void CommandConsumer::next(void(acorn::Compiler::*then)(std::wstring), const char* missing_error_msg) {
    auto bound_then = std::bind(then, std::ref(compiler), std::placeholders::_1);
    next(bound_then, missing_error_msg);
}

CommandLineProcessor::CommandLineProcessor(acorn::Compiler& compiler, int argc, char* argv[])
    : compiler(compiler), argc(argc), consumer(compiler, argc, argv) {
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
                                                           const Callback& callback,
                                                           bool only_starts_with) {
    flags.push_back({
        .flag_name = flag_name,
        .callback = callback,
        .only_starts_with = only_starts_with
    });
    return flags.back();
}

CommandLineProcessor::Flag& CommandLineProcessor::add_flag(llvm::StringRef flag_name,
                                                           AliasList aliases,
                                                           const Callback& callback) {
    flags.push_back({
        .flag_name = flag_name,
        .aliases = std::move(aliases),
        .callback = callback
    });
    return flags.back();
}

template<class... Ts>
struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

int CommandLineProcessor::process(llvm::StringRef flag_name, int idx) {


    for (Flag& flag : flags) {
        if (flag.only_starts_with) {
            if (!flag_name.starts_with(flag.flag_name)) {
                continue;
            }
        } else if (!(flag.flag_name == flag_name ||
            std::ranges::find(flag.aliases, flag_name) != flag.aliases.end()
            )) {
            continue;
        }

        if (flag.acorn_setter) {
            (compiler.*flag.acorn_setter)();
            return idx + 1;
        }

        consumer.offset = idx + 1; 
        consumer.flag_name = consumer.wconverter.from_bytes(flag_name.data());

        flag.callback(consumer);

        return consumer.offset;
    }

    acorn::Logger::global_error(*compiler.get_context(), "Unknown flag: -%s", flag_name)
        .end_error(acorn::ErrCode::GlobalUnknownCompilerFlag);
    
    return idx + 1;
}
