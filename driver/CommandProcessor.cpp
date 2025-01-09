#include "CommandProcessor.h"

#include "Logger.h"

void CommandConsumer::next(const std::function<void(std::wstring)>& then, const char* missing_error_msg) {
    if (offset == argc) {
        report_error_missing_arg(missing_error_msg);
        return;
    }

    char* arg = argv[offset++];
    then(wconverter.from_bytes(arg));
}

void CommandConsumer::next(void(acorn::Compiler::*then)(std::wstring), const char* missing_error_msg) {
    auto bound_then = std::bind(then, std::ref(compiler), std::placeholders::_1);
    next(bound_then, missing_error_msg);
}

void CommandConsumer::next_eql_pair(const std::function<void(std::wstring)>& then, const char* missing_error_msg) {
    auto itr = flag_name.find('=');
    if (itr == std::wstring::npos) {
        acorn::Logger::global_error(*compiler.get_context(), "Expected '=' for flag -%s", flag_name)
            .end_error(acorn::ErrCode::GlobalArgumentFlagFailedToParse);
        return;
    }

    auto value = flag_name.substr(itr + 1);
    if (value.empty()) {
        report_error_missing_arg(missing_error_msg);
        return;
    }
    then(value);
}

void CommandConsumer::next_eql_pair(void(acorn::Compiler::* then)(std::wstring), const char* missing_error_msg) {
    auto bound_then = std::bind(then, std::ref(compiler), std::placeholders::_1);
    next_eql_pair(bound_then, missing_error_msg);
}

CommandConsumer& CommandConsumer::next_eql_pair(const char* missing_error_msg) {
    parsed_value = L"";
    next_eql_pair([this](std::wstring value) {
        parsed_value = value;
    }, missing_error_msg);
    return *this;
}

void CommandConsumer::parse_int(const std::function<void(int)>& then) {
    if (parsed_value.empty()) return;

    int value = 0, prev_value;
    for (wchar_t c : parsed_value) {
        if (c < 48 || c > 57) {
            acorn::Logger::global_error(*compiler.get_context(),
                                        "Unexpected character when parsing integer for flag %s", flag_name)
                .end_error(acorn::ErrCode::GlobalArgumentFlagFailedToParse);
            return;
        }

        prev_value = value;
        value = value * 10;
        value += ((int)c - '0');

        if (value / 10 < prev_value) {
            acorn::Logger::global_error(*compiler.get_context(),
                                        "Integer overflow when parsing integer for flag %s", flag_name)
                .end_error(acorn::ErrCode::GlobalArgumentFlagFailedToParse);
            return;
        }
    }

    then(value);
}

void CommandConsumer::report_error_missing_arg(const char* missing_error_msg) {
    acorn::Logger::global_error(*compiler.get_context(), "%s for flag -%s", missing_error_msg, flag_name)
        .end_error(acorn::ErrCode::GlobalMissingArgumentForFlag);
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
                                                           const Callback& callback,
                                                           bool only_starts_with) {
    flags.push_back({
        .flag_name = flag_name,
        .aliases = std::move(aliases),
        .callback = callback,
        .only_starts_with = only_starts_with
    });
    return flags.back();
}

int CommandLineProcessor::process(llvm::StringRef flag_name, int idx) {

    for (Flag& flag : flags) {

        if (flag.only_starts_with) {
            if (!flag_name.starts_with(flag.flag_name) &&
                std::ranges::find_if(flag.aliases, [flag_name](llvm::StringRef alias) {
                    return flag_name.starts_with(alias);
                }) == flag.aliases.end()) {
                continue;
            }
        } else if (!(flag.flag_name == flag_name ||
            std::ranges::find(flag.aliases, flag_name) != flag.aliases.end())
            ) {
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
