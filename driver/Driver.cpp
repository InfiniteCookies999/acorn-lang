
#include <codecvt>

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Compiler.h"
#include "Util.h"

#include "CommandProcessor.h"

const char* HelpMessage =
R"(
Usage: acorn <options> <sources>

Common options (for more options use -more-options):
 
    -o <name>
        Sets the name of the executable.

    -more-options
        Show more options that can be ran with acorn.

Sources:
    Sources may either be a path to a file or a directory
    in which case all .acorn files within the directory and
    sub-directories will be included.

Examples:
    acorn .        # Compile files in the current directory
    acorn dir      # Compile all the filder under dir
    acorn main.ac  # Compile the file main.ac
)";

const char* ExtendedHelpMessage =
R"(
Usage: acorn <options> <sources>
Options:

    -o, -out-name, -output-name=<name>
        Sets the name of the executable.
    
    -d, -directory, -out-directory, -output-directory=<dir>
        Sets the directory to place the output files such as
        the executable. Will create the director(ies) if they
        do not exist.

    -run
        Runs the program after it is compiled and linked.

    -run-seperate, -run-seperate-window
        Runs the program after it is compiled and linked within
        a seperate terminal (Only supported on windows currently).

    -r, -rel, -release
        Compile in release mode.

    -show-times
        Display how long different stages took.

    -show-llvm-ir
        Displays the LLVM IR generated from source code.
        !! Warning !! This can generate a huge amount of
        output depending on what is compiled.

     -show-error-codes
        When an error occures it also displays the error code.

     -nshow-wrote-to-msg
        Stops showing the message about where the compiled
        program was written to.

     -L<library path>
        Add a search path for libraries.

     -l<library name>
        Add a library.

     -show-linker-command, -show-linker-cmd
        Shows the command that is ran to perform linking.

    -max-errors=<count>
        Sets how many errors will be encountered before
        giving up and exiting compilation.


)";
#include <iostream>

int main(int argc, char* argv[]) {

    acorn::PageAllocator allocator(acorn::get_system_page_size());
    acorn::Compiler compiler(allocator);

    CommandLineProcessor processor(compiler, argc, argv);
    
    processor.add_flag("release", { "r", "rel" }, &acorn::Compiler::set_released_build);
    processor.add_flag("show-times", &acorn::Compiler::set_should_show_times);
    processor.add_flag("show-llvm-ir", &acorn::Compiler::set_should_show_llvm_ir);
    processor.add_flag("show-error-codes", &acorn::Compiler::set_should_show_error_codes);
    processor.add_flag("nshow-wrote-to-msg", &acorn::Compiler::set_dont_show_wrote_to_msg);
    processor.add_flag("run", &acorn::Compiler::set_run_program);
    processor.add_flag("run-seperate", { "run-seperate-window"}, &acorn::Compiler::set_run_program_seperate_window);
    processor.add_flag("show-linker-command", { "show-linker-cmd" }, &acorn::Compiler::set_show_linker_command);
    
    processor.add_flag("output-name", { "out-name", "o" }, [](CommandConsumer& consumer) {
        consumer.next_eql_pair(&acorn::Compiler::set_output_name, "Missing output program name");
    }, true);
    processor.add_flag("output-directory", { "out-directory", "d", "directory" }, [](CommandConsumer& consumer) {
        consumer.next_eql_pair(&acorn::Compiler::set_output_directory, "Missing directory");
    }, true);

    processor.add_flag("max-errors", [&compiler](CommandConsumer& consumer) {
        consumer.next_eql_pair("Missing max errors")
            .parse_int([&compiler](int max_errors) { compiler.set_max_error_count(max_errors); });
    }, true);

    processor.add_flag("L", [&compiler](CommandConsumer& consumer) {
        std::wstring value = consumer.get_flag_name().substr(1);
        if (!value.empty()) {
            compiler.add_library_path(value);
            return;
        }

        consumer.next(&acorn::Compiler::add_library_path, "Missing library path");
    }, true);
    processor.add_flag("l", [&compiler](CommandConsumer& consumer) {    
        std::wstring value = consumer.get_flag_name().substr(1);
        if (!value.empty()) {
            compiler.add_library(value);
            return;
        }

        consumer.next(&acorn::Compiler::add_library, "Missing library");
    }, true);

    std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
    acorn::Compiler::SourceVector sources;
    for (int i = 1; i < argc;) {
        if (argv[i][0] == '-') {
            auto flag_name = llvm::StringRef(argv[i] + 1);
            if (flag_name == "help" || flag_name == "-help") {
                llvm::outs() << HelpMessage;
                return 0;
            }
            if (flag_name == "more-options") {
                llvm::outs() << ExtendedHelpMessage;
                return 0;
            }

            i = processor.process(flag_name, i);

        } else {
            sources.push_back(acorn::Source{ wconverter.from_bytes(argv[i]), "" });
            ++i;
        }
    }

    if (compiler.has_errors()) {
        return 1;
    }

    return compiler.run(sources);
}