
#include <codecvt>

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Acorn.h"
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

    -o, -out-name, -output-name <name>
        Sets the name of the executable.
    
    -d, -directory, -out-directory, -output-directory <dir>
        Sets the directory to place the output files such as
        the executable. Will create the director(ies) if they
        do not exist.

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

)";

int main(int argc, char* argv[]) {

    acorn::PageAllocator allocator(acorn::get_system_page_size());
    acorn::AcornLang acorn(allocator);

    CommandLineProcessor processor(acorn, argc);
    
    std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
    processor.add_flag("release", { "r", "rel" }, &acorn::AcornLang::set_released_build);
    processor.add_flag("show-times", &acorn::AcornLang::set_should_show_times);
    processor.add_flag("show-llvm-ir", &acorn::AcornLang::set_should_show_llvm_ir);
    processor.add_flag("show-error-codes", &acorn::AcornLang::set_should_show_error_codes);
    processor.add_flag("nshow-wrote-to-msg", &acorn::AcornLang::set_dont_show_wrote_to_msg);
    processor.add_flag("output-name", { "out-name", "o" }, [&acorn, &wconverter](char* rest[]) {
        acorn.set_output_name(wconverter.from_bytes(rest[0]));
    }).req_value("Missing output program name");
    processor.add_flag("output-directory", { "out-directory", "d", "directory" }, [&acorn, &wconverter](char* rest[]) {
        acorn.set_output_directory(wconverter.from_bytes(rest[0]));
    }).req_value("Missing directory");


    acorn::AcornLang::SourceVector sources;
    for (int i = 1; i < argc; ++i) {
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

            if (processor.process(flag_name, argv + i + 1, i)) {
                ++i;
            }
        } else {
            sources.push_back(acorn::Source{ wconverter.from_bytes(argv[i]), "" });
        }
    }

    if (acorn.has_errors()) {
        return 1;
    }

    acorn.run(sources);

}