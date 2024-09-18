
#include <codecvt>

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Acorn.h"

#include "CommandProcessor.h"

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
            if (processor.process(argv[i] + 1, argv + i + 1, i)) {
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