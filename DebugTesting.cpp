
#include <codecvt>

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Acorn.h"

int main(int argc, char* argv[]) {
    
    acorn::PageAllocator allocator(acorn::get_system_page_size());
    acorn::AcornLang acorn(allocator);
    acorn.set_should_show_llvm_ir();
    acorn.set_should_show_error_codes();
    acorn.set_stand_alone();
    
    acorn::AcornLang::SourceVector sources;
    sources.push_back({ L"main.ac", "" });

    if (acorn.has_errors()) {
        return 1;
    }

    acorn.run(sources);

}