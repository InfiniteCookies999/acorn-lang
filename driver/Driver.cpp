
#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Acorn.h"

#include <iostream>

int main() {
    
    acorn::PageAllocator allocator(acorn::get_system_page_size());
    acorn::AcornLang acorn(allocator);

    acorn::AcornLang::SourceVector sources;
    sources.push_back(L"main.ac");
    acorn.set_should_show_llvm_ir();
    acorn.run(sources);

}