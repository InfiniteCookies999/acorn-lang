#include "CodegenTests.h"

#include "TestFramework.h"

#include <iostream>
#include <llvm/IR/Module.h>

#include "LexerTests.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Acorn.h"
#include "Process.h"

using namespace acorn;

#define src(x) TEST_SOURCE_DIR x

static auto run_codegen_test(const wchar_t* file) {
    
    AcornLang::SourceVector sources;
    sources.push_back(file);

    PageAllocator allocator(get_system_page_size());
    bool has_errors = false;
    Context* context;

    AcornLang* acorn = mock_acorn_instance(allocator);
    acorn->run(sources);
    context = acorn->get_context();
    has_errors = acorn->has_errors();
    delete acorn;

    using acorn::Context;
    context->~Context();
    allocator.dealloc_all();

    if (!has_errors) {
        std::string result;
        int exit_code;
        std::wstring program_name{ L"program" };
        exe_hidden_process(program_name.data(), result, exit_code);
        if (!exit_code) {
            return std::make_tuple(true, result);
        }
    }
    // Either a compilation failure of runtime error from the test executable.
    return std::make_tuple(false, std::string(""));
}

void test_codegen() {
    section("codegen", [&] {
        test("print test", [&] {
            auto [success, result] = run_codegen_test(src(L"main.ac"));
            expect(result, std::identity()).to_be("hello yuki ^-^");
        });
    });
}
