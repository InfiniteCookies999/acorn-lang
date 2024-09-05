#include "CodegenTests.h"

#include "TestFramework.h"

#if WIN_OS
#include <Windows.h>
#elif MAC_OS

#elif UNIX_OS

#endif

#include <iostream>
#include <filesystem>
#include <llvm/IR/Module.h>

#include "LexerTests.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Acorn.h"
#include "Process.h"

using namespace acorn;

#define src(x) TEST_SOURCE_DIR x

static std::wstring executable_path;

std::wstring get_executable_path() {
#if WIN_OS
    return L"";
#elif MAC_OS
    char buffer[PATH_MAX + 1];
    uint32_t length = PATH_MAX;
    if(!_NSGetExecutablePath(buf, &length)) {
        buffer[length] = '\0';
        std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
        return converter.from_bytes(buffer);
    }
    return L"";
#elif UNIX_OS
    char buffer[PATH_MAX + 1];
    ssize_t length = readlink("/proc/self/exe", buffer, PATH_MAX);
    if (length != -1) {
        buffer[length] = '\0';
        std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
        return converter.from_bytes(buffer);
    }
    return L"";
#endif
}

static auto run_codegen_test(const wchar_t* file) {
    
    AcornLang::SourceVector sources;
    sources.push_back(file);

    PageAllocator allocator(get_system_page_size());
    bool has_errors = false;
    Context* context;

    AcornLang* acorn = mock_acorn_instance(allocator);
    if (!executable_path.empty()) {
        // We will use the executable directory for the program executable
        // created because if the user tries running the tests from a different
        // directory than the tests directory it makes sense that the executable
        // still ends up in the tests directory.
        acorn->set_output_directory(executable_path);
    }
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
        
#if WIN_OS
    std::wstring program_path{ L"program" };
#elif UNIX_OS
    std::wstring program_path{ L"./program" };
#endif

        if (!executable_path.empty()) {
#if WIN_OS
            program_path =executable_path + L"\\" + program_path;
#elif UNIX_OS
            program_path =executable_path + L"/" + program_path;
#endif
        }

        exe_hidden_process(program_path.data(), nullptr, result, exit_code);
        if (!exit_code) {
            return std::make_tuple(true, result);
        }
    }
    // Either a compilation failure of runtime error from the test executable.
    return std::make_tuple(false, std::string(""));
}

void test_codegen() {

    executable_path = get_executable_path();
    if (!executable_path.empty()) {
#if WIN_OS
        executable_path = executable_path.substr(0, executable_path.find_last_of('\\'));
#elif UNIX_OS
        executable_path = executable_path.substr(0, executable_path.find_last_of('/'));
#endif
    }

    section("codegen", [&] {
        test("print test", [&] {
            auto [success, result] = run_codegen_test(src(L"main.ac"));
            expect(result, std::identity()).to_be("hello yuki ^-^");
        });
    });
}
