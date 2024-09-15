#include "CodegenTests.h"

#include "TestFramework.h"

#if WIN_OS
#include <Windows.h>
#undef min
#undef max
#elif MAC_OS
#include <mach-o/dyld.h>
#endif

#include <iostream>
#include <filesystem>
#include <codecvt>
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

// TODO: Not deteching when unicode is enabled fix this
// and use W version if unicode.
#if defined(_UNICODE) || defined(UNICODE)
    wchar_t buffer[MAX_PATH];
    DWORD length = GetModuleFileNameW(nullptr, buffer, MAX_PATH);
    if (length != 0) {
        buffer[length] = '\0';
        return std::wstring{ buffer };
    }
    return L"";
#else
    char buffer[MAX_PATH];
    DWORD length = GetModuleFileNameA(nullptr, buffer, MAX_PATH);
    if (length != 0) {
        buffer[length] = '\0';
        std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
        return converter.from_bytes(buffer);
    }
    return L"";
#endif
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
    acorn->set_dont_show_wrote_to_msg();
    // acorn->set_should_show_llvm_ir();

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
            auto [success, result] = run_codegen_test(src(L"print_test.ac"));
            expect(result, std::identity()).to_be("hello yuki ^-^");
        });
        test("arithmetic test", [&] {
            auto [success, result] = run_codegen_test(src(L"arithmetic_test.ac"));
            expect(result, std::identity()).to_be("EjZQ;-C7 H$");
        });
        test("ptr arithmetic test", [&] {
            auto [success, result] = run_codegen_test(src(L"ptr_arithmetic_test.ac"));
            expect(result, std::identity()).to_be("he!he!heleh");
        });
        test("global test 1", [&] {
            auto [success, result] = run_codegen_test(src(L"globals/global_test1.ac"));
            expect(result, std::identity()).to_be("Hello from global!");
        });
        test("ptr dereferencing", [&] {
            auto [success, result] = run_codegen_test(src(L"ptr_dereferencing.ac"));
            expect(result, std::identity()).to_be("hoh");
        });
        test("if deduces true and false", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test1.ac"));
            expect(result, std::identity()).to_be("True Case");
        });
        test("if with else", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test2.ac"));
            expect(result, std::identity()).to_be("True CaseElse Case");
        });
        test("if with elif", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test3.ac"));
            expect(result, std::identity()).to_be("True CaseElif Case");
        });
        test("if with elif and else", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test4.ac"));
            expect(result, std::identity()).to_be("True CaseElif CaseElse Case");
        });
        test("if with pointers", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test5.ac"));
            expect(result, std::identity()).to_be("Ptr Not Null Case1Ptr Not Null Case2Ptr Not Null Case3");
        });
        test("if with var assign", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test6.ac"));
            expect(result, std::identity()).to_be("True Case");
        });
        test("if with var and post cond", [&] {
            auto [success, result] = run_codegen_test(src(L"ifs/if_test7.ac"));
            expect(result, std::identity()).to_be("True Case");
        });
    });
}
