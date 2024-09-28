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

static std::tuple<std::string, std::string> run_codegen_test(const wchar_t* file) {
    
    AcornLang::SourceVector sources;
    sources.push_back(Source{ file, "" });

    if (!std::filesystem::exists(file)) {
        std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
        std::string bad_path = wconverter.to_bytes(file);
        return { std::format("Test path: \"{}\" does not exist.", bad_path), "" };
    }

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
    acorn->set_stand_alone();
    // acorn->set_should_show_llvm_ir();

    acorn->run(sources);
    context = acorn->get_context();
    has_errors = acorn->has_errors();
    delete acorn;

    using acorn::Context;
    context->~Context();
    allocator.dealloc_all();

    if (has_errors) {
        return { "", "" };
    }

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
    if (exit_code) {
        return { std::format("Processed exited with code {}", exit_code), result };
    }
    
    return { "", result };
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
            auto [err_msg, result] = run_codegen_test(src(L"print_test.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());
            
            expect(result, std::identity()).to_be("hello yuki ^-^");
        });
        test("arithmetic test", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arithmetic_test.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("EjZQ;-C7 H$");
        });
        test("ptr arithmetic test", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ptr_arithmetic_test.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("he!he!heleh");
        });
        test("global foldable value", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"globals/global_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Hello from global!");
        });
        test("global foldable value", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"globals/global_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("H");
        });
        test("global array fold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"globals/global_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("global array nofold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"globals/global_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("ptr dereferencing", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ptr_dereferencing.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("hoh");
        });
        test("if deduces true and false", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True Case");
        });
        test("if with else", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True CaseElse Case");
        });
        test("if with elif", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True CaseElif Case");
        });
        test("if with elif and else", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True CaseElif CaseElse Case");
        });
        test("if with pointers", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test5.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Ptr Not Null Case1Ptr Not Null Case2Ptr Not Null Case3");
        });
        test("if with var assign", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test6.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True Case");
        });
        test("if with var and post cond", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test7.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True Case");
        });
        test("if with foldable var", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"ifs/if_test8.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True Case");
        });
        test("named args ordered", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"named_args/named_args_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("named args not ordered", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"named_args/named_args_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("named args with non-named", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"named_args/named_args_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array access fold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array access nofold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Multi array access fold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Multi array access nofold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array no parse calc length", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test5.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array func pass", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_decay_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Multi array func pass", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_decay_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array func pass twice", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_decay_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Multi array func pass twice", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_decay_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array bg type ret (aggr var)", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test6.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array bg val rets (aggr var)", [&] {
            // This case is effectively the same as above except it ensures that even when
            // there are multiple returns it can still use the address of the variable.
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test7.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array bg val multi rets", [&] {
            // Similar to above but because the return references different arrays
            // it cannot simply use the variable's address as the array and must
            // copy memory to the return address.
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test9.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!Lets go!");
        });
        test("Array sm val ret", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test8.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("hi");
        });
        test("Memory access inline array", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_inline_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Q");
        });
        test("Func pass inline array", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_inline_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array bg ret inline", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_inline_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array sm ret inline", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_inline_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("hi");
        });
        test("Array sm ret func call", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test10.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("hi");
        });
        test("Array bg ret func call", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test11.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array length", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_length_test.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("&");
        });
        test("Array assign one dim", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_assignment1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assign two dim", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_assignment2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
    });
}
