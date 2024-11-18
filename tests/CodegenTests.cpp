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
#include "Compiler.h"
#include "Process.h"
#include "Util.h"

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
    
    Compiler::SourceVector sources;
    sources.push_back(Source{ file, "" });

    if (!std::filesystem::exists(file)) {
        std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
        std::string bad_path = wconverter.to_bytes(file);
        return { std::format("Test path: \"{}\" does not exist.", bad_path), "" };
    }

    PageAllocator allocator(get_system_page_size());
    bool has_errors = false;
    Context* context;
    
    Compiler* compiler = mock_compiler_instance(allocator);
    if (!executable_path.empty()) {
        // We will use the executable directory for the program executable
        // created because if the user tries running the tests from a different
        // directory than the tests directory it makes sense that the executable
        // still ends up in the tests directory.
        compiler->set_output_directory(executable_path);
    }
    compiler->set_dont_show_wrote_to_msg();
    compiler->set_stand_alone();
    // acorn->set_should_show_llvm_ir();

    compiler->run(sources);
    context = compiler->get_context();
    has_errors = compiler->has_errors();
    delete compiler;

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
        test("Array 0 fill remain one dim fold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test12.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array 0 fill remain one dim nofold", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test13.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array 0 fill remain inner arr", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test14.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array 0 fill remain inner arr & outer", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test15.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array mixed inner lengths", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test16.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array mixed inner lengths 2", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test17.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array to pointer", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test18.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array ptr arith", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test19.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assigned indexes", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test20.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assigned index skip first 2", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test21.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assigned index skip surounded values", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test22.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assigned index skip last", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test23.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assigned determines length", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test24.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Array assigned determines length 2 dims", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"arrays/arrays_test25.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Predicate loop", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Range loop init, cond, and inc", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Range loop cond and inc", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Range loop init and inc", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Range loop init and cond", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test5.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Iterator loop iter over arr", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test6.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ramen");
        });
        test("Iterator loop iter over foldable arr", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test7.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ramen");
        });
        test("Iterator loop iter over arr decayed", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test8.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ramen");
        });
        test("Iterator loop iter and assign", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test9.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("HHHHH");
        });
        test("Infinite loop", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test10.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Iterator loop range equal", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test11.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("012345");
        });
        test("Iterator loop range less than", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_test12.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("01234");
        });
        test("Predicate loop break statement", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Range loop break statement", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("aaaaa");
        });
        test("Iterator loop break statement", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ramen");
        });
        test("Predicate loop continue statement", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("babaa");
        });
        test("Range loop continue statement", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test5.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("babaa");
        });
        test("Iterator loop continue statement", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test6.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("FaBen");
        });
        test("Loop break statement depth 2", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test7.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("abcde");
        });
        test("Loop control statement depth 2", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"loops/loops_control_test8.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("abbccddeef");
        });
        test("Logical and bin op", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"logical_bin_ops/logical_bin_ops1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True");
        });
        test("Logical or bin op", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"logical_bin_ops/logical_bin_ops2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True1True2True3True4");
        });
        test("Logical and bin op with var", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"logical_bin_ops/logical_bin_ops3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True");
        });
        test("Logical or bin op with var", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"logical_bin_ops/logical_bin_ops4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("True1True2True3True4");
        });
        test("Switch takes value case", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"switches/switches1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("case 5");
        });
        test("Switch takes default case", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"switches/switches2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("default case");
        });
        test("Switch non foldable case", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"switches/switches3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("case > 5");
        });
        test("Call function type no args", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"func_type_calls/func_type_calls1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("called callback!");
        });
        test("Call function type with args", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"func_type_calls/func_type_calls2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ABC");
        });
        test("Call function type returns value", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"func_type_calls/func_type_calls3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("msg from callback!");
        });
        test("Call function type from func param pass", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"func_type_calls/func_type_calls4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("called callback!");
        });
        test("Call function type returns aggregate", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"func_type_calls/func_type_calls5.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Function call fixes up type", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"func_type_calls/func_type_calls6.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("Lets go!");
        });
        test("Struct assign and access fields", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("I Love Coffee!");
        });
        test("Struct bg type ret (aggr var)", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ijk"); 
        });
        test("Struct bg type rets (aggr var)", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs3.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ijk"); 
        });
        test("Struct sm ret val", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs4.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("i"); 
        });
        test("Struct initializer, initialize values", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs5.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ABC");
        });
        test("Struct initializer, initialize some values", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs6.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ABC");
        });
        test("Struct initialize field values", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs7.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ABC"); 
        });
        test("Struct initializer init field values non-assigned", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"structs/structs8.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("iBCijCijk"); 
        });
        test("Default params all default", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"default_param_values/default_param_values1.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("abcibcijcijkijkajcabkajk");
        });
        test("Default params some default", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"default_param_values/default_param_values2.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("abcaicaijabcaijaijaicabj");
        });
        test("Aggr type zero initialize", [&] {
            auto [err_msg, result] = run_codegen_test(src(L"aggr_ty_zero_init_test.ac"));
            if (!err_msg.empty())  force_fail(err_msg.c_str());

            expect(result, std::identity()).to_be("ABCDABCD"); 
        });
    });
}
