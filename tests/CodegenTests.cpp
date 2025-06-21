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
#include "ProcessExec.h"
#include "Util.h"
#include "ir/IRGen.h"

using namespace acorn;

#define src(x) TEST_SOURCE_DIR x

static std::string executable_path;

static std::tuple<std::string, std::string> run_codegen_test(const char* file, bool use_mock_lib = false) {

    Compiler::SourceVector sources;
    sources.push_back(Source{ acorn::SystemPath(file), "" });

    if (!std::filesystem::exists(file)) {
        return { std::format("Test path: \"{}\" does not exist.", file), "" };
    }

    PageAllocator allocator(get_system_page_size());
    bool has_errors = false;
    Context* context;

#if WIN_OS
    std::string test_executable_directory = executable_path + "\\thread" + std::to_string(thread_id + 1);
#elif UNIX_OS
    std::string test_executable_directory = executable_path + "/thread" + std::to_string(thread_id + 1);
#endif

    Compiler* compiler = mock_compiler_instance(allocator);
    // We will use the executable directory for the program executable
    // created because if the user tries running the tests from a different
    // directory than the tests directory it makes sense that the executable
    // still ends up in the tests directory.
    compiler->set_output_directory(SystemPath(test_executable_directory));
    //compiler->set_should_show_llvm_ir();

    compiler->set_dont_show_wrote_to_msg();
    if (!use_mock_lib) {
        compiler->set_stand_alone();
    } else {
        const char* lib_path = src("mock_std_lib");
        compiler->set_standard_library_path(lib_path);
    }

    compiler->run(sources);
    context = compiler->get_context();
    has_errors = compiler->has_errors();
    delete compiler;

    using acorn::Context;
    context->~Context();
    // TODO: Cannot deallocate the memory because the identifiers
    //       reference existing memory. If memory becomes a concern
    //       then we can tell the compiler to allocate the file buffer
    //       memory seperately.
    // allocator.dealloc_all();

    //if (has_errors) {
    //    return { "has errors", "" };
    //}

    std::string result;
    int exit_code;

#if WIN_OS
std::string program_path{ "program" };
#elif UNIX_OS
std::string program_path{ "./program" };
#endif

#if WIN_OS
    program_path = test_executable_directory + "\\" + program_path;
#elif UNIX_OS
    program_path = test_executable_directory + "/" + program_path;
#endif

    exe_hidden_process(program_path.data(), nullptr, result, exit_code);
    if (exit_code) {
        return { std::format("Processed exited with code {}", exit_code), result };
    }

    return { "", result };
}

static void misc_tests() {
    test("print test", [&] {
        auto [err_msg, result] = run_codegen_test(src("print_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hello yuki ^-^");
    });
    test("arithmetic test", [&] {
        auto [err_msg, result] = run_codegen_test(src("arithmetic_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("EjZQ;-C7 H$");
    });
    test("ptr arithmetic test", [&] {
        auto [err_msg, result] = run_codegen_test(src("ptr_arithmetic_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("he!he!heleh");
    });
    test("ptr dereferencing", [&] {
        auto [err_msg, result] = run_codegen_test(src("ptr_dereferencing.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hohho");
    });
    test("Default params all default", [&] {
        auto [err_msg, result] = run_codegen_test(src("default_param_values/default_param_values1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("abcibcijcijkijkajcabkajk");
    });
    test("Default params some default", [&] {
        auto [err_msg, result] = run_codegen_test(src("default_param_values/default_param_values2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("abcaicaijabcaijaijaicabj");
    });
    test("Aggr type zero initialize", [&] {
        auto [err_msg, result] = run_codegen_test(src("aggr_ty_zero_init_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDABCD");
    });
    test("Ignore aggr return value", [&] {
        auto [err_msg, result] = run_codegen_test(src("ignore_aggr_ret_val_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("");
    });
    test("Ptr array-like memory access", [&] {
        auto [err_msg, result] = run_codegen_test(src("ptr_memory_access_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("potato");
    });
    test("Sizeof gets size", [&] {
        auto [err_msg, result] = run_codegen_test(src("sizeof_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Addressing foldable ident ref", [&] {
        auto [err_msg, result] = run_codegen_test(src("address_foldable_ident_ref_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@@@");
    });
    test("Returning value for void ret single", [&] {
        auto [err_msg, result] = run_codegen_test(src("ret_value_for_void_ret_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Returning value for void ret multiple", [&] {
        auto [err_msg, result] = run_codegen_test(src("ret_value_for_void_ret_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called1called2");
    });
}

static void global_variable_tests() {
    test("global foldable value", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Hello from global!");
    });
    test("global foldable value", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("H");
    });
    test("global array fold", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("global array nofold", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("global struct no init empty struct", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("global struct no init non-empty foldable", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("global struct no init non-empty not foldable", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("global struct no init nested struct fields foldable", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("global struct no init nested struct fields not foldable", [&] {
        auto [err_msg, result] = run_codegen_test(src("globals/global_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
}

static void if_tests() {
    test("if deduces true and false", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True Case");
    });
    test("if with else", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True CaseElse Case");
    });
    test("if with elif", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True CaseElif Case");
    });
    test("if with elif and else", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True CaseElif CaseElse Case");
    });
    test("if with pointers", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Ptr Not Null Case1Ptr Not Null Case2Ptr Not Null Case3");
    });
    test("if with var assign", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True Case");
    });
    test("if with var assign const type", [&] {
        auto [err_msg, result] = run_codegen_test(src("ifs/if_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True Case");
    });
}

static void function_call_named_args_tests() {
    test("named args ordered", [&] {
        auto [err_msg, result] = run_codegen_test(src("named_args/named_args_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("named args not ordered", [&] {
        auto [err_msg, result] = run_codegen_test(src("named_args/named_args_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("named args with non-named", [&] {
        auto [err_msg, result] = run_codegen_test(src("named_args/named_args_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
}

static void array_tests() {
    test("Array access fold", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array access nofold", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Multi array access fold", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Multi array access nofold", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array no parse calc length", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array func pass", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_decay_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Multi array func pass", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_decay_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array func pass twice", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_decay_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Multi array func pass twice", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_decay_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array bg type ret (aggr var)", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array bg val rets (aggr var)", [&] {
        // This case is effectively the same as above except it ensures that even when
        // there are multiple returns it can still use the address of the variable.
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array bg val multi rets", [&] {
        // Similar to above but because the return references different arrays
        // it cannot simply use the variable's address as the array and must
        // copy memory to the return address.
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!Lets go!");
    });
    test("Array sm val ret", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hi");
    });
    test("Memory access inline array", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_inline_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Q");
    });
    test("Func pass inline array", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_inline_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array bg ret inline", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_inline_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array sm ret inline", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_inline_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hi");
    });
    test("Array sm ret func cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hi");
    });
    test("Array bg ret func cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array length", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_length_test.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("&");
    });
    test("Array assign one dim", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_assignment1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assign two dim", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_assignment2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array 0 fill remain one dim fold", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array 0 fill remain one dim nofold", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test13.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array 0 fill remain inner arr", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test14.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array 0 fill remain inner arr & outer", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test15.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array mixed inner lengths", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test16.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array mixed inner lengths 2", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test17.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array to pointer", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test18.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array ptr arith", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test19.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assigned indexes", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test20.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assigned index skip first 2", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test21.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assigned index skip surounded values", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test22.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assigned index skip last", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test23.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assigned determines length", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test24.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array assigned determines length 2 dims", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test25.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array sm return inline memory access", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test26.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Array empty init", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test27.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("Array empty init 2-dim", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test28.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array pass to function as ptr then memory access", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test29.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Array call one func with ret of other func sm ret", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test31.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Array call one func with ret of other func bg ret", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test32.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Array assign determined array applies base type to elements", [&] {
        auto [err_msg, result] = run_codegen_test(src("arrays/arrays_test33.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
}

static void loop_tests() {
    test("Predicate loop", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Range loop init, cond, and inc", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Range loop cond and inc", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Range loop init and inc", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Range loop init and cond", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Iterator loop iter over arr", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ramen");
    });
    test("Iterator loop iter over foldable arr", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ramen");
    });
    test("Iterator loop iter over arr decayed", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ramen");
    });
    test("Iterator loop iter and assign", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("HHHHH");
    });
    test("Infinite loop", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Iterator loop range equa", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("012345");
    });
    test("Iterator loop range less than", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("01234");
    });
    test("Predicate loop break statement", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_control_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Range loop break statement", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_control_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("aaaaa");
    });
    test("Iterator loop break statement", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_control_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ramen");
    });
    test("Predicate loop continue statement", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_control_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("babaa");
    });
    test("Range loop continue statement", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_control_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("babaa");
    });
    test("Iterator loop continue statement", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_control_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("FaBen");
    });
    test("Iterator loop over array of arrays", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test13.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Pizza!");
    });
    test("Iterator loop over array of structs", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test14.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Pizza!");
    });
    test("Iterator loop over array of structs with copy constructors", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test15.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Pizza!");
    });
    test("Iterator loop over slice of integers", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test16.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ramen");
    });
    test("Iterator loop over slice of integers by ptr", [&] {
        auto [err_msg, result] = run_codegen_test(src("loops/loops_test17.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@@@@@");
    });
}

static void logical_bin_ops_tests() {
    test("Logical and bin op", [&] {
        auto [err_msg, result] = run_codegen_test(src("logical_bin_ops/logical_bin_ops1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True");
    });
    test("Logical or bin op", [&] {
        auto [err_msg, result] = run_codegen_test(src("logical_bin_ops/logical_bin_ops2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True1True2True3True4");
    });
    test("Logical and bin op with var", [&] {
        auto [err_msg, result] = run_codegen_test(src("logical_bin_ops/logical_bin_ops3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True");
    });
    test("Logical or bin op with var", [&] {
        auto [err_msg, result] = run_codegen_test(src("logical_bin_ops/logical_bin_ops4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("True1True2True3True4");
    });
}

static void switch_tests() {
    test("Switch takes value case", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("case 5");
    });
    test("Switch takes default case", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("default case");
    });
    test("Switch non foldable case", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("case > 5");
    });
    test("Switch joined cases fall through", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("case 4 & 5");
    });
    test("Switch joined cases fall through last", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("case 4 & 5");
    });
    test("Switch joined with default fallthrough", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("default case");
    });
    test("Switch non foldable joined cases fall through", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("case 4 & 5");
    });
    test("Switch default only", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("default case");
    });
    test("Switch with range captures lowest value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with range captures middle value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with range captures end value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with range lt not captures end value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("end");
    });
    test("Switch with range (bg range no-fold) captures lowest value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches13.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with range (bg range no-fold) captures middle value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches14.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with range (bg range no-fold) captures end value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches15.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with range (bg range no-fold) lt not captures end value", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches16.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("range case");
    });
    test("Switch with enum range eq", [&] {
        auto [err_msg, result] = run_codegen_test(src("switches/switches17.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("case 2case 1case 1case 1case 4");
    });
}

static void function_type_calls_tests() {
    test("Call function type no args", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called callback!");
    });
    test("Call function type with args", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("Call function type returns value", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("msg from callback!");
    });
    test("Call function type from func param pass", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called callback!");
    });
    test("Call function type returns aggregate", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Function call fixes up type", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Call function type for member function", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Call function type for member function with bg aggregate ret", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Call function type with struct (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Call function type with struct (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("func_type_calls/func_type_calls10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
}

static void struct_tests() {
    test("Struct assign and access fields", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("I Love Coffee!");
    });
    test("Struct bg type ret (aggr var)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ijk");
    });
    test("Struct bg type rets (aggr var)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ijk");
    });
    test("Struct sm ret va", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("i");
    });
    test("Struct initializer, initialize values", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("Struct initializer, initialize some values", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("Struct initialize field values", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("Struct initializer init field values non-assigned", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("iBCijCijk");
    });
    test("Struct arr initialize field values", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCABCABCABC");
    });
    test("Struct arr initialize field values 2-dim", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCABCABCABCABCABC");
    });
    test("Struct bg lvalue passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct bg struct initializer passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct bg struct from call passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs13.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct sm lvalue passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs14.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Struct sm struct initializer passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs15.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Struct sm struct from call passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs16.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Struct sm return inline field access", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs17.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("A");
    });
    test("Struct named vals all fields set by names", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs18.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABC");
    });
    test("Struct named vals some fields set by names", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs19.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct named vals mixed named+not named all set", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs20.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct named vals mixed named+not named some set", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs21.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct named vals mixed named+not named some set + gap", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs22.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct passed as param and returned (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs23.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Struct passed as param and returned (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs24.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct ret multiple loc vars (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs25.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Struct ret multiple loc vars (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs26.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDabcd");
    });
    test("Struct ret struct call tmp struct mem func inline (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs27.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Struct ret struct call tmp struct mem func inline (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs28.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct ret struct func type call inline field access (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs29.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Struct ret struct func type call inline field access (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs30.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Struct with const and non-const func chooses non-const for non-const obj", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs31.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("no const");
    });
    test("Struct with const and non-const func chooses const for const obj", [&] {
        auto [err_msg, result] = run_codegen_test(src("structs/structs32.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("const");
    });
}

static void member_function_tests() {
    test("Member function sets field", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("W");
    });
    test("Member function returns bg aggr type", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Member function returns sm aggr type", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Member functions sets field with this ptr", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("W");
    });
    test("Member functions returns pointer to self and inline field access", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("W");
    });
    test("Member function calls another member function", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("W");
    });
    test("Member function calls another member function with this ptr", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("W");
    });
    test("Member function auto-dereference struct ptr cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("A");
    });
    test("Member function returns pointer to 'this' with repeated calls", [&] {
        auto [err_msg, result] = run_codegen_test(src("member_functions/member_functions9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
}

static void multi_variables_on_one_line_tests() {
    test("Multi-var assign 2 variables", [&] {
        auto [err_msg, result] = run_codegen_test(src("multiline_vars/multiline_vars_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Multi-var no 4 variables", [&] {
        auto [err_msg, result] = run_codegen_test(src("multiline_vars/multiline_vars_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABAB");
    });
    test("Multi-var default assign", [&] {
        auto [err_msg, result] = run_codegen_test(src("multiline_vars/multiline_vars_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Multi-var struct type variables", [&] {
        auto [err_msg, result] = run_codegen_test(src("multiline_vars/multiline_vars_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Multi-var global vars", [&] {
        auto [err_msg, result] = run_codegen_test(src("multiline_vars/multiline_vars_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Multi-var struct fields", [&] {
        auto [err_msg, result] = run_codegen_test(src("multiline_vars/multiline_vars_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
}

static void constructors_tests() {
    test("Call constructor with arg", [&] {
        auto [err_msg, result] = run_codegen_test(src("constructors/constructors_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Default constructor called automatically", [&] {
        auto [err_msg, result] = run_codegen_test(src("constructors/constructors_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Default constructor called automatically (for arrays)", [&] {
        auto [err_msg, result] = run_codegen_test(src("constructors/constructors_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABABAB");
    });
    test("Default constructor called automatically init fields", [&] {
        auto [err_msg, result] = run_codegen_test(src("constructors/constructors_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Default constructor called automatically init fields (for arrays)", [&] {
        auto [err_msg, result] = run_codegen_test(src("constructors/constructors_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABABAB");
    });
}

static void destructors_tests() {
    test("Destructor called in main scope no ret encountered", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor called only statement reached", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor called only statement reached but returned call all reached", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledB calledA calledC calledA ");
    });
    test("Destructor called reverse order as declared", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledB calledA ");
    });
    test("Destructor called inner scope", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledA | calledB ");
    });
    test("Destructor called once for ret var", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("begin called");
    });
    test("Destructor called once for ret var (ret ignored sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called once for ret var (ret ignored bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called once for ret var of call var type (ret ignored sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called once for ret var of call var type (ret ignored bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called for inline struct init mem func call (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called for inline struct init mem func call (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructors called for array", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test13.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledcalledcalled");
    });
    test("Destructors called for global variable", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test14.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("begin called");
    });
    test("Destructor called for inline struct init mem field access (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test15.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called for inline struct init mem field access (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test16.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called end");
    });
    test("Destructor called for each loop iteration", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test17.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called called called called called ");
    });
    test("Destructor called for break", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test18.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledB calledA ");
    });
    test("Destructor called when reassigning from struct init", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test19.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called | called");
    });
    test("Destructor called when reassigning from func cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test20.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called | called");
    });
    test("Destructor called once when return inline struct init (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test21.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor called once when return inline struct init (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test22.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor implicitly calls destructor of field", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test23.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Implicit destructor called", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test24.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor called for param", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test25.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called called ");
    });
    test("Destructors called for field array of destructible types", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test26.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called called called ");
    });
    test("Destructors called for field array of destructible types (implicit destructor)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test27.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called called called ");
    });
    test("Destructor called when reassigning to seperate variable and when both destroyed", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test28.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called1 called2 called2 ");
    });
    test("Destructor called once when assigning to self", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test29.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor called once when assigning to self (assigned through ptrs)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test30.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Destructor reassign with initializer destroys copy and origina", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test31.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("created created before before before ");
    });
    test("Destructor reassign with call destroys copy and origina", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test32.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("created created before before before ");
    });
    test("Destructor temp obj for implicit param ptr", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test33.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hello! called end");
    });
    test("Destructor temp obj from func call for implicit param ptr", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test34.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("hello! called end");
    });
    test("Multiple return destructor called for both paths with local var (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test35.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called@called@calledcalled");
    });
    test("Multiple return destructor called for both paths with local var (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test36.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledABCDcalledABCDcalledcalled");
    });
    test("Inline pass call arg with struct ret only calls destructor at call site (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test37.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@calledend");
    });
    test("Inline pass call arg with struct ret only calls destructor at call site (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test38.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDcalledend");
    });
    test("Inline pass call arg with struct initializer only calls destructor at call site (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test39.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@calledend");
    });
    test("Inline pass call arg with struct initializer only calls destructor at call site (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test40.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDcalledend");
    });
    test("Temporary with member function that returns and assigns to another variable still retains destructor order", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test41.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("B calledD calledC calledA called");
    });
    test("Scope pop retains destructor call order", [&] {
        auto [err_msg, result] = run_codegen_test(src("destructors/destructors_test42.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("C calledB calledA called");
    });
}

static void copy_constructor_tests() {
    test("Copy constructor called on assignment", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Copy constructor called on reassignment", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Copy constructor called on assignment (implicitly)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Copy constructor called on reassignment (implicitly)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Copy constructor not called for local var ret (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("end");
    });
    test("Copy constructor not called for local var ret (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("end");
    });
    test("Copy constructor called dep on which ret for multi-var ret (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called1 called2 ");
    });
    test("Copy constructor called dep on which ret for multi-var ret (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called1 called2 ");
    });
    test("Copy constructor called ret global var (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Copy constructor called ret global var (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Copy constructor called implicitly when has field with move constructor and explitit copy constructor", [&] {
        auto [err_msg, result] = run_codegen_test(src("copy_constructors/copy_constructors_test11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
}

static void auto_type_tests() {
    test("Auto type assign numbers", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@@");
    });
    test("Auto type assign function reference", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("from foo");
    });
    test("Auto type assign for field", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("A");
    });
    test("Auto ptr type assign variable address", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Auto variable for iterator loop", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDE");
    });
    test("Auto ptr variable for iterator loop", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AAAAA");
    });
    test("Const auto assign value", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Auto in iterator range loop", [&] {
        auto [err_msg, result] = run_codegen_test(src("auto_type/auto_type_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDE");
    });
}

static void test_implicit_ptrs() {
    test("Implicit pointer passing integer values", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@ABBB");
    });
    test("Implicit pointer passing struct", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDCDCD");
    });
    test("Implicit pointer passing from func call ret (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Implicit pointer passing from func call ret (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Implicit pointer passing an array", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDABCDABCDABCD");
    });
    test("Implicit pointer passing an array from func call (sm array)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Implicit pointer passing an array from func call (bg array)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/param_implicit_ptrs_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Implicit return pointer stores to integer", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/return_implicit_ptrs_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@@@");
    });
    test("Implicit return pointer stores to struct (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/return_implicit_ptrs_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@@@");
    });
    test("Implicit return pointer stores to struct (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/return_implicit_ptrs_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDABCDABCD");
    });
    test("Implicit return pointer for call arg to integer", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/return_implicit_ptrs_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Implicit return pointer for call arg to struct (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/return_implicit_ptrs_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Implicit return pointer for call arg to struct (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("implicit_ptrs/return_implicit_ptrs_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
}

static void move_constructors_tests() {
    test("Return multiple local variable calls move constructor (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledcalled");
    });
    test("Return multiple local variable calls move constructor (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("calledcalled");
    });
    test("Return multiple local variable and inline return only one calls move constructor (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("!!!called");
    });
    test("Return multiple local variable and inline return only one calls move constructor (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("!!!called");
    });
    test("Single local variable variable does not call move constructor (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("end");
    });
    test("Global multiple return does not call move constructor (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("end");
    });
    test("Global multiple return does not call move constructor (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("end");
    });
    test("Explicit call to moveobj call move constructor for initial assign", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called#@");
    });
    test("Explicit call to moveobj call move constructor for assign op", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called#@");
    });
    test("Call to move constructor when using moveobj on call argument", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called@#");
    });
    test("Move constructor called implicitly when has field with move constructor", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Move constructor called implicitly when has field with move constructor and explitit move constructor", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test13.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("called");
    });
    test("Passing argument with moveobj to constructor", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test14.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@called%@");
    });
    test("Passing argument with moveobj to struct initializer", [&] {
        auto [err_msg, result] = run_codegen_test(src("move_constructors/move_constructors_test15.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@called%@");
    });
}

static void ternaries_tests() {
    test("Ternary of foldable integers", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@#");
    });
    test("Ternary of non-foldable integers", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@#");
    });
    test("Ternary of select inline structs", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Ternary of select non-inline structs", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Returning ternary struct inline (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Returning ternary struct inline (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDEFGH");
    });
    test("Ternary of non-foldable integers with one const type", [&] {
        auto [err_msg, result] = run_codegen_test(src("ternaries/ternaries_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@#");
    });
}

static void enums_tests() {
    test("Enum int values", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Enum uint64 values", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Enum const char* values", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("I love coffeeee1 I love coffeeee2 I love coffeeee3 I love coffeeee4 ");
    });
    test("Enum int values eq and neq", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("eq2neq1");
    });
    test("Enum const char* values eq and neq", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("eq2neq1");
    });
    test("Enum int values add gives container type", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Enums int value auto-increments index", [&] {
        auto [err_msg, result] = run_codegen_test(src("enums/enums_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDEF");
    });
}

static void slices_tests() {
    test("Slice assign variable array to slice", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice assign inline array to slice", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice assign variable array to slice and pass to func", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice asign variable after declare slice", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Global slice assign variable array to slice", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Global slice assign variable array at global scope", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice accessing length field", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@");
    });
    test("Slice return pointer to global array", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice return pointer to array passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice return pointer to array passed to function", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice return pointer to array passed to function multi-return", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test11.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Slice access ptr through ptr field", [&] {
        auto [err_msg, result] = run_codegen_test(src("slices/slices_test12.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
}

static void varargs_tests() {
    test("Varargs pass foldable integers", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Varargs pass non-foldable integers", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Varargs pass foldable arrays", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Varargs pass non-foldable arrays", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Varargs forward", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("Lets go!");
    });
    test("Varargs with other arguments", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@?Lets go!");
    });
    test("Varargs with other named arguments", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@?Lets go!");
    });
    test("Varargs with other named arguments mixed", [&] {
        auto [err_msg, result] = run_codegen_test(src("varargs/varargs_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@?Lets go!");
    });
}

static void interface_tests() {
    test("Call virtual interface func no arguments", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test1.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual interface func no arguments two funcs", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test2.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual interface func pass arguments", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test3.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual interface func no arguments implement two interfaces", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test4.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual interface func overloaded", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test5.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual function two interfaces and two v. functions each", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test6.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCD");
    });
    test("Call virtual functions for array", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test7.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABABABABAB");
    });
    test("Call virtual function struct initialize", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test8.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual function struct called constructors", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test9.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
    test("Call virtual function but non-dynamically", [&] {
        auto [err_msg, result] = run_codegen_test(src("interfaces/interfaces_test10.ac"));
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("AB");
    });
}

static void error_tests() {
    test("Raise #abort error", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors1.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("error raised!");
    });
    test("Raise uncaught error", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors2.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("error raised!");
    });
    test("Raise conditionally raises error or returns value", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors3.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error raised!");
    });
    test("Raise conditionally raises error or returns sm struct", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors4.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error raised!");
    });
    test("Raise conditionally raises error or returns bg struct", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors5.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDerror raised!");
    });
    test("Raise #abort error function returns int", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors6.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("error raised!");
    });
    test("Raise #abort error function returns void", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors7.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("error raised!");
    });
    test("Raise error and catch it", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors8.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("foo called!caught error!test error msg");
    });
    test("Raise errors of different struct sizes", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors9.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("foo called!caught error!test error msg 1foo called!caught error!test error msg 2ABCD");
    });
    test("Raise error assign to variable", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors10.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error caught!");
    });
    test("Raise error assign to variable by assign op", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors11.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error caught!");
    });
    test("Raise error assign to struct variable (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors12.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error caught!");
    });
    test("Raise error assign to struct variable (bg struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors13.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDerror caught!");
    });
    test("Raise error assign to variable and recover", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors14.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@caught!#");
    });
    test("Raise error assign op to variable and recover", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors15.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("caught 1@caught 2^");
    });
    test("Call function that raises error and passes raise along with try", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors16.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("caught error!error raised!error raised!");
    });
    test("Raise #abort error from function type cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors17.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("error raised!");
    });
    test("Raise conditionally raises error or returns value from function type cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors18.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error raised!");
    });
    test("Raise conditionally raises error or returns from function type call (sm struct)", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors19.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@error raised!");
    });
    test("Raise conditionally raises error or returns bg struct", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors20.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("ABCDerror raised!");
    });
    test("Raise error and catch it from function type cal", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors21.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("foo called!caught error!test error msg");
    });
    test("Raise uncaught error when calling interface function", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors22.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("foo called!test error msg");
    });
    test("Raise conditionally raises error when calling interface function", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors23.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@test error msg");
    });
    // ... Destructors
    test("Raise error does not call destructor for assigned object", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors1.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@deconstructor called!...error raised!");
    });
    test("Raise error does not call destructor for assigned object even with catch block", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors2.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("...caught error!deconstructor called!");
    });
    test("Raise error does not call destructor for ignored return object", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors3.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("deconstructor called!no raise!...error raised!");
    });
    test("Raise error does not call destructor for ignored return object even with catch block", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors4.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("deconstructor called!...caught error!");
    });
    test("Raise error does not call destructor for assigned object when raise in catch", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors5.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("@deconstructor called!...caught error!error raised!");
    });
    test("Raise error does not call destructor for temporary object of caught error return in catch", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors6.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("deconstructor called!@...caught error!");
    });
    test("Raise error destroys object up until the current point in expression retaining destructor order and catches error", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors7.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("~A called!@~D called!~B called!~C called!...~A called!caught error!~E called!~C called!");
    });
    test("Raise error destroys object up until the current point in expression and passes error", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors8.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("A deconstructor called!@B deconstructor called!...A deconstructor called!error raised!");
    });
    test("Raise error recover does not continue to destroy temporary is error was raised", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors9.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("~A called!@...caught error!^");
    });
    test("Raise error with destructor calls error's destructor", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors10.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("caught error!destructor called!end!");
    });
    test("Raise error with destructor calls error's destructor when using recover", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors11.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("caught error!destructor called!end!");
    });
    test("Raise errors with destructors calls both error's destructors", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors12.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("caught error!destructor called 1!...caught error!destructor called 2!end!");
    });
    test("Raise errors with one destructor other not called. Only error with destructor called", [&] {
        auto [err_msg, result] = run_codegen_test(src("errors/errors_destructors13.ac"), true);
        if (!err_msg.empty())  force_fail(err_msg.c_str());

        expect(result, std::identity()).to_be("caught error!destructor called 1!...caught error!end!");
    });
}

void test_codegen() {

    std::string err;
    auto exe_path = acorn::get_current_directory_path(err);
    if (!err.empty()) {
        acorn_fatal_fmt("Failed to get executable path. Error: %s", err);
    }
    executable_path = exe_path.to_utf8_string();

    // Creating a fake compiler just to initialize the target machine because
    // the target machine is global memory and we do not want some race condition
    // for it. We cannot just call a static function to initialize it because in
    // normal execution flow of the program it would report an error.
    PageAllocator allocator(get_system_page_size());
    auto compiler = new acorn::Compiler(allocator);
    compiler->pre_initialize_target_machine();
    if (compiler->has_errors()) {
        // Exit before even running the tests we could not initialize
        // the target machine.
        exit(1);
    }
    delete compiler;

    section("codegen", [&] {
        misc_tests();
        global_variable_tests();
        if_tests();
        function_call_named_args_tests();
        array_tests();
        loop_tests();
        logical_bin_ops_tests();
        switch_tests();
        function_type_calls_tests();
        struct_tests();
        member_function_tests();
        multi_variables_on_one_line_tests();
        constructors_tests();
        destructors_tests();
        copy_constructor_tests();
        auto_type_tests();
        test_implicit_ptrs();
        move_constructors_tests();
        ternaries_tests();
        enums_tests();
        slices_tests();
        varargs_tests();
        interface_tests();
        error_tests();
    }, true);
}
