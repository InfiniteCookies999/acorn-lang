#include "SemaTests.h"

// Include parser first because it has a function named expect.
#include "Parser.h"

#include "TestFramework.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Sema.h"
#include "SourceFile.h"

using namespace acorn;

static llvm::LLVMContext ll_context;
static llvm::Module* ll_test_model = new llvm::Module("Test Module", ll_context);

static PageAllocator allocator(4096);

void test_sema() {

    auto mock_sema = [&](const char* program) {
        Buffer buffer = {
            .content = const_cast<char*>(program),
            .length  = strlen(program)
        };

        Context* context = allocator.alloc_type<Context>();
        new (context) Context(ll_context, *ll_test_model, allocator);
        context->set_stand_alone();

        acorn::Identifier::clear_cache();
        Module* mock_modl = new Module();
        context->get_modules().insert({ Identifier::get("sema_modl"), mock_modl});
        SourceFile* mock_file = new SourceFile(*context, L"", L"", buffer, *mock_modl);
        mock_modl->add_source_file(mock_file);
        mock_logger(mock_file->logger);
        Parser* parser = new Parser(*context, *mock_modl, mock_file);
        parser->parse();

        for (auto& entry : context->get_modules()) {
            Sema::check_nodes_wrong_scopes(*entry.second);
        }
        if (context->has_errors()) {
            return mock_modl;
        }

        if (context->has_errors()) {
            return mock_modl;
        }

        for (auto& entry : context->get_modules()) {
            for (auto source_file : entry.second->get_source_files()) {
                Sema::resolve_imports(*context, source_file);
            }
        }

        for (auto& entry : context->get_modules()) {
            for (auto source_file : entry.second->get_source_files()) {
                auto nspace = source_file->get_namespace();
                if (nspace->have_duplicates_been_checked()) {
                    continue;
                }
                Sema::check_for_duplicate_functions(nspace, *context);
                Sema::check_for_duplicate_functions(source_file, *context);
            }
            Sema::check_all_other_duplicates(*entry.second, *context);
        }

        if (context->has_errors()) {
            return mock_modl;
        }

        for (Decl* decl : context->get_unchecked()) {
            Sema sema(*context, mock_file, mock_file->logger);
            if (decl->is(NodeKind::Func)) {
                sema.check_function(static_cast<Func*>(decl));
            } else if (decl->is(NodeKind::Var)) {
                sema.check_variable(static_cast<Var*>(decl));
            }
        }

        return mock_modl;
    };

    section("sema", [&] {
        test("No find param, named args", [&] {
            mock_sema(R"(
                void foo(int a) {}

                void main() {
                    foo(z = 4);
                }
            )");

            expect_none().to_produce_error(ErrCode::SemaInvalidFuncCallSingle);
        });
        test("Params no order, named args 1", [&] {
            mock_sema(R"(
                void foo(int a, int b) {}

                void main() {
                    foo(b = 4, 2);
                }
            )");

            expect_none().to_produce_error(ErrCode::SemaInvalidFuncCallSingle);
        });
        test("Params no order, named args 2", [&] {
            mock_sema(R"(
                void foo(int a, int b, int c) {}

                void main() {
                    foo(b = 3, a = 6, 66);
                }
            )");

            expect_none().to_produce_error(ErrCode::SemaInvalidFuncCallSingle);
        });
        test("Assign det arr expected arr assign", [&] {
            mock_sema(R"(int[] a = 4;)");
            expect_none().to_produce_error(ErrCode::SemaAssignDetArrTypeReqsArrAssignment);
        });
        test("Assign det arr wrong dimensions", [&] {
            mock_sema(R"(int[][] a = [4];)");
            expect_none().to_produce_error(ErrCode::SemaAssignDetArrWrongDimensions);
        });
        test("Assign det arr wrong dimensions 2", [&] {
            mock_sema(R"(int[] a = [[4]];)");
            expect_none().to_produce_error(ErrCode::SemaIncompatibleArrayElmTypes);
        });
        test("Duplicate parameter", [&] {
            mock_sema(R"(void foo(int a, int b, int a) {})");
            expect_none().to_produce_error(ErrCode::SemaDuplicateParamVariableDecl);
        });
        test("Circular function declaration", [&] {
            mock_sema(R"(
                int q = foo();

                void foo(int a = 44 + q) {}
            )");
            expect_none().to_produce_error(ErrCode::SemaCircularFuncDeclDependency);
        });
        test("Circular global declaration", [&] {
             mock_sema(R"(
                int a = b;
                int b = c;
                int c = a;
            )");
            expect_none().to_produce_error(ErrCode::SemaGlobalCircularDependency);
        });
        test("Const struct field assign", [&] {
            mock_sema(R"(
                struct A {
                    int q;
                }

                void main() {
                    const A a = A{9};
                    a.q = 8;
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaReassignConstAddress);
        });
        test("Const struct field assign nested", [&] {
            mock_sema(R"(
                struct B {
                    int q;
                }

                struct A {
                    B b;
                }

                void main() {
                    const A a = A{ B{9} };
                    a.b.q = 8;
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaReassignConstAddress);
        });
        test("Function call duplicate named arg", [&] {
            mock_sema(R"(
                void foo(int a, int b) {

                }

                void main() {
                    foo(a=44, b=44, a=77);
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaDuplicatedNamedCallArg);
        });
        test("Struct init duplicate named val", [&] {
            mock_sema(R"(
                struct A {
                    int v1;
                    int v2;
                }

                void main() {
                    A a = A{
                        v1=55,
                        v2=66,
                        v1=88
                    };
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaDuplicatedNamedStructInitVal);
        });
        test("Const lossness in array to pointer", [&] {
            mock_sema(R"(
                void main() {
                    const int[5] a = [];
                    int* p = a;
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaVariableTypeMismatch);
        });
        test("Const lossness in multi array to pointer", [&] {
            mock_sema(R"(
                void main() {
                    const int[5][5] a = [];
                    int[5]* p = a;
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaVariableTypeMismatch);
        });
        test("multi array to pointer, pointer has wrong element type", [&] {
            mock_sema(R"(
                void main() {
                    const int[5][5] a = [];
                    const int* p = a;
                }
            )");
            expect_none().to_produce_error(ErrCode::SemaVariableTypeMismatch);
        });
    });
}