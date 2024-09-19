#include "SemaTests.h"

// Include parser first because it has a function named expect.
#include "Parser.h"

#include "TestFramework.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "Sema.h"

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
        
        acorn::Identifier::clear_cache();
        Module* mock_modl = new Module();
        SourceFile* mock_file = new SourceFile(*context, L"", buffer, *mock_modl);
        mock_logger(mock_file->logger);
        Parser* parser = new Parser(*context, *mock_modl, mock_file);
        parser->parse();
        
        for (auto& entry : context->get_modules()) {
            Sema::check_nodes_wrong_scopes(*entry.second);
        }
        if (context->has_errors()) {
            return mock_modl;
        }

        for (auto& entry : context->get_modules()) {
            Sema::resolve_global_comptime(*context, *entry.second);
        }
        if (context->has_errors()) {
            return mock_modl;
        }

        for (auto& entry : context->get_modules()) {
            Sema::resolve_imports(*context, *entry.second);
        }

        for (auto& entry : context->get_modules()) {
            Sema::check_for_duplicate_functions(*entry.second);
            Sema::check_for_duplicate_variables(*entry.second);
        }

        if (context->has_errors()) {
            return mock_modl;
        }

        for (Decl* decl : context->get_unchecked()) {
            Sema sema(*context, *mock_modl, mock_file->logger);
            if (decl->is(NodeKind::Func)) {
                sema.check_function(as<Func*>(decl));
            } else if (decl->is(NodeKind::Var)) {
                sema.check_variable(as<Var*>(decl));
            }
        }

        return mock_modl;
    };

    section("parsing", [&] {
        test("no find param, named args", [&] {
            mock_sema(R"(
                void foo(int a) {}

                void main() {
                    foo(z = 4)
                }
            )");

            expect_none().to_produce_error(ErrCode::SemaInvalidFuncCallSingle);
        });
        test("params no order, named args 1", [&] {
            mock_sema(R"(
                void foo(int a, int b) {}

                void main() {
                    foo(b = 4, 2)
                }
            )");

            expect_none().to_produce_error(ErrCode::SemaInvalidFuncCallSingle);
        });
        test("params no order, named args 2", [&] {
            mock_sema(R"(
                void foo(int a, int b, int c) {}

                void main() {
                    foo(b = 3, a = 6, 66)
                }
            )");

            expect_none().to_produce_error(ErrCode::SemaInvalidFuncCallSingle);
        });
    });
}