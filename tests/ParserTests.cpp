#include "ParserTests.h"

#include <ranges>

// Include parser first because it has a function named expect.
#include "Parser.h"

#include "TestFramework.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"

using namespace acorn;

static llvm::LLVMContext ll_context;
static llvm::Module* ll_test_model = new llvm::Module("Test Module", ll_context);

static PageAllocator allocator(4096);

static Context* context;

std::string node_kind_to_string(NodeKind kind) {
    return std::to_string(static_cast<unsigned>(kind));
}

std::string identifier_to_string(Identifier identifier) {
    return identifier.reduce().str();
}

std::string type_to_string(Type* type) {
    return type->to_string();
}

template<typename K, typename V>
auto get_second(const std::pair<K, V>& pair) -> V {
    return pair.second;
}

void test_parser() {

    context = allocator.alloc_type<Context>();
    new (context) Context(ll_context, *ll_test_model, allocator);
    context->set_max_error_count(999999);
    
    auto mock_parser = [&](const char* program) {
        Buffer buffer = {
            .content = const_cast<char*>(program),
            .length  = strlen(program)
        };
        Module* mock_modl = new Module();
        SourceFile* mock_file = new SourceFile(*context, L"", buffer, *mock_modl);
        mock_logger(mock_file->logger);
        Parser* parser = new Parser(*context, *mock_modl, mock_file);
        parser->parse();
        return mock_modl;
    };

    section("parsing", [&] {
        test("integer parse", [&] {
            const char* program = "123456789 74532 2147483647 2147483648 9223372036854775807 9223372036854775808 18446744073709551615 "
                "123'i8  123'i16  123'i32  123'i64"
                "123'u8  123'u16  123'u32  123'u64";
            Module& modl = *mock_parser(program);
    
            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_second<BadScopeLocation, Node*>);

            expect(nodes.size(), to_string<size_t>).to_be(15);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(123456789);
            
            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(74532);
            
            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(2147483647);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[3])->type, type_to_string).to_be(context->int64_type);
            expect(as<Number*>(nodes[3])->value_u64, to_string<int64_t>).to_be(2147483648);

            expect(nodes[4]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[4])->type, type_to_string).to_be(context->int64_type);
            expect(as<Number*>(nodes[4])->value_u64, to_string<int64_t>).to_be(9223372036854775807);

            expect(nodes[5]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[5])->type, type_to_string).to_be(context->uint64_type);
            expect(as<Number*>(nodes[5])->value_u64, to_string<uint64_t>).to_be(9223372036854775808ull);

            expect(nodes[6]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[6])->type, type_to_string).to_be(context->uint64_type);
            expect(as<Number*>(nodes[6])->value_u64, to_string<uint64_t>).to_be(18446744073709551615ull);

            // Testing explicit integer types.
            expect(nodes[7]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[7])->type, type_to_string).to_be(context->int8_type);
            expect(as<Number*>(nodes[7])->value_u64, to_string<int8_t>).to_be(123);

            expect(nodes[8]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[8])->type, type_to_string).to_be(context->int16_type);
            expect(as<Number*>(nodes[8])->value_u64, to_string<int16_t>).to_be(123);

            expect(nodes[9]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[9])->type, type_to_string).to_be(context->int32_type);
            expect(as<Number*>(nodes[9])->value_u64, to_string<int32_t>).to_be(123);

            expect(nodes[10]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[10])->type, type_to_string).to_be(context->int64_type);
            expect(as<Number*>(nodes[10])->value_u64, to_string<int64_t>).to_be(123);

            expect(nodes[11]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[11])->type, type_to_string).to_be(context->uint8_type);
            expect(as<Number*>(nodes[11])->value_u64, to_string<uint8_t>).to_be(123);

            expect(nodes[12]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[12])->type, type_to_string).to_be(context->uint16_type);
            expect(as<Number*>(nodes[12])->value_u64, to_string<uint16_t>).to_be(123);

            expect(nodes[13]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[13])->type, type_to_string).to_be(context->uint32_type);
            expect(as<Number*>(nodes[13])->value_u64, to_string<uint32_t>).to_be(123);

            expect(nodes[14]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[14])->type, type_to_string).to_be(context->uint64_type);
            expect(as<Number*>(nodes[14])->value_u64, to_string<uint64_t>).to_be(123);

        });
        test("parsing hexidecimals", [&] {
            const char* program = "0x0 0x123 0x5aF1Ba7C 0xabcdef 0xABCDEF";
            Module& modl = *mock_parser(program);

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_second<BadScopeLocation, Node*>);

            expect(nodes.size(), to_string<size_t>).to_be(5);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(0);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(291);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(1525791356);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[3])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[3])->value_u64, to_string<int32_t>).to_be(11259375);

            expect(nodes[4]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[4])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[4])->value_u64, to_string<int32_t>).to_be(11259375);

        });
        test("parsing binary", [&] {
            const char* program = "0b0 0b11010 0b111111111";
            Module& modl = *mock_parser(program);

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_second<BadScopeLocation, Node*>);

            expect(nodes.size(), to_string<size_t>).to_be(3);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(0);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(26);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(511);

        });
        test("octol parsing", [&] {
            const char* program = "00 0123 01234567 0523471";
            Module& modl = *mock_parser(program);

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_second<BadScopeLocation, Node*>);

            expect(nodes.size(), to_string<size_t>).to_be(4);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(0);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(83);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(342391);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(as<Number*>(nodes[3])->type, type_to_string).to_be(context->int_type);
            expect(as<Number*>(nodes[3])->value_u64, to_string<int32_t>).to_be(173881);

        });
        test("integer parsing overflow", [&] {
            const char* program = "18446744073709551616";
            Module& modl = *mock_parser(program);
    
            expect_none().to_produce_error(ErrCode::ParseIntegerValueCalcOverflow);

        });
        test("type parsing", [&] {
            const char* program = R"(
                int         a1
                int32       a2
                uint32      a3
                int*        a4
                uint32*     a5
                const int   a6
                const int*  a7
                int**       a8
                const int** a9
            )";
            Module& modl = *mock_parser(program);
            auto nodes = modl.get_global_variables()
                | std::views::transform(get_second<Identifier, Var*>)
                | std::ranges::to<llvm::SmallVector<Var*>>();
            TypeTable& type_table = context->type_table;

            expect(nodes.size(), to_string<size_t>).to_be(9);

            // Base types
            expect(nodes[0]->type, type_to_string).to_be(context->int_type);
            expect(nodes[0]->type->remove_all_const(), type_to_string).to_be(context->int_type);
            expect(nodes[1]->type, type_to_string).to_be(context->int32_type);
            expect(nodes[1]->type->remove_all_const(), type_to_string).to_be(context->int32_type);
            expect(nodes[2]->type, type_to_string).to_be(context->uint32_type);
            expect(nodes[2]->type->remove_all_const(), type_to_string).to_be(context->uint32_type);

            // Pointers
            expect(nodes[3]->type, type_to_string).to_be(type_table.get_ptr_type(context->int_type));
            expect(nodes[3]->type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(context->int_type));
            expect(nodes[4]->type, type_to_string).to_be(type_table.get_ptr_type(context->uint32_type));
            expect(nodes[4]->type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(context->uint32_type));

            // Types with constness
            expect(nodes[5]->type, type_to_string).to_be(type_table.get_const_type(context->int_type));
            expect(nodes[5]->type->remove_all_const(), type_to_string).to_be(context->int_type);
            expect(nodes[6]->type, type_to_string).to_be(type_table.get_ptr_type(type_table.get_const_type(context->int_type)));
            expect(nodes[6]->type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(context->int_type));
            expect(nodes[7]->type, type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(context->int_type)));
            expect(nodes[7]->type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(context->int_type)));
            expect(nodes[8]->type, type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(type_table.get_const_type(context->int_type))));
            expect(nodes[8]->type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(context->int_type)));


        });
        test("function parsing", [&] {
            const char* program = R"(
                void foo() {}
                void foo(int a) {}
                native dllimport void foo2()
                void foo3(int a, int b, int c) {}
            )";
            Module& modl = *mock_parser(program); 
            
            auto funcs = modl.get_global_functions()
                | std::views::transform(get_second<Identifier, FuncList>)
                | std::views::join
                | std::ranges::to<llvm::SmallVector<Func*>>();

            expect(funcs.size(), to_string<size_t>).to_be(4);

            expect(funcs[0]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[0]->params.empty(), to_string<bool>).to_be(true);
            expect(funcs[0]->name, identifier_to_string).to_be(Identifier::get("foo"));
            expect(funcs[0]->scope.empty(), to_string<bool>).to_be(true);
            expect(funcs[0]->modifiers, to_string<uint32_t>).to_be(0);

            expect(funcs[1]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[1]->params.size(), to_string<size_t>).to_be(1);
            expect(funcs[1]->name, identifier_to_string).to_be(Identifier::get("foo"));
            expect(funcs[1]->scope.empty(), to_string<bool>).to_be(true);
            expect(funcs[1]->modifiers, to_string<uint32_t>).to_be(0);

            expect(funcs[2]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[2]->params.empty(), to_string<bool>).to_be(true);
            expect(funcs[2]->name, identifier_to_string).to_be(Identifier::get("foo2"));
            expect(funcs[2]->scope.empty(), to_string<bool>).to_be(true);
            expect(funcs[2]->modifiers, to_string<uint32_t>).to_be(Modifier::Native | Modifier::DllImport);

            expect(funcs[3]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[3]->params.size(), to_string<size_t>).to_be(3);
            expect(funcs[3]->name, identifier_to_string).to_be(Identifier::get("foo3"));
            expect(funcs[3]->scope.empty(), to_string<bool>).to_be(true);
            expect(funcs[3]->modifiers, to_string<uint32_t>).to_be(0);

        });
    });
}
