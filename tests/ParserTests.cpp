#include "ParserTests.h"

#include <ranges>

// Include parser first because it has a function named expect.
#include "Parser.h"

#include "TestFramework.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "SourceFile.h"

using namespace acorn;

static llvm::LLVMContext ll_context;
static llvm::Module* ll_test_model = new llvm::Module("Test Module", ll_context);

static PageAllocator allocator(4096);

static Context* context;

std::string node_kind_to_string(NodeKind kind) {
    return std::to_string(static_cast<unsigned>(kind));
}

std::string identifier_to_string(Identifier identifier) {
    return identifier.to_string().str();
}

std::string type_to_string(Type* type) {
    return type->to_string();
}

template<typename K, typename V>
auto get_second(const std::pair<K, V>& pair) -> V {
    return pair.second;
}

acorn::Node* get_bad_scope_node(const acorn::Module::BadScopeNode& bad_node) {
    return bad_node.node;
}

void test_parser() {

    context = allocator.alloc_type<Context>();
    new (context) Context(ll_context, *ll_test_model, allocator);
    context->set_max_error_count(999999);
    context->set_stand_alone();

    acorn::initialize_float_parsing(allocator);
    
    auto mock_parser = [&](const char* program) {
        Buffer buffer = {
            .content = const_cast<char*>(program),
            .length  = strlen(program)
        };
        acorn::Identifier::clear_cache();
        Module* mock_modl = new Module();
        SourceFile* mock_file = new SourceFile(*context, L"", L"", buffer, *mock_modl);
        mock_logger(mock_file->logger);
        Parser* parser = new Parser(*context, *mock_modl, mock_file);
        parser->parse();
        return mock_file;
    };


    section("parsing", [&] {
        test("integer parse", [&] {
            const char* program = "123456789; 74532; 2147483647; 2147483648; 9223372036854775807; 9223372036854775808; 18446744073709551615; "
                "123'i8;  123'i16;  123'i32;  123'i64;"
                "123'u8;  123'u16;  123'u32;  123'u64;"
                "4294967295'u32; -1'u32; -2147483648'u32;";
            Module& modl = mock_parser(program)->modl;
    
            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(18);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(123456789);
            
            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(74532);
            
            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(2147483647);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[3])->type, type_to_string).to_be(context->int64_type);
            expect(static_cast<Number*>(nodes[3])->value_u64, to_string<int64_t>).to_be(2147483648);

            expect(nodes[4]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[4])->type, type_to_string).to_be(context->int64_type);
            expect(static_cast<Number*>(nodes[4])->value_u64, to_string<int64_t>).to_be(9223372036854775807);

            expect(nodes[5]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[5])->type, type_to_string).to_be(context->uint64_type);
            expect(static_cast<Number*>(nodes[5])->value_u64, to_string<uint64_t>).to_be(9223372036854775808ull);

            expect(nodes[6]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[6])->type, type_to_string).to_be(context->uint64_type);
            expect(static_cast<Number*>(nodes[6])->value_u64, to_string<uint64_t>).to_be(18446744073709551615ull);

            // Testing explicit integer types.
            expect(nodes[7]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[7])->type, type_to_string).to_be(context->int8_type);
            expect(static_cast<Number*>(nodes[7])->value_u64, to_string<int8_t>).to_be(123);

            expect(nodes[8]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[8])->type, type_to_string).to_be(context->int16_type);
            expect(static_cast<Number*>(nodes[8])->value_u64, to_string<int16_t>).to_be(123);

            expect(nodes[9]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[9])->type, type_to_string).to_be(context->int32_type);
            expect(static_cast<Number*>(nodes[9])->value_u64, to_string<int32_t>).to_be(123);

            expect(nodes[10]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[10])->type, type_to_string).to_be(context->int64_type);
            expect(static_cast<Number*>(nodes[10])->value_u64, to_string<int64_t>).to_be(123);

            expect(nodes[11]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[11])->type, type_to_string).to_be(context->uint8_type);
            expect(static_cast<Number*>(nodes[11])->value_u64, to_string<uint8_t>).to_be(123);

            expect(nodes[12]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[12])->type, type_to_string).to_be(context->uint16_type);
            expect(static_cast<Number*>(nodes[12])->value_u64, to_string<uint16_t>).to_be(123);

            expect(nodes[13]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[13])->type, type_to_string).to_be(context->uint32_type);
            expect(static_cast<Number*>(nodes[13])->value_u64, to_string<uint32_t>).to_be(123);

            expect(nodes[14]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[14])->type, type_to_string).to_be(context->uint64_type);
            expect(static_cast<Number*>(nodes[14])->value_u64, to_string<uint64_t>).to_be(123);

            expect(nodes[15]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[15])->type, type_to_string).to_be(context->uint32_type);
            expect(static_cast<Number*>(nodes[15])->value_u64, to_string<uint64_t>).to_be(4294967295u);

            expect(nodes[16]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[16])->type, type_to_string).to_be(context->uint32_type);
            expect(static_cast<Number*>(nodes[16])->value_u64, to_string<uint64_t>).to_be(4294967295u);

            expect(nodes[17]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[17])->type, type_to_string).to_be(context->uint32_type);
            expect(static_cast<Number*>(nodes[17])->value_u64, to_string<uint64_t>).to_be(2147483648u);

        });
        test("parsing hexidecimals", [&] {
            const char* program = "0x0; 0x123; 0x5aF1Ba7C; 0xabcdef; 0xABCDEF;";
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(5);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(0);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(291);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(1525791356);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[3])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[3])->value_u64, to_string<int32_t>).to_be(11259375);

            expect(nodes[4]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[4])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[4])->value_u64, to_string<int32_t>).to_be(11259375);

        });
        test("parsing binary", [&] {
            const char* program = "0b0; 0b11010; 0b111111111;";
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(3);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(0);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(26);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(511);

        });
        test("octal parsing", [&] {
            const char* program = "00; 0123; 01234567; 0523471;";
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(4);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[0])->value_u64, to_string<int32_t>).to_be(0);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[1])->value_u64, to_string<int32_t>).to_be(83);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[2])->value_u64, to_string<int32_t>).to_be(342391);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[3])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[3])->value_u64, to_string<int32_t>).to_be(173881);

        });
        test("integer parsing overflow", [&] {
            const char* program = "18446744073709551616;";
            mock_parser(program);
    
            expect_none().to_produce_error(ErrCode::ParseIntegerValueCalcOverflow);

        });
        test("type parsing", [&] {
            const char* program = R"(
                int         a1;
                int32       a2;
                uint32      a3;
                int*        a4;
                uint32*     a5;
                const int   a6;
                const int*  a7;
                int**       a8;
                const int** a9;
            )";
            SourceFile& file = *mock_parser(program);
            auto nodes = file.get_variables()
                | std::views::transform(get_second<Identifier, Var*>)
                | std::ranges::to<llvm::SmallVector<Var*>>();
            TypeTable& type_table = context->type_table;

            expect(nodes.size(), to_string<size_t>).to_be(9);

            // Base types
            expect(nodes[0]->parsed_type, type_to_string).to_be(context->int_type);
            expect(nodes[0]->parsed_type->remove_all_const(), type_to_string).to_be(context->int_type);
            expect(nodes[1]->parsed_type, type_to_string).to_be(context->int32_type);
            expect(nodes[1]->parsed_type->remove_all_const(), type_to_string).to_be(context->int32_type);
            expect(nodes[2]->parsed_type, type_to_string).to_be(context->uint32_type);
            expect(nodes[2]->parsed_type->remove_all_const(), type_to_string).to_be(context->uint32_type);

            // Pointers
            expect(nodes[3]->parsed_type, type_to_string).to_be(type_table.get_ptr_type(context->int_type));
            expect(nodes[3]->parsed_type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(context->int_type));
            expect(nodes[4]->parsed_type, type_to_string).to_be(type_table.get_ptr_type(context->uint32_type));
            expect(nodes[4]->parsed_type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(context->uint32_type));

            // Types with constness
            expect(nodes[5]->parsed_type, type_to_string).to_be(type_table.get_const_type(context->int_type));
            expect(nodes[5]->parsed_type->remove_all_const(), type_to_string).to_be(context->int_type);
            expect(nodes[6]->parsed_type, type_to_string).to_be(type_table.get_ptr_type(type_table.get_const_type(context->int_type)));
            expect(nodes[6]->parsed_type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(context->int_type));
            expect(nodes[7]->parsed_type, type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(context->int_type)));
            expect(nodes[7]->parsed_type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(context->int_type)));
            expect(nodes[8]->parsed_type, type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(type_table.get_const_type(context->int_type))));
            expect(nodes[8]->parsed_type->remove_all_const(), type_to_string).to_be(type_table.get_ptr_type(type_table.get_ptr_type(context->int_type)));

        });
        test("function parsing", [&] {
            const char* program = R"(
                void foo() {}
                void foo(int a) {}
                native dllimport void foo2();
                void foo3(int a, int b, int c) {}
            )";
            SourceFile& file = *mock_parser(program); 
            
            auto funcs = file.get_functions()
                | std::views::transform(get_second<Identifier, FuncList>)
                | std::views::join
                | std::ranges::to<llvm::SmallVector<Func*>>();

            expect(funcs.size(), to_string<size_t>).to_be(4);

            expect(funcs[0]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[0]->params.empty(), to_string<bool>).to_be(true);
            expect(funcs[0]->name, identifier_to_string).to_be(Identifier::get("foo"));
            expect(funcs[0]->scope->empty(), to_string<bool>).to_be(true);
            expect(funcs[0]->modifiers, to_string<uint32_t>).to_be(0);

            expect(funcs[1]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[1]->params.size(), to_string<size_t>).to_be(1);
            expect(funcs[1]->name, identifier_to_string).to_be(Identifier::get("foo"));
            expect(funcs[1]->scope->empty(), to_string<bool>).to_be(true);
            expect(funcs[1]->modifiers, to_string<uint32_t>).to_be(0);

            expect(funcs[2]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[2]->params.empty(), to_string<bool>).to_be(true);
            expect(funcs[2]->name, identifier_to_string).to_be(Identifier::get("foo2"));
            expect(funcs[2]->scope, to_string<ScopeStmt*>).to_be(nullptr);
            expect(funcs[2]->modifiers, to_string<uint32_t>).to_be(Modifier::Native | Modifier::DllImport);

            expect(funcs[3]->return_type, type_to_string).to_be(context->void_type);
            expect(funcs[3]->params.size(), to_string<size_t>).to_be(3);
            expect(funcs[3]->name, identifier_to_string).to_be(Identifier::get("foo3"));
            expect(funcs[3]->scope->empty(), to_string<bool>).to_be(true);
            expect(funcs[3]->modifiers, to_string<uint32_t>).to_be(0);

        });
        test("numeric operations", [&] {
            const char* program = R"(
                4624 + 67423;
                256 - 6;
                34 - 743;
                13 * 52;
                3452 / 56;
                417 % 5;
                324 + 33 * 22;
                22 * 324 + 33;
                436 -346;
                53 +74;
                436 + -346;
                53 + +74;
                22 * (324 + 33);
                52 + 23 / 88 * 32 - 5 + 12 / 6;
                32 & 63 >> 2 ^ 12 * 94 - 32 / 3 >> 13 | ~11;
            )";
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(15);
            
            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[0])->value_u64, to_string<uint64_t>).to_be(72047ull);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[1])->value_u64, to_string<uint64_t>).to_be(250ull);
            
            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[2])->value_s64, to_string<int64_t>).to_be(-709ll);
            
            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[3])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[3])->value_u64, to_string<uint64_t>).to_be(676ull);
            
            expect(nodes[4]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[4])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[4])->value_u64, to_string<uint64_t>).to_be(61ull);

            expect(nodes[5]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[5])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[5])->value_u64, to_string<uint64_t>).to_be(2ull);

            expect(nodes[6]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[6])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[6])->value_u64, to_string<uint64_t>).to_be(1050ull);

            expect(nodes[7]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[7])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[7])->value_u64, to_string<uint64_t>).to_be(7161ull);

            expect(nodes[7]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[7])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[7])->value_u64, to_string<uint64_t>).to_be(7161ull);

            expect(nodes[8]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[8])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[8])->value_u64, to_string<uint64_t>).to_be(90ull);

            expect(nodes[9]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[9])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[9])->value_u64, to_string<uint64_t>).to_be(127ull);

            expect(nodes[10]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[10])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[10])->value_u64, to_string<uint64_t>).to_be(90ull);

            expect(nodes[11]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[11])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[11])->value_u64, to_string<uint64_t>).to_be(127ull);

            expect(nodes[12]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[12])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[12])->value_u64, to_string<uint64_t>).to_be(7854ull);

            expect(nodes[13]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[13])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[13])->value_u64, to_string<uint64_t>).to_be(49ull);

            expect(nodes[14]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[14])->type, type_to_string).to_be(context->int_type);
            expect(static_cast<Number*>(nodes[14])->value_s64, to_string<int64_t>).to_be(-12ll);

        });

        test("add signed overflow", [&] {
            mock_parser("2147483647 + 1;");
            expect_none().to_produce_error(ErrCode::NumericOverflow);
        });
        test("add signed underflow", [&] {
            mock_parser("-2147483648 + -1;");
            expect_none().to_produce_error(ErrCode::NumericUnderflow);
        });
        test("add unsigned overflow", [&] {
            mock_parser("4294967295'u32 + 1'u32;");
            expect_none().to_produce_error(ErrCode::NumericOverflow);
        });
        test("sub signed overflow", [&] {
            mock_parser("2147483647 - -1;");
            expect_none().to_produce_error(ErrCode::NumericOverflow);
        });
        test("sub signed underflow", [&] {
            mock_parser("-2147483648 - 1;");
            expect_none().to_produce_error(ErrCode::NumericUnderflow);
        });
        test("sub unsigned overflow", [&] {
            mock_parser("5'u32 - 10'u32;");
            expect_none().to_produce_error(ErrCode::NumericUnderflow);
        });
        test("integer calc overflow", [&] {
            mock_parser("18446744073709551616;");
            expect_none().to_produce_error(ErrCode::ParseIntegerValueCalcOverflow);
        });
        test("integer calc underflow", [&] {
            mock_parser("-9223372036854775809;");
            expect_none().to_produce_error(ErrCode::ParseIntegerValueCalcUnderflow);
        });
        test("int8 lit does not fit", [&] {
            mock_parser("128'i8;");
            expect_none().to_produce_error(ErrCode::ParseIntegerValueNotFitType);
        });
        test("signed int8 lit does not fit", [&] {
            mock_parser("-129'i8;");
            expect_none().to_produce_error(ErrCode::ParseIntegerValueNotFitType);
        });
        test("uint8 lit does not fit", [&] {
            mock_parser("256'u8;");
            expect_none().to_produce_error(ErrCode::ParseIntegerValueNotFitType);
        });
        test("int16 lit does not fit", [&] {
            mock_parser("32768'i16;");
            expect_none().to_produce_error(ErrCode::ParseIntegerValueNotFitType);
        });
        test("string literals", [&] {
            const char* program = R"(
                "";
                "abcASwe325#@R12eAsF/.,(1";
                "abc\n\\\tew@\f";
                "abc\uBA1F:D";
                "abc\U000Abf03jt";
            )";
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(5);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::String);
            expect(static_cast<String*>(nodes[0])->type, type_to_string).to_be(context->const_char_ptr_type);
            expect(static_cast<String*>(nodes[0])->text8bit, std::identity()).to_be("");

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::String);
            expect(static_cast<String*>(nodes[1])->type, type_to_string).to_be(context->const_char_ptr_type);
            expect(static_cast<String*>(nodes[1])->text8bit, std::identity()).to_be("abcASwe325#@R12eAsF/.,(1");

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::String);
            expect(static_cast<String*>(nodes[2])->type, type_to_string).to_be(context->const_char_ptr_type);
            expect(static_cast<String*>(nodes[2])->text8bit, std::identity()).to_be("abc\n\\\tew@\f");

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::String);
            expect(static_cast<String*>(nodes[3])->type, type_to_string).to_be(context->const_char16_ptr_type);

            expect(nodes[4]->kind, node_kind_to_string).to_be(NodeKind::String);
            expect(static_cast<String*>(nodes[4])->type, type_to_string).to_be(context->const_char32_ptr_type);
            
        });
        test("char literals", [&] {
            const char* program = R"(
                'a';
                '^';
                '\uB35c';
                '\U62Ab0DD7';
            )";
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(4);

            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->char_type);
            expect(static_cast<Number*>(nodes[0])->value_u64, to_string<uint64_t>).to_be(97);

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->char_type);
            expect(static_cast<Number*>(nodes[1])->value_u64, to_string<uint64_t>).to_be(94);

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->char16_type);
            expect(static_cast<Number*>(nodes[2])->value_u64, to_string<uint64_t>).to_be(0xB35c);

            expect(nodes[3]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[3])->type, type_to_string).to_be(context->char32_type);
            expect(static_cast<Number*>(nodes[3])->value_u64, to_string<uint64_t>).to_be(0x62Ab0DD7);

        });
        test("elm type of array must have element", [&] {
            mock_parser("int[4][] a;");
            expect_none().to_produce_error(ErrCode::ParseElmTypeMustHaveArrLen);
        });
        test("float 64 arithmetic", [&] {
            const char* program = R"(
                5421.743166 + 314.781;
                412.712 - 521.341 / 6.3;
                125.624 * 412.721 / 4.542156 + 63.213;
            )";

            const double value1 = 5421.743166 + 314.781;
            const double value2 = 412.712 - 521.341 / 6.3;
            const double value3 = 125.624 * 412.721 / 4.542156 + 63.213;
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(3);
            
            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->float64_type);
            expect(std::bit_cast<uint64_t>(static_cast<Number*>(nodes[0])->value_f64), to_string<uint64_t>).to_be(std::bit_cast<uint64_t>(value1));

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->float64_type);
            expect(std::bit_cast<uint64_t>(static_cast<Number*>(nodes[1])->value_f64), to_string<uint64_t>).to_be(std::bit_cast<uint64_t>(value2));

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->float64_type);
            expect(std::bit_cast<uint64_t>(static_cast<Number*>(nodes[2])->value_f64), to_string<uint64_t>).to_be(std::bit_cast<uint64_t>(value3));

        });
        test("float 32 arithmetic", [&] {
            const char* program = R"(
                5421.743166f + 314.781f;
                412.712f - 521.341f / 6.3f;
                125.624f * 412.721f / 4.542156f + 63.213f;
            )";

            const float value1 = 5421.743166f + 314.781f;
            const float value2 = 412.712f - 521.341 / 6.3f;
            const float value3 = 125.624f * 412.721f / 4.542156f + 63.213f;
            Module& modl = mock_parser(program)->modl;

            auto nodes = std::views::transform(modl.get_bad_scope_nodes(),
                                               get_bad_scope_node);

            expect(nodes.size(), to_string<size_t>).to_be(3);
            
            expect(nodes[0]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[0])->type, type_to_string).to_be(context->float32_type);
            expect(std::bit_cast<uint32_t>(static_cast<Number*>(nodes[0])->value_f32), to_string<uint32_t>).to_be(std::bit_cast<uint32_t>(value1));

            expect(nodes[1]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[1])->type, type_to_string).to_be(context->float32_type);
            expect(std::bit_cast<uint32_t>(static_cast<Number*>(nodes[1])->value_f32), to_string<uint32_t>).to_be(std::bit_cast<uint32_t>(value2));

            expect(nodes[2]->kind, node_kind_to_string).to_be(NodeKind::Number);
            expect(static_cast<Number*>(nodes[2])->type, type_to_string).to_be(context->float32_type);
            expect(std::bit_cast<uint32_t>(static_cast<Number*>(nodes[2])->value_f32), to_string<uint32_t>).to_be(std::bit_cast<uint32_t>(value3));

        });
    });

    /*
        Code used for testing floating point numbers. It would be unreasonable
        to test the floating point numbers every time so this is left here commented
        out.
    
        acorn::PageAllocator allocator(1024);

        int total_overflow_or_underflows = 0;

        template<typename F>
        static void test(std::string text, F &&parse_function) {
            auto [value, error] = parse_function(allocator, text.c_str());
            if (error != acorn::FloatParseError::None) {
                ++total_overflow_or_underflows;
                if (error == acorn::FloatParseError::Overflow) {
                    value = std::numeric_limits<decltype(value)>::infinity();
                    // Have to check for negative here because the parsing does not
                    // make a distinguishing error for overflowing when negative.
                    if (text[0] == '-') {
                        value = -value;
                    }
                } else {
                    value = 0.0;
                }
            }

            decltype(value) correct_value;
            std::from_chars(text.c_str(), text.c_str() + text.size(), correct_value);

            if (value != correct_value) {
                std::cout << "Value mismatch for: " << text << "\n";
                exit(1);
            }
        }

        template<typename F>
        static void test_random_floats(int max_whole_digits,
                                       int min_exp_value, 
                                       int max_exp_value, 
                                       double exp_dist_mean,
                                       double exp_dist_std_dev,
                                       F&& parse_function) {
            std::random_device dev;
            std::mt19937 generator(dev());

            std::uniform_int_distribution<int> whole_digit_count_dist(1, max_whole_digits);
            std::uniform_int_distribution<int> fraction_digit_count_dist(1, 40);
            std::uniform_int_distribution<int> digit_dist(0, 9);
            std::uniform_int_distribution<int> has_decimal_dist(0, 100);
            std::uniform_int_distribution<int> has_exponent_dist(0, 2);
            std::normal_distribution<> exponent_value_dist(exp_dist_mean, exp_dist_std_dev);
            std::uniform_int_distribution<int> is_negative_dist(0, 6);
    
            const int total = 10'000'000;
            for (int i = 0; i < total; i++) {
                std::string text = "";
                int negative_chance = is_negative_dist(generator);
                if (negative_chance == 0) {
                    text = "-";
                }
    
                int whole_digits = whole_digit_count_dist(generator);
                for (int j = 0; j < whole_digits; j++) {
                    text += digit_dist(generator) + '0';
                }
                int decimal_chance = has_decimal_dist(generator);
                if (decimal_chance != 1) {
                    text += ".";
                    int fraction_digits = fraction_digit_count_dist(generator);
                    for (int j = 0; j < fraction_digits; j++) {
                        text += digit_dist(generator) + '0';
                    }
                }
                int exponent_chance = has_exponent_dist(generator);
                if (exponent_chance == 1) {
                    text += "E";
                    int exp_value = static_cast<int>(exponent_value_dist(generator));
                    exp_value = std::max(min_exp_value, std::min(max_exp_value, exp_value));
                    text += std::to_string(exp_value);
                }
    
                std::cout << "testing: " << text << "\n";
                test(text, parse_function);
            }
            std::cout << "\n";
            std::cout << "successfully parsed: " << total << "\n";
            double under_over_percent = (total_overflow_or_underflows / (double)total) * 100;
            std::cout << "over/under flow: " << std::fixed << std::setprecision(2) << under_over_percent << "%\n";
        }

        static void test_32_bit_floats() {

            auto test_32bit = [](std::string text) {
                test(text, &acorn::parse_float32_bits);
            };

            // Edge cases:
            test_32bit("0.0");
            test_32bit("-0.0");
            test_32bit("1.17549435E-38"); // smallest normalized.
            test_32bit("1.40129846E-45"); // smallest subnormal.
            test_32bit("3.40282347E38");  // largest normalized.
            test_32bit("1.7014117E-38");  // largest denormalized.
            test_32bit("3.40282348E38");  // slightly above maximum but still shouldn't overflow due to rounding down.
            test_32bit("3.40282347E+39"); // should overflow.
            test_32bit("1.0000001");
            test_32bit("1.9999999");
            test_32bit("1.1754944E-38"); // just above smallest normalized number.
            test_32bit("1.1754941E-38"); // just below smallest normalized, expected subnormal.
            test_32bit("1E-50");
            test_32bit("-1E-50");
            test_32bit("1E-300");
            test_32bit("1.0E+300");
    
            // Values that gave issues at one point or another.
            test_32bit("58939.23392128");
            test_32bit("379018.7048317439011206813390");
            test_32bit("93706.114487064");
            test_32bit("93706.114487064");
            test_32bit("7588557.7262");
            test_32bit("7661612.0445606E20");
            test_32bit("-861.1876523230203");
            test_32bit("-8581454.1968874340633539076258593948992E33");

            int max_whole_digits = 8;
            int min_exp_value = -45;
            int max_exp_value = 38;
            double exp_dist_mean = 6.0;
            double exp_dist_std_dev = 10.0;
            test_random_floats(max_whole_digits, 
                               min_exp_value, 
                               max_exp_value, 
                               exp_dist_mean, 
                               exp_dist_std_dev,
                               &acorn::parse_float32_bits);
        }

        static void test_64_bit_floats() {

            auto test_64bit = [](std::string text) {
                test(text, &acorn::parse_float64_bits);
            };

            // Edge cases:
            test_64bit("0.0");
            test_64bit("-0.0");
            test_64bit("2.225E-308"); // smallest normalized.
            test_64bit("4.94E-324"); // smallest subnormal.
            test_64bit("1.7976931348623157E308");  // largest normalized.
            test_64bit("2.2250738585072014E-308");  // largest denormalized.
            test_64bit("1.0000001");
            test_64bit("1.9999999");
            test_64bit("1E-50");
            test_64bit("-1E-400");
            test_64bit("1E-800");
            test_64bit("1.0E+500");

            int max_whole_digits = 16;
            int min_exp_value = -324;
            int max_exp_value = 308;
            double exp_dist_mean = 50.0;
            double exp_dist_std_dev = 95.0;
            test_random_floats(max_whole_digits,
                               min_exp_value, 
                               max_exp_value, 
                               exp_dist_mean, 
                               exp_dist_std_dev,
                               &acorn::parse_float64_bits);
        }

        int main() {

            acorn::initialize_float_parsing(allocator);
            //test_32_bit_floats();
            test_64_bit_floats();

            return 0;
        }
    */
}
