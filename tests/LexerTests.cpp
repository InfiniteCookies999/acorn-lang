#include "LexerTests.h"

#include <iostream>
#include <llvm/IR/Module.h>

#include "Lexer.h"
#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "SourceFile.h"

#include "TestFramework.h"

using namespace acorn;

static llvm::LLVMContext ll_context;
static llvm::Module* ll_test_model = new llvm::Module("Test Module", ll_context);

static PageAllocator allocator(4096);
static Module mock_modl;

static Context* context;

template<typename T>
static std::string empty(T) {
    return "";
}

void test_lexer() {

    context = allocator.alloc_type<Context>();
    new (context) Context(ll_context, *ll_test_model, allocator);
    context->set_max_error_count(999999);

    auto mock_lexer = [&](const char* program) {
        Buffer buffer = {
            .content = const_cast<char*>(program),
            .length  = strlen(program)
        };
        SourceFile* mock_file = new SourceFile(*context, "", "", buffer, mock_modl);
        Lexer* lexer = new Lexer(*context, buffer, set_logger_mock_interpreter(mock_file->logger));
        return lexer;
    };

    section("lexing", [&] {
        test("keywords", [&] {
            const char* program = "int8 int16 int32 int64 uint8 "
                "uint16 uint32 uint64 bool void char "
                "const as null true false native dllimport return";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect((TokenKind)lexer.next_token().kind, to_string).to_be(Token::KW_INT8);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_INT16);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_INT32);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_INT64);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_UINT8);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_UINT16);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_UINT32);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_UINT64);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_BOOL);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_VOID);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_CHAR);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_CONST);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_AS);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_NULL);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_TRUE);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_FALSE);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_NATIVE);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_DLLIMPORT);
            expect(lexer.next_token().kind, to_string).to_be(Token::KW_RETURN);

        });
        test("identifiers", [&] {
            const char* program = "abc abc123 ABC123 aBc123 666abc abc^qq";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).not_to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).not_to_be(Token::IDENTIFIER);
            expect(lexer.next_token().kind, to_string).to_be(Token::IDENTIFIER);
        });
        test("special tokens", [&] {
            const char* program = "~ + - * / % ! < > ^ & | ( ) { } = "
                ">> << ++ -- == != <= >= %= ^= &= *= /= += -= |= >>= <<= ~=";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be('~');
            expect(lexer.next_token().kind, to_string).to_be('+');
            expect(lexer.next_token().kind, to_string).to_be('-');
            expect(lexer.next_token().kind, to_string).to_be('*');
            expect(lexer.next_token().kind, to_string).to_be('/');
            expect(lexer.next_token().kind, to_string).to_be('%');
            expect(lexer.next_token().kind, to_string).to_be('!');
            expect(lexer.next_token().kind, to_string).to_be('<');
            expect(lexer.next_token().kind, to_string).to_be('>');
            expect(lexer.next_token().kind, to_string).to_be('^');
            expect(lexer.next_token().kind, to_string).to_be('&');
            expect(lexer.next_token().kind, to_string).to_be('|');
            expect(lexer.next_token().kind, to_string).to_be('(');
            expect(lexer.next_token().kind, to_string).to_be(')');
            expect(lexer.next_token().kind, to_string).to_be('{');
            expect(lexer.next_token().kind, to_string).to_be('}');
            expect(lexer.next_token().kind, to_string).to_be('=');
            expect(lexer.next_token().kind, to_string).to_be(Token::GT_GT);
            expect(lexer.next_token().kind, to_string).to_be(Token::LT_LT);
            expect(lexer.next_token().kind, to_string).to_be(Token::ADD_ADD);
            expect(lexer.next_token().kind, to_string).to_be(Token::SUB_SUB);
            expect(lexer.next_token().kind, to_string).to_be(Token::EQ_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::EX_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::LT_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::GT_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::MOD_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::CARET_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::AND_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::MUL_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::DIV_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::ADD_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::SUB_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::OR_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::GT_GT_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::LT_LT_EQ);
            expect(lexer.next_token().kind, to_string).to_be(Token::TILDE_EQ);

        });
        test("integer tokens", [&] {
            const char* program = "123 0 99999999999999 123456789 987654321 "
                "123_123_123 1_5135_312_63 "
                "123'i8   123'i16   123'i32   123'i64   123'u8   123'u16   123'u32   123'u64 "
                "1_2_3'i8   1_2_3'i16   1_2_3'i32   1_2_3'i64   1_2_3'u8   1_2_3'u16   1_2_3'u32   1_2_3'u64";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            // type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            // type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL);
        });
        test("hexidecimal tokens", [&] {
            const char* program = "0x0 0x523 0xFFFFFF 0x123456789ABCDEF 0xffffff 0x123456789abcdef "
                "0x12Af_31 0x4_1_F_98 "
                "0x1Fa'i8   0x1Fa'i16   0x1Fa'i32   0x1Fa'i64   0x1Fa'u8   0x1Fa'u16   0x1Fa'u32   0x1Fa'u64 "
                "0x1_Fa'i8   0x1_Fa'i16   0x1_Fa'i32   0x1_Fa'i64   0x1_Fa'u8   0x1_Fa'u16   0x1_Fa'u32   0x1_Fa'u64 ";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            // Underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            // Type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            // Type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::HEX_LITERAL);
        });
        test("binary tokens", [&] {
            const char* program = "0b0 0b10101001 0b000 0b1011101 "
                "0b1_00110_0 0b1_00__1 "
                "0b101'i8   0b101'i16   0b101'i32   0b101'i64   0b101'u8   0b101'u16   0b101'u32   0b101'u64 "
                "0b1_01'i8   0b1_01'i16   0b1_01'i32   0b1_01'i64   0b1_01'u8   0b1_01'u16   0b1_01'u32   0b1_01'u64 ";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            // Underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            // Type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            // Type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::BIN_LITERAL);
        });
        test("octol tokens", [&] {
            const char* program = "00 0123 0735 001234567 "
                "031_532 052_52__2 "
                "0123'i8   0123'i16   0123'i32   0123'i64   0123'u8   0123'u16   0123'u32   0123'u64 "
                "01_23'i8   01_23'i16   01_23'i32   01_23'i64   01_23'u8   01_23'u16   01_23'u32   01_23'u64 ";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            // Underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            // Type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            // Type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::OCT_LITERAL);
        });
        test("invalid type specifiers", [&] {
            const char* program = "523'i11  636'  7'i  62'u  23'7  51'123";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token().kind, to_string).to_be(Token::INT_LITERAL); // 7 occures after '7
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
        });
        test("Number cannot end in _", [&] {
            const char* program = "123_ 0_ 523_ 0x41_ 0b1011_ 0412_";
            Lexer& lexer = *mock_lexer(program);

            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberCannotEndUnderscore);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberCannotEndUnderscore);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberCannotEndUnderscore);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberCannotEndUnderscore);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberCannotEndUnderscore);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberCannotEndUnderscore);
        });
        test("string 8bit tokens", [&] {
            const char* program = R"(
                ""   "abc"   "!@#sad34asd@#%$fsda;./!"  "\n"
                "\'\\\"\a\b\f\n\r\v\t"  "as\naAS\b"
            )";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::placeholders::_1, std::ref(*context));

            expect(lexer.next_token().kind, to_string).to_be(Token::STRING_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::STRING_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::STRING_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::STRING_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::STRING_LITERAL);
            expect(lexer.next_token().kind, to_string).to_be(Token::STRING_LITERAL);
        });
        test("string missing closing quote", [&] {
            const char* program = R"(
                "asd!  abc
                "okioki)";
            Lexer& lexer = *mock_lexer(program);

            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexStringMissingEndQuote);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexStringMissingEndQuote);
        });
        test("string bad unicode sequence", [&] {
            const char* program = R"(
                "asd!\u000"  "asd!\u00F"
                "asAS\U0000"   "sdag\U0000AAA"
            )";
            Lexer& lexer = *mock_lexer(program);

            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexInvalidUnicodeSeq);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexInvalidUnicodeSeq);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexInvalidUnicodeSeq);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexInvalidUnicodeSeq);
        });
    });
}
