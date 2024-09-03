#include "LexerTests.h"

#include <iostream>
#include <llvm/IR/Module.h>

#include "Lexer.h"
#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"

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
        SourceFile* mock_file = new SourceFile(*context, L"", buffer, mock_modl);
        Lexer* lexer = new Lexer(*context, buffer, mock_logger(mock_file->logger));
        return lexer;
    };
    
    section("lexing", [&] {
        test("keywords", [&] {
            const char* program = "int8 int16 int32 int64 uint8 "
                "uint16 uint32 uint64 bool void char char16 char32 "
                "const as null true false native dllimport return";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::KwInt8);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwInt16);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwInt32);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwInt64);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwUInt8);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwUInt16);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwUInt32);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwUInt64);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwBool);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwVoid);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwChar);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwChar16);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwChar32);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwConst);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwAs);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwNull);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwTrue);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwFalse);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwNative);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwDllimport);
            expect(lexer.next_token().kind, to_string).to_be(Token::KwReturn);

        });
        test("identifiers", [&] {
            const char* program = "abc abc123 ABC123 aBc123 666abc abc^qq";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).not_to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).not_to_be(Token::Identifier);
            expect(lexer.next_token().kind, to_string).to_be(Token::Identifier);
        });
        test("special tokens", [&] {
            const char* program = "~ + - * / % ! < > ^ & | ( ) { } = "
                ">> << ++ -- == != <= >= %= ^= &= *= /= += -= |= >>= <<= ~=";
            Lexer& lexer = *mock_lexer(program);
            
            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

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
            expect(lexer.next_token().kind, to_string).to_be(Token::GtGt);
            expect(lexer.next_token().kind, to_string).to_be(Token::LtLt);
            expect(lexer.next_token().kind, to_string).to_be(Token::AddAdd);
            expect(lexer.next_token().kind, to_string).to_be(Token::SubSub);
            expect(lexer.next_token().kind, to_string).to_be(Token::EqEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::ExEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::LtEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::GtEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::ModEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::CaretEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::AndEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::MulEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::DivEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::AddEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::SubEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::OrEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::GtGtEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::LtLtEq);
            expect(lexer.next_token().kind, to_string).to_be(Token::TildeEq);

        });
        test("integer tokens", [&] {
            const char* program = "123 0 99999999999999 123456789 987654321 "
                "123_123_123 1_5135_312_63 "
                "123'i8   123'i16   123'i32   123'i64   123'u8   123'u16   123'u32   123'u64 "
                "1_2_3'i8   1_2_3'i16   1_2_3'i32   1_2_3'i64   1_2_3'u8   1_2_3'u16   1_2_3'u32   1_2_3'u64";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            // type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            // type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral);
        });
        test("hexidecimal tokens", [&] {
            const char* program = "0x0 0x523 0xFFFFFF 0x123456789ABCDEF 0xffffff 0x123456789abcdef "
                "0x12Af_31 0x4_1_F_98 "
                "0x1Fa'i8   0x1Fa'i16   0x1Fa'i32   0x1Fa'i64   0x1Fa'u8   0x1Fa'u16   0x1Fa'u32   0x1Fa'u64 "
                "0x1_Fa'i8   0x1_Fa'i16   0x1_Fa'i32   0x1_Fa'i64   0x1_Fa'u8   0x1_Fa'u16   0x1_Fa'u32   0x1_Fa'u64 ";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            // Underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            // Type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            // Type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::HexLiteral);
        });
        test("binary tokens", [&] {
            const char* program = "0b0 0b10101001 0b000 0b1011101 "
                "0b1_00110_0 0b1_00__1 "
                "0b101'i8   0b101'i16   0b101'i32   0b101'i64   0b101'u8   0b101'u16   0b101'u32   0b101'u64 "
                "0b1_01'i8   0b1_01'i16   0b1_01'i32   0b1_01'i64   0b1_01'u8   0b1_01'u16   0b1_01'u32   0b1_01'u64 ";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            // Underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            // Type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            // Type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::BinLiteral);
        });
        test("octol tokens", [&] {
            const char* program = "00 0123 0735 001234567 "
                "031_532 052_52__2 "
                "0123'i8   0123'i16   0123'i32   0123'i64   0123'u8   0123'u16   0123'u32   0123'u64 "
                "01_23'i8   01_23'i16   01_23'i32   01_23'i64   01_23'u8   01_23'u16   01_23'u32   01_23'u64 ";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);     

            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            // Underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            // Type specifiers
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            // Type specifiers with underscores
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::OctLiteral);
        });
        test("invalid type specifiers", [&] {
            const char* program = "523'i11  636'  7'i  62'u  23'7  51'123";
            Lexer& lexer = *mock_lexer(program);
            
            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token(), empty<Token>).to_produce_error(ErrCode::LexNumberBadTypeSpec);
            expect(lexer.next_token().kind, to_string).to_be(Token::IntLiteral); // 7 occures after '7
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

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::String8BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String8BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String8BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String8BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String8BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String8BitLiteral);
        });
        test("string 16bit tokens", [&] {
            const char* program = R"(
                "asd!\u0000"   "asd!\u1Ca2ASd21@"
            )";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::String16BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String16BitLiteral);
        });
        test("string 32bit tokens", [&] {
            const char* program = R"(
                "asd!\U00000000"   "asd!\U00FAF12Sd21@"
                "dgwSA@\u0011 abc\U000C23A6"  "abc\U000C23A6 dgwSA@\u0011"
            )";
            Lexer& lexer = *mock_lexer(program);

            auto to_string = std::bind(token_kind_to_string, std::ref(*context), std::placeholders::_1);

            expect(lexer.next_token().kind, to_string).to_be(Token::String32BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String32BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String32BitLiteral);
            expect(lexer.next_token().kind, to_string).to_be(Token::String32BitLiteral);
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