#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <llvm/ADT/StringRef.h>

#include "Source.h"

namespace acorn {

    class Context;

    using TokenKind = uint16_t;
    struct Token {
        enum : TokenKind {
            INVALID = 0,

            // Reserve space for ASCII characters
            UNIQUE_TOKENS = 127,

            KEYWORD_START,

            // Type keywords
            KW_INT,
            KW_INT8,
            KW_INT16,
            KW_INT32,
            KW_INT64,
            KW_UINT8,
            KW_UINT16,
            KW_UINT32,
            KW_UINT64,
            KW_ISIZE,
            KW_USIZE,
            KW_FLOAT,
            KW_DOUBLE,
            KW_BOOL,
            KW_VOID,
            KW_CHAR,
            KW_CHAR16,
            KW_CONST,
            // Statement keywords
            KW_FN,
            KW_IF,
            KW_ELFIF,
            KW_ELSE,
            KW_RETURN,
            KW_LOOP,
            KW_IN,
            KW_IMPORT,
            KW_CONTINUE,
            KW_BREAK,
            KW_SWITCH,
            KW_CASE,
            KW_STATIC,
            KW_STRUCT,
            KW_INTERFACE,
            KW_ENUM,
            KW_RAISE,
            KW_RAISES,
            KW_TRY,
            KW_RECOVER,
            KW_GENERICS,
            // Expression keywords
            KW_AS,
            KW_BITCAST,
            KW_CONST_CAST,
            KW_THIS,
            KW_SIZEOF,
            KW_COPYOBJ,
            KW_MOVEOBJ,
            KW_NEW,
            KW_UNINIT_NEW,
            KW_DELETE,
            KW_NULL,
            KW_TRUE,
            KW_FALSE,
            // Modifier keywords
            MODIFIER_START,
            KW_NATIVE,
            KW_DLLIMPORT,
            KW_PUBLIC,
            KW_PRIVATE,
            KW_READONLY,
            MODIFIER_END,
            // Comptime directive keywords
            COMPTIME_KEYWORD_START,
            KW_CT_IF,
            KW_CT_ELIF,
            KW_CT_ELSE,
            KW_CT_ENDIF,
            KW_CT_FILE,
            KW_CT_TYPEINFO,
            KW_CT_ABORTS,
            COMPTIME_KEYWORD_END,

            KeywordEnd,

            // Special tokens
            ARROW,
            LT_LT,
            GT_GT,
            LT_EQ,
            GT_EQ,
            EQ_EQ,
            EX_EQ,
            ADD_EQ,
            SUB_EQ,
            MUL_EQ,
            DIV_EQ,
            MOD_EQ,
            AND_EQ,
            OR_EQ,
            CARET_EQ,
            TILDE_EQ,
            LT_LT_EQ,
            GT_GT_EQ,
            POUND_EQ,
            ADD_ADD,
            SUB_SUB,
            SUB_SUB_SUB,
            AND_AND,
            OR_OR,
            // While technically these are not tokens
            // they are used to distinguish pre vs. post
            // incrementing.
            POST_ADD_ADD,
            POST_SUB_SUB,
            RANGE_EQ,
            RANGE_LT,
            DOT_DOT,
            DOT_DOT_DOT,
            BACKSLASH_BACKSLASH,

            // Literals
            INT_LITERAL,
            HEX_LITERAL,
            BIN_LITERAL,
            OCT_LITERAL,
            FLOAT_LITERAL,
            DOUBLE_LITERAL,
            IDENTIFIER,
            STRING_LITERAL,
            CHAR_LITERAL,
            INVALID_STRING_LITERAL,
            INVALID_CHAR_LITERAL,
            INVALID_NUMBER_LITERAL,

            // End of buffer
            EOB
        };

        TokenKind   kind;
        uint32_t    lexeme_length;
        const char* buffer_ptr; // Pointer to the start of the token in the source buffer.


        Token() {}

        Token(TokenKind kind, const char* buffer_ptr, uint32_t lexeme_length)
            : kind(kind)
            , buffer_ptr(buffer_ptr)
            , lexeme_length(lexeme_length) {
        }

        // Checks if the token has the same `kind`.
        [[nodiscard]] constexpr bool is(TokenKind kind) const noexcept {
            return this->kind == kind;
        }

        // Checks if the token does not have the same `kind`.
        [[nodiscard]] constexpr bool is_not(TokenKind kind) const noexcept {
            return !is(kind);
        }

        // Checks if the token is a keyword.
        [[nodiscard]] constexpr bool is_keyword() const noexcept {
            return kind > Token::KEYWORD_START && kind < Token::KeywordEnd &&
                   kind < Token::COMPTIME_KEYWORD_START;
        }

        [[nodiscard]] constexpr bool is_number_literal() const noexcept {
            return kind >= Token::INT_LITERAL && kind <= Token::OCT_LITERAL;
        }

        [[nodiscard]] constexpr bool is_modifier() const noexcept {
            return kind > Token::MODIFIER_START && kind < Token::MODIFIER_END;
        }

        llvm::StringRef text() const {
            return llvm::StringRef(buffer_ptr, static_cast<size_t>(lexeme_length));
        }

        SourceLoc get_location() const {
            return SourceLoc{
                .ptr        = buffer_ptr,
                .central_pt = buffer_ptr,
                .length     = lexeme_length
            };
        }
    };

    std::string token_kind_to_string(TokenKind kind, Context& context);

}

#endif // TOKEN_H
