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
            Invalid = 0,

            // Reserve space for ASCII characters
            UniqueTokens = 127,

            KeywordStart,

            // Type keywords
            KwInt,
            KwInt8,
            KwInt16,
            KwInt32,
            KwInt64,
            KwUInt8,
            KwUInt16,
            KwUInt32,
            KwUInt64,
            KwISize,
            KwUSize,
            KwFloat,
            KwDouble,
            KwBool,
            KwVoid,
            KwChar,
            KwChar16,
            KwConst,
            // Statement keywords
            KwFn,
            KwIf,
            KwElIf,
            KwElse,
            KwReturn,
            KwLoop,
            KwIn,
            KwImport,
            KwContinue,
            KwBreak,
            KwSwitch,
            KwCase,
            KwStatic,
            KwStruct,
            KwInterface,
            KwEnum,
            KwRaise,
            KwRaises,
            KwTry,
            KwRecover,
            KwGenerics,
            // Expression keywords
            KwAs,
            KwThis,
            KwSizeof,
            KwCopyobj,
            KwMoveobj,
            KwNew,
            KwUninitNew,
            KwDelete,
            KwNull,
            KwTrue,
            KwFalse,
            // Modifier keywords
            ModifierStart,
            KwNative,
            KwDllimport,
            KwPublic,
            KwPrivate,
            KwReadonly,
            ModifierEnd,
            // Comptime directive keywords
            ComptimeKeywordStart,
            KwCTIf,
            KwCTElIf,
            KwCTElse,
            KwCTEndIf,
            KwCTFile,
            KwCTTypeInfo,
            KwCTAborts,
            ComptimeKeywordEnd,

            KeywordEnd,

            // Special tokens
            Arrow,
            LtLt,
            GtGt,
            LtEq,
            GtEq,
            EqEq,
            ExEq,
            AddEq,
            SubEq,
            MulEq,
            DivEq,
            ModEq,
            AndEq,
            OrEq,
            CaretEq,
            TildeEq,
            LtLtEq,
            GtGtEq,
            AddAdd,
            SubSub,
            SubSubSub,
            AndAnd,
            OrOr,
            // While technically these are not tokens
            // they are used to distinguish pre vs. post
            // incrementing.
            PostAddAdd,
            PostSubSub,
            RangeEq,
            RangeLt,
            DotDot,
            DotDotDot,
            BackslashBackslash,

            // Literals
            IntLiteral,
            HexLiteral,
            BinLiteral,
            OctLiteral,
            FloatLiteral,
            DoubleLiteral,
            Identifier,
            StringLiteral,
            CharLiteral,
            InvalidStringLiteral,
            InvalidCharLiteral,
            InvalidNumberLiteral,

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
            return kind > Token::KeywordStart && kind < Token::KeywordEnd &&
                   kind < Token::ComptimeKeywordStart;
        }

        [[nodiscard]] constexpr bool is_number_literal() const noexcept {
            return kind >= Token::IntLiteral && kind <= Token::OctLiteral;
        }

        [[nodiscard]] constexpr bool is_modifier() const noexcept {
            return kind > Token::ModifierStart && kind < Token::ModifierEnd;
        }

        llvm::StringRef text() const {
            return llvm::StringRef(buffer_ptr, static_cast<size_t>(lexeme_length));
        }

        SourceLoc get_location() const {
            return SourceLoc{ buffer_ptr, lexeme_length };
        }
    };

    std::string token_kind_to_string(TokenKind kind, Context& context);

}

#endif // TOKEN_H
