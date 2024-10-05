#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <llvm/ADT/StringRef.h>

#include "Source.h"

namespace acorn {

    using tokkind = uint16_t;
    
    class Context;

    struct Token {

        enum {
            Invalid = 0,

            // Reserve space for ASCII characters
            UniqueTokens = 256,

            KeywordStart,

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
            KwBool,
            KwVoid,
            KwChar,
            KwChar16,
            KwChar32,
            KwConst,
            KwAs,
            KwIf,
            KwElIf,
            KwElse,
            KwImport,

            KwNull,
            KwTrue,
            KwFalse,

            ModifierStart,
            KwNative,
            KwDllimport,
            KwPub,
            KwPrv,
            ModifierEnd,

            KwReturn,
            KwLoop,

            ComptimeKeywordStart,
            KwCTIf,
            KwCTElIf,
            KwCTElse,
            KwCTEndIf,
            ComptimeKeywordEnd,

            KeywordEnd,

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
            // While technically these are not tokens
            // they are used to distinguish pre vs. post
            // incrementing.
            PostAddAdd,
            PostSubSub,

            IntLiteral,
            HexLiteral,
            BinLiteral,
            OctLiteral,
            Identifier,
            String8BitLiteral,
            String16BitLiteral,
            String32BitLiteral,
            CharLiteral,
            InvalidStringLiteral,
            InvalidCharLiteral,
            InvalidNumberLiteral,

            // End of buffer
            EOB
        };

        SourceLoc loc;
        tokkind   kind;

        Token() {}

        Token(tokkind kind, SourceLoc loc)
            : kind(kind), loc(loc) {
        }

        // Checks if the token is of the given kind.
        [[nodiscard]] constexpr bool is(tokkind kind) const noexcept { return this->kind == kind; }
    
        // Checks if the token is not of the given kind.
        [[nodiscard]] constexpr bool is_not(tokkind kind) const noexcept { return !is(kind); }

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

        // Get a text representation of the token.
        [[nodiscard]] llvm::StringRef text() const noexcept { return llvm::StringRef(loc.ptr, loc.length); }

    };

    std::string token_kind_to_string(Context& context, tokkind kind);

    std::string to_string(Context& context, Token token);

    std::string to_lexical_string(Context& context, Token token);

}

#endif // TOKEN_H