#include "Token.h"

#include <format>

#include "Context.h"

std::string acorn::token_kind_to_string(TokenKind kind, Context& context) {
    switch (kind) {
    case Token::Invalid:              return "invalid";
    case Token::EOB:                  return "eob";
    case Token::Identifier:           return "identifier";
    case Token::IntLiteral:           return "int-literal";
    case Token::HexLiteral:           return "hex-literal";
    case Token::BinLiteral:           return "bin-literal";
    case Token::OctLiteral:           return "oct-literal";
    case Token::FloatLiteral:         return "float32-literal";
    case Token::DoubleLiteral:        return "float64-literal";
    case Token::StringLiteral:        return "string-literal";
    case Token::CharLiteral:          return "char-literal";
    case Token::InvalidStringLiteral: return "invalid-string-literal";
    case Token::InvalidCharLiteral:   return "invalid-char-literal";
    case Token::InvalidNumberLiteral: return "invalid-number-literal";
    case Token::Arrow:                return "->";
    case Token::GtGt:                 return ">>";
    case Token::LtLt:                 return "<<";
    case Token::GtEq:                 return ">=";
    case Token::LtEq:                 return "<=";
    case Token::EqEq:                 return "==";
    case Token::ExEq:                 return "!=";
    case Token::AddEq:                return "+=";
    case Token::SubEq:                return "-=";
    case Token::MulEq:                return "*=";
    case Token::DivEq:                return "/=";
    case Token::ModEq:                return "%=";
    case Token::AndEq:                return "&=";
    case Token::OrEq:                 return "|=";
    case Token::CaretEq:              return "^=";
    case Token::TildeEq:              return "~=";
    case Token::LtLtEq:               return "<<=";
    case Token::GtGtEq:               return ">>=";
    case Token::AddAdd:
    case Token::PostAddAdd:           return "++";
    case Token::SubSub:
    case Token::PostSubSub:           return "--";
    case Token::SubSubSub:            return "---";
    case Token::AndAnd:               return "&&";
    case Token::OrOr:                 return "||";
    case Token::RangeEq:              return "..=";
    case Token::RangeLt:              return "..<";
    case Token::DotDot:               return "..";
    case Token::DotDotDot:            return "...";
    case Token::BackslashBackslash:   return "\\\\";

    default: {
        if (kind > Token::KeywordStart && kind < Token::KeywordEnd) {
            return context.get_keyword_from_kind(kind).str();
        } else if (kind < Token::UniqueTokens) {
            uint32_t val = static_cast<uint32_t>(kind);
            if (val >= 33 && val <= 126) {
                return std::string(1, static_cast<char>(kind));
            } else {
                return std::format("cc ({})", val);
            }
        }
        return "Unknown (Internal error)";
    }
    }
}
