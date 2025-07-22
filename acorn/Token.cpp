#include "Token.h"

#include <format>

#include "Context.h"

std::string acorn::token_kind_to_string(TokenKind kind, Context& context) {
    switch (kind) {
    case Token::INVALID:                return "invalid";
    case Token::EOB:                    return "eob";
    case Token::IDENTIFIER:             return "identifier";
    case Token::INT_LITERAL:            return "int-literal";
    case Token::HEX_LITERAL:            return "hex-literal";
    case Token::BIN_LITERAL:            return "bin-literal";
    case Token::OCT_LITERAL:            return "oct-literal";
    case Token::FLOAT_LITERAL:          return "float32-literal";
    case Token::DOUBLE_LITERAL:         return "float64-literal";
    case Token::STRING_LITERAL:         return "string-literal";
    case Token::CHAR_LITERAL:           return "char-literal";
    case Token::INVALID_STRING_LITERAL: return "invalid-string-literal";
    case Token::INVALID_CHAR_LITERAL:   return "invalid-char-literal";
    case Token::INVALID_NUMBER_LITERAL: return "invalid-number-literal";
    case Token::ARROW:                  return "->";
    case Token::GT_GT:                  return ">>";
    case Token::LT_LT:                  return "<<";
    case Token::GT_EQ:                  return ">=";
    case Token::LT_EQ:                  return "<=";
    case Token::EQ_EQ:                  return "==";
    case Token::EX_EQ:                  return "!=";
    case Token::ADD_EQ:                 return "+=";
    case Token::SUB_EQ:                 return "-=";
    case Token::MUL_EQ:                 return "*=";
    case Token::DIV_EQ:                 return "/=";
    case Token::MOD_EQ:                 return "%=";
    case Token::AND_EQ:                 return "&=";
    case Token::OR_EQ:                  return "|=";
    case Token::CARET_EQ:               return "^=";
    case Token::TILDE_EQ:               return "~=";
    case Token::LT_LT_EQ:               return "<<=";
    case Token::GT_GT_EQ:               return ">>=";
    case Token::ADD_ADD:
    case Token::POST_ADD_ADD:           return "++";
    case Token::SUB_SUB:
    case Token::POST_SUB_SUB:           return "--";
    case Token::SUB_SUB_SUB:            return "---";
    case Token::AND_AND:                return "&&";
    case Token::OR_OR:                  return "||";
    case Token::RANGE_EQ:               return "..=";
    case Token::RANGE_LT:               return "..<";
    case Token::DOT_DOT:                return "..";
    case Token::DOT_DOT_DOT:            return "...";
    case Token::BACKSLASH_BACKSLASH:    return "\\\\";

    default: {
        if (kind > Token::KEYWORD_START && kind < Token::KeywordEnd) {
            return context.get_keyword_from_kind(kind).str();
        } else if (kind < Token::UNIQUE_TOKENS) {
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
