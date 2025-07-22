#include "Lexer.h"

#include "Context.h"

static bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z');
}

static bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

static bool is_hexidecimal(char c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

static bool is_octal(char c) {
    return c >= '0' && c <= '7';
}

acorn::Lexer::Lexer(const Context& context, Buffer buffer, Logger& logger) :
    context(context),
    logger(logger),
    ptr(buffer.content),
    end(buffer.content + buffer.length) {
}

acorn::Lexer::Lexer(const Lexer& lexer, llvm::SmallVector<Token, 8> peeked_tokens, Logger& logger)
    : context(lexer.context),
      logger(logger),
      ptr(lexer.ptr),
      end(lexer.end),
      total_lines_lexed(lexer.total_lines_lexed),
      whitespace_lines_lexed(lexer.whitespace_lines_lexed) {

}

acorn::Token acorn::Lexer::next_token() {

    // Keep track of if whitespace was parsed to determine
    // if the line is only made up of whitespace.
    bool whitespace = false;

#define lex1(c) { ++ptr; return Token(c, ptr - 1, 1); }
#define lex2(c1, c2, c2kind) {            \
    ++ptr;                                \
    if (*ptr == c2) {                     \
        ++ptr;                            \
        return Token(c2kind, ptr - 2, 2); \
    }                                     \
    return Token(c1, ptr - 1, 1);       }


start:
    switch (*ptr) {
    // Skip any whitespace.
    case ' ': case '\t': case '\v': case '\f':
        ++ptr;
        goto start;
    // Encountered a new line.
    case '\n': {
        if (whitespace) {
            ++whitespace_lines_lexed;
        }
        whitespace = true;
        ++total_lines_lexed;
        ++ptr;
        goto start;
    }
    case '\r': {
        if (whitespace) {
            ++whitespace_lines_lexed;
        }
        whitespace = true;
        ++total_lines_lexed;
        ++ptr;
        // Check for windows \r\n case.
        if (*ptr == '\n') {
            ++ptr;
        }
        goto start;
    }
    // End of buffer
    case '\0': {
        ++ptr;
        return Token(Token::EOB, ptr - 1, 1);
    }
    case 'a': case 'b': case 'c': case 'd': case 'e':
    case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y':
    case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y':
    case 'Z':
    case '_':
        return next_word();
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return next_number(ptr);
    case '\"':
        return next_string();
    case '\'':
        return next_char();
    case '#':
        if (*(ptr + 1) == '=') {
            ptr += 2;
            return Token(Token::POUND_EQ, ptr - 2, 2);
        }
        return next_comptime();
    // Special characters
    case ',': lex1(',');
    case ':': lex1(':');
    case '(': lex1('(');
    case ')': lex1(')');
    case '[': lex1('[');
    case ']': lex1(']');
    case '{': lex1('{');
    case '}': lex1('}');
    case ';': lex1(';');
    case '?': lex1('?');
    case '$': lex1('$');
    case '=':  lex2('=', '=', Token::EQ_EQ);
    case '*':  lex2('*', '=', Token::MUL_EQ);
    case '%':  lex2('%', '=', Token::MOD_EQ);
    case '^':  lex2('^', '=', Token::CARET_EQ);
    case '~':  lex2('~', '=', Token::TILDE_EQ);
    case '!':  lex2('!', '=', Token::EX_EQ);
    case '\\': lex2('\\', '\\', Token::BACKSLASH_BACKSLASH);
    case '/': {
        ++ptr;
        if (*ptr == '/') {
            eat_single_line_comment();
            goto start;
        } else if (*ptr == '*') {
            eat_multiline_comment();
            goto start;
        } else if (*ptr == '=') {
            ++ptr;
            return Token(Token::DIV_EQ, ptr - 2, 2);
        }
        return Token('/', ptr - 1, 1);
    }
    case '&': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return Token(Token::AND_EQ, ptr - 2, 2);
        } else if (*ptr == '&') {
            ++ptr;
            return Token(Token::AND_AND, ptr - 2, 2);
        }
        return Token('&', ptr - 1, 1);
    }
    case '|': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return Token(Token::OR_EQ, ptr - 2, 2);
        } else if (*ptr == '|') {
            ++ptr;
            return Token(Token::OR_OR, ptr - 2, 2);
        }
        return Token('|', ptr - 1, 1);
    }
    case '+': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return Token(Token::ADD_EQ, ptr - 2, 2);
        } else if (*ptr == '+') {
            ++ptr;
            return Token(Token::ADD_ADD, ptr - 2, 2);
        } else if (is_digit(*ptr)) {
            return next_number(ptr - 1);
        }
        return Token('+', ptr - 1, 1);
    }
    case '-': {
        ++ptr;
        if (*ptr == '>') {
            ++ptr;
            return Token(Token::ARROW, ptr - 2, 2);
        } else if (*ptr == '=') {
            ++ptr;
            return Token(Token::SUB_EQ, ptr - 2, 2);
        } else if (*ptr == '-') {
            ++ptr;
            if (*ptr == '-') {
                ++ptr;
                return Token(Token::SUB_SUB_SUB, ptr - 3, 3);
            }
            return Token(Token::SUB_SUB, ptr - 2, 2);
        } else if (is_digit(*ptr)) {
            return next_number(ptr - 1);
        }
        return Token('-', ptr - 1, 1);
    }
    case '>': {
        ++ptr;
        if (*ptr == '>') {
            if (*(++ptr) == '=') {
                ++ptr;
                return Token(Token::GT_GT_EQ, ptr - 3, 3);
            }
            return Token(Token::GT_GT, ptr - 2, 2);
        }
        else if (*ptr == '=') {
            ++ptr;
            return Token(Token::GT_EQ, ptr - 2, 2);
        }
        return Token('>', ptr - 1, 1);
    }
    case '<': {
        ++ptr;
        if (*ptr == '<') {
            if (*(++ptr) == '=') {
                ++ptr;
                return Token(Token::LT_LT_EQ, ptr - 3, 3);
            }
            return Token(Token::LT_LT, ptr - 2, 2);
        }
        else if (*ptr == '=') {
            ++ptr;
            return Token(Token::LT_EQ, ptr - 2, 2);
        }
        return Token('<', ptr - 1, 1);
    }
    case '.': {
        ++ptr;
        if (*ptr == '.' && *(ptr + 1) == '=') {
            ptr += 2;
            return Token(Token::RANGE_EQ, ptr - 3, 3);
        } else if (*ptr == '.' && *(ptr + 1) == '<') {
            ptr += 2;
            return Token(Token::RANGE_LT, ptr - 3, 3);
        } else if (*ptr == '.') {
            ptr += 1;
            if (*ptr == '.') {
                ptr += 1;
                return Token(Token::DOT_DOT_DOT, ptr - 3, 3);
            } else {
                return Token(Token::DOT_DOT, ptr - 2, 2);
            }
        }
        return Token('.', ptr - 1, 1);
    }
    // Invalid characters
    default: {

        const char* beg = ptr;

        auto byte = static_cast<unsigned char>(*ptr);
        if (byte >= 33 && byte <= 126) { // Easily displayable.
            logger.begin_error(SourceLoc{ ptr, 1 }, "Invalid character: '%s'", std::string(1, byte))
                  .add_arrow_msg(Logger::CaretPlacement::At, "remove this character")
                  .end_error(ErrCode::LexInvalidChar);
            ++ptr;
            return Token(Token::INVALID, beg, 1);
        }

        if (byte <= 127) {
            error("Invalid character (ASCII code): '%s'", byte).end_error(ErrCode::LexInvalidChar);
            ++ptr;
            return Token(Token::INVALID, beg, 1);
        }

        // Dealing with non-ascii characters.
        bool is_valid_utf8;
        bool is_overlong;
        size_t num_bytes = get_utf8_byte_distance(ptr, is_valid_utf8, is_overlong);

        if (!is_valid_utf8) {
            // else not a valid utf-8 character.
            error("Invalid character. Could not interpret character using UTF-8 encoding")
                .end_error(ErrCode::LexInvalidChar);
            ++ptr;
            return Token(Token::INVALID, beg, 1);
        }

        SourceLoc loc = {
            .ptr    = ptr,
            .length = static_cast<uint32_t>(num_bytes)
        };
        error("Invalid character. Only ASCII characters are allowed outside comments and strings")
            .end_error(ErrCode::LexInvalidChar);

        ptr += num_bytes;
        return Token(Token::INVALID, beg, static_cast<uint32_t>(ptr - beg));
    }
    }
}

void acorn::Lexer::eat_single_line_comment() {

    while (*ptr != '\r' && *ptr != '\n' && *ptr != '\0') {
        ++ptr;
    }

    // Check for windows \r\n case.
    if (*ptr != '\0') {
        ++total_lines_lexed;
        ++whitespace_lines_lexed;
    }
    if (*ptr == '\r' && *(ptr + 1) == '\n') {
        ptr += 2; // skip \r\n
    } else if (*ptr != '\0') {
        ptr += 1; // skip \r or \n
    }
}

void acorn::Lexer::eat_multiline_comment() {

    ptr += 1; // Skip '/*'. Note: one character was already eaten.

    int depth = 1;
    while (*ptr != '\0' && depth > 0) {
        if (*ptr == '/' && *(ptr + 1) == '*') {
            // Found a nested '/*' comment so must increase depth.
            ++depth;
            ptr += 2;
        } else if (*ptr == '*' && *(ptr + 1) == '/') {
            // Found closing '*/', must decrease depth.
            --depth;
            ptr += 2;
        } else if (*ptr == '\n') {
            ++total_lines_lexed;
            ++whitespace_lines_lexed;
            ++ptr;
        } else if (*ptr == '\r') {
            ++total_lines_lexed;
            ++whitespace_lines_lexed;
            ++ptr;
            if (*ptr == '\n') {
                ++ptr;
            }
        } else {
            ++ptr;
        }
    }

    // Make sure the multiline comment was closed.
    if (depth != 0) {
        error("Missing closing '*/' for multi-line comment")
            .end_error(ErrCode::LexMultilineCommentMissingClose);
    }
}

acorn::Token acorn::Lexer::next_word() {

    const char* beg = ptr;

    while (is_alpha(*ptr) || is_digit(*ptr) || *ptr == '_') {
        ++ptr;
    }

    auto lexeme_length = ptr - beg;
    auto word = llvm::StringRef(beg, static_cast<size_t>(lexeme_length));
    auto kind = context.get_keyword_kind(word);

    kind = kind != Token::INVALID ? kind : Token::IDENTIFIER;
    return Token(kind, beg, static_cast<uint32_t>(lexeme_length));
}

acorn::Token acorn::Lexer::next_number(const char* beg) {

    if (*ptr == '0') {
        ++ptr;
        if (*ptr == 'x') {
            // Lexing hexidecimal
            ++ptr;
            while (is_hexidecimal(*ptr) || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
            return finish_int_number(Token::HEX_LITERAL, beg);
        } else if (*ptr == 'b') {
            // Lexing binary
            ++ptr;
            while (*ptr == '0' || *ptr == '1' || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
            return finish_int_number(Token::BIN_LITERAL, beg);
        } else if (is_octal(*ptr)) {
            // Parsing octal number
            while (is_octal(*ptr) || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
            return finish_int_number(Token::OCT_LITERAL, beg);
        }
    }

    // Parsing leading whole digits.
    while (is_digit(*ptr) || *ptr == NUMBER_SEPERATOR) {
        ++ptr;
    }

    if ((*ptr == '.' && *(ptr+1) != '.') ||
        *ptr == 'E' || *ptr == 'e') {

        bool has_errors = false;

        if (*ptr == '.') {
            ++ptr;
            // Parsing fractional digits.
            while (is_digit(*ptr) || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
        }

        if (*ptr == 'E' || *ptr == 'e') {
            ++ptr;

            // Possible exponent sign.
            if (*ptr == '+' || *ptr == '-') {
                ++ptr;
            }

            // Parsing exponent digits.
            bool encountered_exp_digits = false;
            while (is_digit(*ptr) || *ptr == NUMBER_SEPERATOR) {
                if (*ptr == NUMBER_SEPERATOR) {
                    error("Numberic seperators cannot go in the exponent")
                        .end_error(ErrCode::LexNumberSeperateInExp);
                    has_errors = true;
                } else {
                    encountered_exp_digits = true;
                }
                ++ptr;
            }

            if (!encountered_exp_digits) {
                error("Expected digits for exponent")
                    .end_error(ErrCode::LexExpectedDigitsForExp);
                has_errors = true;
            }
        }

        return finish_float_number(beg, has_errors);
    }

    return finish_int_number(Token::INT_LITERAL, beg);
}

acorn::Token acorn::Lexer::finish_int_number(TokenKind kind, const char* beg) {

    if (*(ptr - 1) == NUMBER_SEPERATOR) {
        auto error_loc = SourceLoc::from_ptrs(beg, ptr);
        logger.begin_error(error_loc, "Numbers cannot end with _")
              .end_error(ErrCode::LexNumberCannotEndUnderscore);
        return Token(Token::INVALID_NUMBER_LITERAL, beg, static_cast<uint32_t>(ptr - beg));
    }

    if (*ptr == '\'') {
        auto type_spec_start = ptr;

        auto invalid_type_spec = [this, type_spec_start, beg]() finline{
            auto error_loc = SourceLoc::from_ptrs(type_spec_start, ptr);
            logger.begin_error(error_loc, "Invalid type specifier")
                  .add_line("One of: i8, i16, i32, i64, u8, u16, u32, u64")
                  .end_error(ErrCode::LexNumberBadTypeSpec);
            return Token(Token::INVALID_NUMBER_LITERAL, beg, static_cast<uint32_t>(ptr - type_spec_start));
        };

        // Explicit type specification.
        ++ptr;
        if (!(*ptr == 'i' || *ptr == 'u')) {
            return invalid_type_spec();
        }

        ++ptr;
        char digit1 = *ptr;
        if (!is_digit(*ptr)) {
            return invalid_type_spec();
        }
        ++ptr;
        char digit2 = *ptr;
        if (!is_digit(*ptr)) {
            if (digit1 != '8')
                return invalid_type_spec();
            return Token(kind, beg, static_cast<uint32_t>(ptr - beg));
        }
        ++ptr;
        if ((digit1 == '1' && digit2 == '6') ||
            (digit1 == '3' && digit2 == '2') ||
            (digit1 == '6' && digit2 == '4')
            ) {
            return Token(kind, beg, static_cast<uint32_t>(ptr - beg));
        }
        return invalid_type_spec();
    } else if (*ptr == 'f' || *ptr == 'd') {   // Integer will be treated as a 32/64 bit float.
        ++ptr;
    }

    return Token(kind, beg, static_cast<uint32_t>(ptr - beg));
}

acorn::Token acorn::Lexer::finish_float_number(const char* beg, bool has_errors) {
    TokenKind kind = Token::DOUBLE_LITERAL;;

    if (*ptr == 'f') {
        ++ptr;
        kind = Token::FLOAT_LITERAL;
    } else if (*ptr == 'd') {
        ++ptr;
    }

    kind = has_errors ? Token::INVALID_NUMBER_LITERAL : kind;
    return Token(kind, beg, static_cast<uint32_t>(ptr - beg));
}

bool acorn::Lexer::skip_unicode_seq_digits(size_t n) {
    auto beg = ptr;
    ++ptr; // Skip u or U
    for (size_t i = 0; i < n && *ptr != '\0'; ++i, ++ptr) {
        if (!is_hexidecimal(*ptr)) {
            SourceLoc error_loc = SourceLoc{ beg - 1, static_cast<uint32_t>(i + 2) };
            logger.begin_error(error_loc, "Incomplete UTF-8 sequence. Expected %s digits", n)
                .end_error(ErrCode::LexInvalidUnicodeSeq);
            return false;
        }
    }
    return true;
}

acorn::Token acorn::Lexer::next_string() {

    const char* beg = ptr;
    ++ptr; // Skip initial "

    bool invalid = false;
    while (true) {
        switch (*ptr) {
        case '"': case '\0': case '\r': case '\n':
            goto FinishedStringLexLab;
        case '\\': { // Escape cases!
            ++ptr;
            if (*ptr == 'u') {
                invalid |= !skip_unicode_seq_digits(4);
            } else if (*ptr == 'U') {
                invalid |= !skip_unicode_seq_digits(8);
            } else if (!(*ptr == '\n' || *ptr == '\r' || *ptr == '\0')) {
                ++ptr;
            } else {
                ++ptr;
            }
            break;
        }
        default:
            ++ptr;
        }
    }

FinishedStringLexLab:

    if (*ptr == '"') {
        ++ptr;
    } else {
        invalid = true;

        // Underlining the entire string so that there is not just a weird hanging messing
        // at the end of the line with the whitespace.
        SourceLoc error_loc = SourceLoc::from_ptrs(beg, ptr);
        logger.begin_error(error_loc, "Expected closing \" for string")
            .end_error(ErrCode::LexStringMissingEndQuote);
    }

    auto kind = !invalid ? Token::STRING_LITERAL : Token::INVALID_STRING_LITERAL;
    return Token(kind, beg, static_cast<uint32_t>(ptr - beg));
}

acorn::Token acorn::Lexer::next_char() {

    const char* beg = ptr;
    ++ptr; // Skip initial '

    // TODO: What should we do if the user types a tab?
    switch (*ptr) {
    case '\0':
    case '\r':
    case '\n': {
        error("Expected closing ' for character")
            .end_error(ErrCode::LexCharMissingEndQuote);
        return Token(Token::INVALID_CHAR_LITERAL, beg, static_cast<uint32_t>(ptr - beg));
    }
    case '\\': {
        ++ptr;
        // Prevent the closing quote from ending up on the next line/EOF.
        if (!(*ptr == '\n' || *ptr == '\r' || *ptr == '\0')) {
            ++ptr;
        }
        break;
    }
    default:
        ++ptr;
        break;
    }

    if (*ptr != '\'') {
        error("Expected closing ' for character")
            .end_error(ErrCode::LexCharMissingEndQuote);
        return Token(Token::INVALID_CHAR_LITERAL, beg, static_cast<uint32_t>(ptr - beg));
    } else {
        ++ptr; // Eating closing '
        return Token(Token::CHAR_LITERAL, beg, static_cast<uint32_t>(ptr - beg));
    }
}

acorn::Token acorn::Lexer::next_comptime() {

    const char* beg = ptr;

    ++ptr; // Eating the '#' character.

    while (is_alpha(*ptr) || is_digit(*ptr) || *ptr == '_') {
        ++ptr;
    }

    auto word = llvm::StringRef(beg, static_cast<size_t>(ptr - beg));
    auto kind = context.get_keyword_kind(word);

    Token token = Token(kind, beg, static_cast<uint32_t>(ptr - beg));
    if (kind == Token::INVALID) {
        SourceLoc error_loc = SourceLoc::from_ptrs(beg, ptr);
        logger.begin_error(error_loc, "Unknown comptime directive")
            .end_error(ErrCode::LexUnknownComptimeDirective);
    }
    return token;

}
