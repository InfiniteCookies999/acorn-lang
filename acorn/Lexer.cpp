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
           (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z');
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

acorn::Token acorn::Lexer::next_token() {

    bool whitespace = false;

RestartLexingLabel:

#define two_tok(c1, c2, c2kind)                 \
case c1:                                        \
    if (*(++ptr) == c2) { ++ptr;                \
        return new_token(ptr - 2, 2, c2kind); } \
    return new_token(ptr - 1, 1, c1);


    switch (*ptr) {

    // Skip any whitespace.
    case ' ': case '\t': case '\v': case '\f':
        ++ptr;
        goto RestartLexingLabel;

    // Encountered a new line.
    case '\n': {
        if (whitespace) {
            ++whitespace_lines_lexed;
        }
        whitespace = true;
        ++total_lines_lexed;
        ++ptr;
        goto RestartLexingLabel;
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
        goto RestartLexingLabel;
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

    case '/': {
        ++ptr;
        if (*ptr == '/') {
            eat_single_line_comment();
            goto RestartLexingLabel;
        } else if (*ptr == '*') {
            eat_multiline_comment();
            goto RestartLexingLabel;
        } else if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::DivEq);
        }
        return new_token(ptr - 1, 1, '/');
    }
    case ',':
        return new_token_and_eat(',');

    two_tok('=', '=', Token::EqEq);
    two_tok('*', '=', Token::MulEq);
    two_tok('%', '=', Token::ModEq);
    two_tok('^', '=', Token::CaretEq);
    two_tok('~', '=', Token::TildeEq);
    two_tok('!', '=', Token::ExEq);
    two_tok(':', ':', Token::ColCol);
    two_tok('\\', '\\', Token::BackslashBackslash);

    case '&': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::AndEq);
        } else if (*ptr == '&') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::AndAnd);
        }
        return new_token(ptr - 1, 1, '&');
    }
    case '|': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::OrEq);
        } else if (*ptr == '|') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::OrOr);
        }
        return new_token(ptr - 1, 1, '|');
    }
    case '+': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::AddEq);
        } else if (*ptr == '+') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::AddAdd);
        } else if (is_digit(*ptr)) {
            return next_number(ptr - 1);
        }
        return new_token(ptr - 1, 1, '+');
    }
    case '-': {
        ++ptr;
        if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::SubEq);
        } else if (*ptr == '-') {
            ++ptr;
            if (*ptr == '-') {
                ++ptr;
                return new_token(ptr - 3, 3, Token::SubSubSub);
            }
            return new_token(ptr - 2, 2, Token::SubSub);
        } else if (is_digit(*ptr)) {
            return next_number(ptr - 1);
        }
        return new_token(ptr - 1, 1, '-');
    }
    case '>': {
        ++ptr;
        if (*ptr == '>') {
            if (*(++ptr) == '=') {
                ++ptr;
                return new_token(ptr - 3, 3, Token::GtGtEq);
            }
            return new_token(ptr - 2, 2, Token::GtGt);
        }
        else if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::GtEq);
        }
        return new_token(ptr - 1, 1, '>');
    }
    case '<': {
        ++ptr;
        if (*ptr == '<') {
            if (*(++ptr) == '=') {
                ++ptr;
                return new_token(ptr - 3, 3, Token::LtLtEq);
            }
            return new_token(ptr - 2, 2, Token::LtLt);
        }
        else if (*ptr == '=') {
            ++ptr;
            return new_token(ptr - 2, 2, Token::LtEq);
        }
        return new_token(ptr - 1, 1, '<');
    }
    case '.': {
        ++ptr;
        if (*ptr == '.' && *(ptr + 1) == '=') {
            ptr += 2;
            return new_token(ptr - 3, 3, Token::RangeEq);
        } else if (*ptr == '.' && *(ptr + 1) == '<') {
            ptr += 2;
            return new_token(ptr - 3, 3, Token::RangeLt);
        } else if (*ptr == '.') {
            ptr += 1;
            if (*ptr == '.') {
                ptr += 1;
                return new_token(ptr - 3, 3, Token::DotDotDot);
            } else {
                return new_token(ptr - 2, 2, Token::DotDot);
            }
        }
        return new_token(ptr - 1, 1, '.');
    }
    case '#':
        return next_comptime();
    case '(':
        return new_token_and_eat('(');
    case ')':
        return new_token_and_eat(')');
    case '[':
        return new_token_and_eat('[');
    case ']':
        return new_token_and_eat(']');
    case '{':
        return new_token_and_eat('{');
    case '}':
        return new_token_and_eat('}');
    case ';':
        return new_token_and_eat(';');
    case '?':
        return new_token_and_eat('?');
    case '$':
        return new_token_and_eat('$');

    case '\0':
        return new_token_and_eat(Token::EOB);
    default: {
        unsigned char unknown_char = static_cast<unsigned char>(*ptr);
        if (unknown_char >= 33 && unknown_char <= 126) { // Easily displayable.
            logger.begin_error(SourceLoc{ ptr, 1 }, "Invalid character: '%s'", std::string(1, unknown_char))
                  .add_arrow_msg(Logger::ArrowPosition::At, "remove this character")
                  .end_error(ErrCode::LexInvalidChar);
        } else {
            error("Invalid character (ascii code): '%s'", unknown_char).end_error(ErrCode::LexInvalidChar);
        }
        ++ptr;
        goto RestartLexingLabel;
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

    ptr += 2; // Skip '/*'

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

    const char* start = ptr;

    while (is_alpha(*ptr) || is_digit(*ptr) || *ptr == '_') {
        ++ptr;
    }

    auto word = llvm::StringRef(start, ptr - start);
    auto kind = context.get_keyword_kind(word);

    return new_token(kind != Token::Invalid ? kind : Token::Identifier, start);
}

acorn::Token acorn::Lexer::next_number(const char* start) {

    if (*ptr == '0') {
        ++ptr;
        if (*ptr == 'x') {
            // Lexing hexidecimal
            ++ptr;
            while (is_hexidecimal(*ptr) || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
            return finish_int_number(Token::HexLiteral, start);
        } else if (*ptr == 'b') {
            // Lexing binary
            ++ptr;
            while (*ptr == '0' || *ptr == '1' || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
            return finish_int_number(Token::BinLiteral, start);
        } else if (is_octal(*ptr)) {
            // Parsing octal number
            while (is_octal(*ptr) || *ptr == NUMBER_SEPERATOR) {
                ++ptr;
            }
            return finish_int_number(Token::OctLiteral, start);
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

        return finish_float_number(start, has_errors);
    }

    return finish_int_number(Token::IntLiteral, start);
}

acorn::Token acorn::Lexer::finish_int_number(tokkind kind, const char* start) {

    if (*(ptr - 1) == NUMBER_SEPERATOR) {
        auto error_loc = SourceLoc{ start, static_cast<uint16_t>(ptr - start) };
        logger.begin_error(error_loc, "Numbers cannot end with _")
              .end_error(ErrCode::LexNumberCannotEndUnderscore);
        return new_token(Token::InvalidNumberLiteral, start);
    }

    if (*ptr == '\'') {
        auto invalid_type_spec = [this, start]() finline{
            auto error_loc = SourceLoc{ start, static_cast<uint16_t>(ptr - start) };
            logger.begin_error(error_loc, "Expected to end in a type specifier")
                  .add_line("One of: 'i8, 'i16, 'i32, 'i64, 'u8, 'u16, 'u32, 'u64")
                  .end_error(ErrCode::LexNumberBadTypeSpec);
            return new_token(Token::InvalidNumberLiteral, start);
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
            return new_token(kind, start);
        }
        ++ptr;
        if ((digit1 == '1' && digit2 == '6') ||
            (digit1 == '3' && digit2 == '2') ||
            (digit1 == '6' && digit2 == '4')
            ) {
            return new_token(kind, start);
        }
        return invalid_type_spec();
    } else if (*ptr == 'f' || *ptr == 'd') {   // Integer will be treated as a 32/64 bit float.
        ++ptr;
    }

    return new_token(kind, start);
}

acorn::Token acorn::Lexer::finish_float_number(const char* start, bool has_errors) {
    tokkind kind = Token::Float64Literal;;

    if (*ptr == 'f') {
        ++ptr;
        kind = Token::Float32Literal;
    } else if (*ptr == 'd') {
        ++ptr;
    }

    return new_token(has_errors ? Token::InvalidNumberLiteral : kind, start);
}

bool acorn::Lexer::skip_unicode_seq_digits(size_t n) {
    ++ptr; // Skip u or U
    for (size_t i = 0; i < n && *ptr != '\0'; ++i, ++ptr) {
        if (!is_hexidecimal(*ptr)) {
            error("Incomplete unicode sequence. Expected '%s' digits", n)
                .end_error(ErrCode::LexInvalidUnicodeSeq);
            return false;
        }
    }
    return true;
}

acorn::Token acorn::Lexer::next_string() {

    const char* start = ptr;
    ++ptr; // Skip initial "

    tokkind kind = Token::String8BitLiteral;
    bool invalid = false;
    while (true) {
        switch (*ptr) {
        case '"': case '\0': case '\r': case '\n':
            goto FinishedStringLexLab;
        case '\\': { // Escape cases!
            ++ptr;
            if (*ptr == 'u') {          // Unicode 16 bits
                if (kind != Token::String32BitLiteral)
                    kind = Token::String16BitLiteral;
                invalid |= !skip_unicode_seq_digits(4);
            } else if (*ptr == 'U') {   // Unicode 32 bits
                kind = Token::String32BitLiteral;
                // TODO: This does not actually reach 32 bits of unicode characters!
                invalid |= !skip_unicode_seq_digits(8);
            } else if (!(*ptr == '\n' || *ptr == '\r' || *ptr == '\0')) {
                ++ptr;
            }
            break;
        }
        default:
            // TODO: check for character validation?
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
        SourceLoc loc = SourceLoc{
            .ptr = start,
            .length = static_cast<uint16_t>(ptr - start)
        };
        logger.begin_error(loc, "Expected closing \" for string")
            .end_error(ErrCode::LexStringMissingEndQuote);
    }

    return new_token(start, static_cast<uint16_t>(ptr - start), !invalid ? kind : Token::InvalidStringLiteral);
}

acorn::Token acorn::Lexer::next_char() {

    const char* start = ptr;
    ++ptr; // Skip initial '

    // TODO: What should we do if the user types a tab?
    switch (*ptr) {
    case '\0':
    case '\r':
    case '\n': {
        error("Expected closing ' for character")
            .end_error(ErrCode::LexCharMissingEndQuote);
        return new_token(start, static_cast<uint16_t>(ptr - start), Token::InvalidCharLiteral);
    }
    case '\\': {
        ++ptr;
        if (*ptr == 'u' || *ptr == 'U') {
            if (!skip_unicode_seq_digits(*ptr == 'u' ? 4 : 8)) {
                return new_token(start, static_cast<uint16_t>(ptr - start), Token::InvalidCharLiteral);
            }
        } else if(!(*ptr == '\n' || *ptr == '\r' || *ptr == '\0')) {
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
        return new_token(start, static_cast<uint16_t>(ptr - start), Token::InvalidCharLiteral);
    } else {
        ++ptr; // Eating closing '
        return new_token(start, static_cast<uint16_t>(ptr - start), Token::CharLiteral);
    }
}

acorn::Token acorn::Lexer::next_comptime() {

    const char* start = ptr;

    ++ptr; // Eating the '#' character.

    while (is_alpha(*ptr) || is_digit(*ptr) || *ptr == '_') {
        ++ptr;
    }

    auto word = llvm::StringRef(start, ptr - start);
    auto kind = context.get_keyword_kind(word);

    Token token = new_token(kind, start);
    if (kind == Token::Invalid) {
        logger.begin_error(token.loc, "Unknown comptime directive")
            .end_error(ErrCode::LexUnknownComptimeDirective);
    }
    return token;

}
