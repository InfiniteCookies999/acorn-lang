#ifndef LEXER_H
#define LEXER_H

#include "Token.h"
#include "Logger.h"

namespace acorn {

    class Context;

    class Lexer {
    public:

        Lexer(const Context& context, Buffer buffer, Logger& logger);

        Lexer(const Lexer& lexer, llvm::SmallVector<Token, 8> peeked_tokens, Logger& logger);

        Lexer(Lexer&&) = default;

        [[nodiscard]] Token next_token();

        int get_total_lines_lexed() const {
            return total_lines_lexed;
        }

        int get_whitespace_lines_lexed() const {
            return whitespace_lines_lexed;
        }

    private:
        const Context& context;
        Logger&        logger;

        int total_lines_lexed      = 0;
        int whitespace_lines_lexed = 0;

        const char* ptr;
        const char* end;

        void eat_single_line_comment();
        void eat_multiline_comment();

        Token next_word();

        Token next_number(const char* start);
        Token finish_int_number(tokkind kind, const char* start);
        Token finish_float_number(const char* start, bool has_errors);

        bool skip_unicode_seq_digits(size_t n);
        Token next_string();
        Token next_char();

        Token next_comptime();

        inline Token new_token(const char* start, uint16_t length, tokkind c) noexcept {
            return Token(c, SourceLoc{ start, length });
        }

        inline Token new_token_and_eat(tokkind c) noexcept {
            return Token(c, SourceLoc{ ptr++, 1 });
        }

        inline Token new_token(tokkind kind, const char* start) {
            uint16_t length = static_cast<uint16_t>(ptr - start);
            return Token(kind, SourceLoc{ start, length });
        }

        template<typename... TArgs>
        [[nodiscard]] Logger& error(const char* fmt, TArgs&&... args) {
            return logger.begin_error(SourceLoc{ ptr, 1 }, fmt, std::forward<TArgs>(args)...);
        }

    };
}

#endif