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

        Token next_number(const char* beg);
        Token finish_int_number(TokenKind kind, const char* beg);
        Token finish_float_number(const char* beg, bool has_errors);

        bool skip_unicode_seq_digits(size_t n);
        Token next_string();
        Token next_char();

        Token next_comptime();

        template<typename... TArgs>
        [[nodiscard]] Logger& error(const char* fmt, TArgs&&... args) {
            return logger.begin_error(SourceLoc{ ptr, 1 }, fmt, std::forward<TArgs>(args)...);
        }

    };
}

#endif
