#ifndef LEXER_H
#define LEXER_H

#include "Token.h"
#include "Logger.h"

namespace acorn {

    class Context;

    class Lexer {
    public:

        Lexer(const Context& context, Buffer buffer, Logger& logger);

        Lexer(Lexer&&) = default;
        
        [[nodiscard]] Token next_token();

        [[nodiscard]] bool is_next_token_on_line();

    private:
        const Context& context;
        Logger&        logger;

        const char* ptr;
        const char* end;

        void eat_single_line_comment();
        void eat_multiline_comment();

        Token next_word();

        Token next_number(const char* start);
        Token finish_mumber(tokkind kind, const char* start);

        Token next_string();

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

#endif // LEXER_H