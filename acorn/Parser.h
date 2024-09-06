#ifndef PARSER_H
#define PARSER_H

#include "Logger.h"
#include "Context.h"
#include "Lexer.h"
#include "AST.h"

namespace acorn {

    class Type;
    class Module;
    class TypeTable;

    class Parser {
        static const size_t MAX_PEEKED_TOKENS = 8;

    public:

        Parser(Context& context, Module& modl, SourceFile* file);

        void parse();

    private:
        Context&       context;
        Module&        modl;
        PageAllocator& context_allocator;
        SourceFile*    file;
        TypeTable&     type_table;
        
        Logger& logger;
        
        Func* cur_func = nullptr;

        Lexer lex;
        Token cur_token;
        Token prev_token;

        Token  peeked_tokens[MAX_PEEKED_TOKENS];
        size_t peeked_size = 0;

        // Statement parsing
        //--------------------------------------

        Node* parse_statement();

        Func* parse_function(uint32_t modifiers, Type* type);
        Func* parse_function(uint32_t modifiers, Type* type, Identifier name);

        Var* parse_variable();
        Var* parse_variable(uint32_t modifiers, Type* type);
        Var* parse_variable(uint32_t modifiers, Type* type, Identifier name);

        uint32_t parse_modifiers();

        ReturnStmt*     parse_return();
        IfStmt*         parse_if();
        ComptimeIfStmt* parse_comptime_if(bool chain_start = true);

        ScopeStmt* parse_scope(const char* closing_for = nullptr);

        // Expression parsing
        //--------------------------------------

        Type* parse_type();

        Type* parse_base_type();

        Expr* parse_assignment_and_expr();
        Expr* parse_expr();
        Expr* parse_binary_expr();
        Expr* parse_postfix();
        Expr* parse_function_call(Expr* site);
        Expr* parse_term();

        Expr* parse_int_literal();
        Expr* parse_hex_literal();
        Expr* parse_bin_literal();
        Expr* parse_oct_literal();
        Expr* parse_string8bit_literal();
        Expr* parse_string16bit_literal();
        Expr* parse_string32bit_literal();

        template<uint32_t radix, uint64_t convert_table[256], bool use_table = true>
        Expr* parse_number_literal(const char* start, const char* end);

        // Utility functions
        //--------------------------------------

        // Gets the next token either from the peeked_tokens
        // if there are any or from the lexer and stores it
        // in cur_token.
        void next_token();

        // Get the nth (zero based) peeked token.
        Token peek_token(size_t n);

        // Expect the current token to be of kind and if it is it consumes
        // it.
        void expect(tokkind kind, const char* for_msg = nullptr);

        // Expect the current token to be an identifier and construct
        // an identifier object if it is. It then consumes the token.
        Identifier expect_identifier(const char* for_msg = nullptr);

        template<typename... TArgs>
        [[nodiscard]] Logger& error(SourceLoc error_loc, const char* fmt, TArgs&&... args) {
            return logger.begin_error(error_loc, fmt, std::forward<TArgs>(args)...);
        }

        template<typename... TArgs>
        [[nodiscard]] Logger& error(Token error_token, const char* fmt, TArgs&&... args) {
            return error(error_token.loc, fmt, std::forward<TArgs>(args)...);
        }

        template<typename... TArgs>
        [[nodiscard]] Logger& error(const char* fmt, TArgs&&... args) {
            return error(cur_token, fmt, std::forward<TArgs>(args)...);
        }

        template<typename T>
        T* new_node(Token token) {
            return new_node<T>(token.loc);
        }

        template<typename T>
        T* new_node(SourceLoc loc) {
            T* node = static_cast<T*>(
                context.get_allocator().allocate(sizeof(T)));
            new (node) T(); // Call the constructor.
            node->loc = loc;
            return node;
        }

        // Continues to skip tokens until it can find a new valid
        // location to start parsing again.
        void skip_recovery();

    };
}

#endif // PARSER_H