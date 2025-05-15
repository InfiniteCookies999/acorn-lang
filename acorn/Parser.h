#ifndef PARSER_H
#define PARSER_H

#include "Logger.h"
#include "Context.h"
#include "Lexer.h"
#include "AST.h"
#include "FloatParsing.h"

namespace acorn {

    class Type;
    class Module;
    class TypeTable;
    class SourceFile;

    class Parser {
        static const size_t MAX_PEEKED_TOKENS = 8;
        static const size_t MAX_ARRAY_DIMENSION = 16;

    public:

        Parser(Context& context, Module& modl, SourceFile* file);

        void parse();

        int get_total_lines_parsed() const {
            return lex.get_total_lines_lexed();
        }

        int get_whitespace_lines_parsed() const {
            return lex.get_whitespace_lines_lexed();
        }

    private:
        Context&       context;
        Module&        modl;
        PageAllocator& allocator;
        SourceFile*    file;
        TypeTable&     type_table;

        Logger& logger;

        Func*      cur_func   = nullptr;
        Struct*    cur_struct = nullptr;
        Interface* cur_interface = nullptr;

        Lexer lex;
        Token cur_token;
        Token prev_token;

        Token  peeked_tokens[MAX_PEEKED_TOKENS];
        size_t peeked_size = 0;

        // A single variable list to temporarily
        // place multiple variables that appear in
        // a single line into.
        VarList var_list;

        // Linkage name set by the native modifier.
        llvm::StringRef linkname;

        // If true still parsing imports at the top of the file.
        bool parsing_import_tops      = true;
        bool allow_struct_initializer = true;

        int paran_count   = 0;
        int bracket_count = 0;

        // Statement parsing
        //--------------------------------------

        void add_node_to_global_scope(Node* node);

        void parse_import_top();
        ImportStmt* parse_import();

        Node* parse_statement();
        Node* parse_ident_decl_or_expr(bool is_for_expr);
        Node* parse_decl(uint32_t modifiers, Type* type);

        template<typename D, bool uses_linkname>
        D* new_declaration(uint32_t modifiers, Identifier name, Token loc_token);

        Func* parse_function(uint32_t modifiers, Type* type);
        Func* parse_function(uint32_t modifiers,
                             Type* type,
                             bool has_implicit_return_ptr,
                             Identifier name,
                             bool is_copy_constructor = false,
                             bool is_move_constructor = false);

        Var* parse_variable();
        Var* parse_variable(uint32_t modifiers, Type* type);
        Var* parse_variable(uint32_t modifiers, Type* type, Identifier name);
        Node* parse_variable_list(uint32_t modifiers, Type* type);

        Struct* parse_struct();
        Struct* parse_struct(uint32_t modifiers, Identifier name);
        void add_node_to_struct(Struct* structn, Node* node);
        void add_node_to_interface(Interface* interfacen, Node* node);

        Enum* parse_enum(uint32_t modifiers);
        Interface* parse_interface(uint32_t modifiers);

        uint32_t parse_modifiers();

        ReturnStmt*        parse_return();
        IfStmt*            parse_if();
        void               parse_comptime_if(bool chain_start = true, bool takes_path = true);
        Node*              parse_loop();
        PredicateLoopStmt* parse_predicate_loop(Token loop_token);
        RangeLoopStmt*     parse_range_loop(Token loop_token, Node* init_node);
        IteratorLoopStmt*  parse_iterator_loop(Token loop_token, Var* var);
        LoopControlStmt*   parse_loop_control();
        SwitchStmt*        parse_switch();
        RaiseStmt*         parse_raise();

        ScopeStmt* parse_scope(const char* closing_for = nullptr);
        static void add_node_to_scope(ScopeStmt* scope, Node* node);

        void parse_comptime_file_info();

        // Expression parsing
        //--------------------------------------

        Type* parse_type();
        Type* parse_type_for_decl();
        Type* parse_base_type();
        Type* construct_type_from_identifier(Token name_token, bool is_const);
        Type* construct_unresolved_bracket_type(Type* base_type,
                                                const llvm::SmallVector<Expr*, 8>& exprs);
        Type* parse_optional_type_trailing_info(Type* type);
        Type* parse_function_type(Type* base_type);

        Expr* parse_assignment_and_expr();
        Expr* parse_assignment_and_expr(Expr* lhs);
        Expr* parse_expr();
        Expr* parse_expr(Expr* lhs);
        Expr* parse_binary_expr(Expr* lhs);
        std::pair<Token, Token> split_number_from_sign(Token token);
        Expr* fold_number(Token op, Expr* lhs, Expr* rhs);
        template<typename T>
        Expr* fold_int(Token op, Number* lhs, Number* rhs, Type* to_type);
        template<typename T>
        Expr* fold_float(Token op, Number* lhs, Number* rhs, Type* to_type, T lval, T rval);
        Expr* report_overflow(Token op, Expr* lhs, Expr* rhs, Type* to_type);
        Expr* report_underflow(Token op, Expr* lhs, Expr* rhs, Type* to_type);
        Expr* parse_postfix();
        Expr* parse_postfix(Expr* term);
        FuncCall* parse_function_call(Expr* site);
        Expr* parse_dot_operator(Expr* site);
        Expr* parse_memory_access(Expr* site);
        Expr* parse_term();

        Number* parse_int_literal();
        Number* parse_hex_literal();
        Number* parse_bin_literal();
        Number* parse_oct_literal();
        Number* parse_float32_literal();
        Number* parse_float64_literal();
        void report_float_error(FloatParseError parse_error);
        Expr* parse_string8bit_literal();
        Expr* parse_string16bit_literal();
        Expr* parse_string32bit_literal();
        Expr* parse_char_literal();

        template<uint32_t radix, uint64_t convert_table[256], bool use_table = true>
        Number* parse_number_literal(const char* start, const char* end);

        Expr* parse_array();
        Expr* parse_struct_initializer(IdentRef* ref);

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
        bool expect(tokkind kind, const char* for_msg = nullptr);

        // Expect the current token to be an identifier and construct
        // an identifier object if it is. It then consumes the token.
        Identifier expect_identifier(const char* for_msg = nullptr);

        bool peek_if_expr_is_type(Token tok0, Token tok1);

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

        Expr* new_binary_op(Token op_tok, Expr* lhs, Expr* rhs);

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
        void skip_recovery(bool stop_on_modifiers = true);

    };
}

#endif // PARSER_H