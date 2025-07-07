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
            return lexer.get_total_lines_lexed();
        }

        int get_whitespace_lines_parsed() const {
            return lexer.get_whitespace_lines_lexed();
        }

    private:
        Context&       context;
        Module&        modl;
        PageAllocator& allocator;
        SourceFile*    file;
        TypeTable&     type_table;

        Logger& logger;

        Lexer lexer;
        Token cur_token;
        Token prev_token;

        Token  peeked_tokens[MAX_PEEKED_TOKENS];
        size_t peeked_size = 0;

        VarList var_list;

        Func*      cur_func      = nullptr;
        Struct*    cur_struct    = nullptr;
        Var*       cur_var = nullptr;
        Interface* cur_interface = nullptr;
        llvm::SmallVector<Generic*> cur_generics;

        // Linkage name set by the native modifier.
        llvm::StringRef linkname;

        int paren_count   = 0;
        int bracket_count = 0;

        // If true still parsing imports at the top of the file.
        bool parsing_import_tops      = true;
        bool allow_struct_initializer = true;

        // Statement processing
        //--------------------------------------

        template<typename D, bool uses_linkname>
        D* new_declaration(uint32_t modifiers, Token loc_token);

        void add_node_to_global_scope(Node* node);
        void add_node_to_struct(Struct* structn, Node* node);
        void add_node_to_interface(Interface* interfacen, Node* node);
        static void add_node_to_scope(ScopeStmt* scope, Node* node);


        // Statement parsing
        //--------------------------------------

        void parse_comptime_file_info();

        void        parse_import_top();
        ImportStmt* parse_import();

        Node* parse_statement();

        Func*      parse_function(uint32_t modifiers, bool is_const);
        VarList*   parse_variable_list(uint32_t modifiers);
        Var*       parse_variable(uint32_t modifiers, bool may_parse_implicit_ptr = false);
        Struct*    parse_struct(uint32_t modifiers);
        Enum*      parse_enum(uint32_t modifiers);
        Interface* parse_interface(uint32_t modifiers);
        void       check_composite_name_conflict_with_imports(Identifier name, const char* composite_type_str);
        void       parse_raised_errors(llvm::SmallVector<RaisedError>& raised_errors);
        uint32_t   parse_modifiers();
        void       parse_generics(llvm::SmallVector<acorn::Generic*>& generics);

        ScopeStmt*  parse_scope(const char* closing_for = nullptr);
        ReturnStmt* parse_return();
        IfStmt*     parse_if();
        void        parse_comptime_if(bool chain_start = true, bool takes_path = true);
        Node*       parse_loop();
        Node*       parse_iterator_loop(Token loop_token, Var* var, bool var_as_pointer);
        Node*       parse_range_loop(Token loop_token, Node* init_node);
        Node*       parse_loop_control();
        Node*       parse_switch();
        Node*       parse_raise();
        Node*       parse_recover();

        // Type parsing
        //--------------------------------------

        struct ArrayTypeExpr {
            union {
                SourceLoc empty_elm_loc;
                Expr*     expr;
            };
            bool has_expr;
        };
        Type* parse_type();
        Type* parse_base_type();
        Type* parse_trailing_type_info(Type* type);
        Type* construct_array_type(Type* base_type, const llvm::SmallVector<ArrayTypeExpr, 8>& exprs);

        // Expression parsing
        //--------------------------------------

        Expr* parse_assignment_and_expr();
        Expr* parse_expr();
        Expr* parse_try(Var* tried_on_var = nullptr);

        Expr*                   parse_binary_expr(Expr* lhs);
        Expr*                   new_binary_op(Token op_tok, Expr* lhs, Expr* rhs);
        std::pair<Token, Token> split_number_from_sign(Token token);
        Expr*                   fold_number(Token op, Expr* lhs, Expr* rhs);
        template<typename T>
        Expr* fold_int(Token op, Number* lhs, Number* rhs, Type* to_type);
        template<typename T>
        Expr* fold_float(Token op, Number* lhs, Number* rhs, Type* to_type, T lval, T rval);
        Expr* report_overflow(Token op, Expr* lhs, Expr* rhs, Type* to_type);
        Expr* report_underflow(Token op, Expr* lhs, Expr* rhs, Type* to_type);

        Expr*     parse_expr_trail();
        Expr*     parse_expr_trail(Expr* lhs);
        FuncCall* parse_function_call(Expr* site);
        Expr*     parse_generic_function_bind_call();
        void      parse_function_call_args(llvm::SmallVector<Expr*>& args, size_t& non_named_args_offset);
        Expr*     parse_term();

        Number* parse_int_literal();
        Number* parse_hex_literal();
        Number* parse_bin_literal();
        Number* parse_oct_literal();

        template<uint32_t radix, uint64_t convert_table[256], bool use_table = true>
        Number* parse_int_number(const char* start, const char* end);

        Number* parse_float_literal();
        Number* parse_double_literal();
        void report_float_error(FloatParseError parse_error);

        Expr* parse_string_literal();
        Expr* parse_char_literal();
        void convert_and_check_codepoint(uint32_t     codepoint,
                                         std::string& dest_string,
                                         const char*  ptr,
                                         int          ptr_offset);

        Expr* parse_array();
        Expr* parse_struct_initializer(Expr* site);
        Expr* parse_type_expr();

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

        // Search the current generics list of the generic type.
        Type* find_generic_type(Identifier name) const;

        // Expect the current token to be an identifier and construct
        // an identifier object if it is. It then consumes the token.
        Identifier expect_identifier(const char* for_msg = nullptr);

        // Given look ahead tokens tok0 and tok1 determines if the
        // next characters would result in the expression being a type
        // expression.
        bool peek_if_expr_is_type(Token tok0, Token tok1);

        // Continues to skip tokens until it can find a new valid
        // location to start parsing again.
        void skip_recovery(bool stop_on_modifiers = true);

        template<typename T>
        T* new_node(Token token) {
            return new_node<T>(token.loc);
        }

        template<typename T>
        T* new_node(SourceLoc loc) {
            T* node = static_cast<T*>(allocator.allocate(sizeof(T)));
            new (node) T(); // Call the constructor.
            node->loc = loc;
            return node;
        }

        // Error reporting
        //--------------------------------------

        template<typename... TArgs>
        [[nodiscard]] Logger& error(SourceLoc error_loc, const char* fmt, TArgs&&... args) {
            return logger.begin_error(error_loc, fmt, std::forward<TArgs>(args)...);
        }

        template<typename... TArgs>
        [[nodiscard]] Logger& error(Token error_token, const char* fmt, TArgs&&... args) {
            return error(error_token.loc, fmt, std::forward<TArgs>(args)...);
        }

    };
}

#endif // PARSER_H
