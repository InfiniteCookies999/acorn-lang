#ifndef SEMA_H
#define SEMA_H

#include "AST.h"
#include "Logger.h"

namespace llvm {
    class Constant;
}

namespace acorn {

    class Context;
    class Module;
    class TypeTable;
    class SourceFile;

    class Sema {
    public:

        Sema(Context& context, SourceFile* file, Logger& logger);

        static void resolve_global_comptime(Context& context, Module& modl);

        static bool is_potential_main_function(Context& context, const Func* canidate);
        static bool find_main_function(Context& context);

        static void check_for_duplicate_functions(Module& modl);
        static bool check_for_duplicate_match(const Func* func1, const Func* func2);
        static void check_for_duplicate_variables(Module& modl);
        static void report_redeclaration(const Decl* decl1, const Decl* decl2, const char* node_kind_str, ErrCode error_code);

        static void check_nodes_wrong_scopes(Module& modl);
        
        static void resolve_imports(Context& context, Module& modl);
        static void resolve_import(Context& context, ImportStmt* importn);

        void check_function(Func* func);
        void check_variable(Var* var);

    private:
        Context&    context;
        Logger&     logger;
        Module&     modl;
        SourceFile* file;
        TypeTable&  type_table;

        Func* cur_func;
        Var*  cur_global_var = nullptr;

        bool is_global_comptime = false;
        // How many nested loops currently within.
        int loop_depth = 0;

        // A structure to keep track of current scope information
        // to help report errors.
        //
        struct SemScope {
            SemScope* parent = nullptr;
            // If true then on every possible branch
            // path there exists a return statement.
            bool all_paths_return = false;
            // True when encountering a statement that
            // branches.
            bool found_terminal = false;

            llvm::SmallVector<Var*> variables;

            // Recursively ascends the current function stack to
            // find the variable or returns nullptr.
            Var* find_variable(Identifier name) const;

        } * cur_scope = nullptr;

        void check_node(Node* node);

        Type* fixup_type(Type* type);

        // Statement checking
        //--------------------------------------

        void check_return(ReturnStmt* ret);
        void check_if(IfStmt* ifs, bool& all_paths_return);
        void check_comptime_if(ComptimeIfStmt* ifs);
        void check_predicate_loop(PredicateLoopStmt* loop);
        void check_range_loop(RangeLoopStmt* loop);
        void check_iterator_loop(IteratorLoopStmt* loop);
        void check_loop_control(LoopControlStmt* loop_control);
        void check_loop_scope(ScopeStmt* scope, SemScope* sem_scope);

        SemScope push_scope();
        void pop_scope();
        void check_scope(ScopeStmt* scope);
        // Use this version only if the caller sets up and tears down its own sem_scope.
        void check_scope(ScopeStmt* scope, SemScope* sem_scope);

        // Expression checking
        //--------------------------------------        

        void check_binary_op(BinOp* bin_op);
        void check_unary_op(UnaryOp* unary_op);
        void check_ident_ref(IdentRef* ref, Module* search_modl, bool is_for_call);
        void check_dot_operator(DotOperator* dot, bool is_for_call);
        void check_function_call(FuncCall* call);
        Func* find_best_call_canidate(FuncList& canidates,
                                      llvm::SmallVector<Expr*, 8>& args);
        // Tells weather or not the function is callable and gathers information
        // to indicate if calling the function would be more viable to call than
        // a different overloaded version of the function.
        //
        // @return true if the function is callable with the given arguments.
        bool compare_as_call_canidate(Func* canidate,
                                      llvm::SmallVector<Expr*, 8>& args,
                                      uint32_t& mimatched_types);
        void display_call_mismatch_info(PointSourceLoc error_loc,
                                        const FuncList& canidates,
                                        const llvm::SmallVector<Expr*, 8>& args) const;
        // Displays information for why trying to call a function failed.
        void display_call_mismatch_info(const Func* canidate,
                                        const llvm::SmallVector<Expr*, 8>& args,
                                        bool indent) const;
        void check_cast(Cast* cast);
        void check_named_value(NamedValue* named_value);
        void check_array(Array* arr);
        void check_memory_access(MemoryAccess* mem_access);

        void check_global_variable(SourceLoc error_loc, Var* var);

        // Utility functions
        //--------------------------------------

        bool is_assignable_to(Type* to_type, Expr* expr) const;
        bool is_castable_to(Type* to_type, Expr* expr) const;
        bool try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr) const;
        bool has_valid_constness(Type* to_type, Type* from_type) const;
        void check_modifiable(Expr* expr);
        bool is_lvalue(Expr* expr);
        void check_division_by_zero(PointSourceLoc error_loc, Expr* expr);
        void create_cast(Expr* expr, Type* to_type);
        bool check_condition(Expr* cond);
        bool is_condition(Type* type) const;
        void check_modifier_incompatibilities(Decl* decl);
        void display_circular_dep_error(SourceLoc error_loc, Decl* dep, const char* msg, ErrCode error_code);

        llvm::Constant* gen_constant(PointSourceLoc error_loc, Expr* expr);
        
        std::string get_type_mismatch_error(Type* to_type, Expr* expr) const;
        std::string get_type_mismatch_error(Type* to_type, Type* from_type) const;
        template<typename... TArgs>
        [[nodiscard]] Logger& error(PointSourceLoc loc, const char* fmt, TArgs... args) {
            return logger.begin_error(loc, fmt, std::forward<TArgs>(args)...);
        }

        template<typename... TArgs>
        [[nodiscard]] Logger& error(SourceLoc loc, const char* fmt, TArgs... args) {
            return logger.begin_error(loc, fmt, std::forward<TArgs>(args)...);
        }

        template<typename... TArgs>
        [[nodiscard]] Logger& error(Node* node, const char* fmt, TArgs... args) {
            return error(node->loc, fmt, std::forward<TArgs>(args)...);
        }

    };
}

#endif // SEMA_H