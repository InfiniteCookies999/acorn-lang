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

    class Sema {
    public:

        Sema(Context& context, Module& modl, Logger& logger);

        static void resolve_global_comptime(Context& context, Module& modl);

        static bool is_potential_main_function(Context& context, const Func* canidate);
        static bool find_main_function(Context& context);

        static void check_for_duplicate_functions(Module& modl);
        static bool check_for_duplicate_match(const Func* func1, const Func* func2);

        void check_function(Func* func);
        void check_variable(Var* var);

    private:
        Context&   context;
        Logger&    logger;
        Module&    modl;
        TypeTable& type_table;

        Func* cur_func;

        bool is_global_comptime = false;

        // A structure to keep track of current scope information
        // to help report errors.
        //
        struct SemScope {
            SemScope* parent = nullptr;
            // If true then on every possible branch
            // path there exists a return statement.
            bool all_paths_return = false;

            llvm::SmallVector<Var*> variables;

            // Recursively ascends the current function stack to
            // find the variable or returns nullptr.
            Var* find_variable(Identifier name) const;

        } * cur_scope = nullptr;

        void check_node(Node* node);

        // Statement checking
        //--------------------------------------

        void check_return(ReturnStmt* ret);
        void check_if(IfStmt* ifs, bool& all_paths_return);
        void check_comptime_if(ComptimeIfStmt* ifs);

        void check_scope(ScopeStmt* scope, SemScope& new_sem_scope);

        // Expression checking
        //--------------------------------------        

        void check_binary_op(BinOp* bin_op);
        void check_unary_op(UnaryOp* unary_op);
        void check_ident_ref(IdentRef* ref, bool is_for_call);
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

        // Utility functions
        //--------------------------------------

        bool is_assignable_to(Type* to_type, Expr* expr) const;
        bool is_castable_to(Type* to_type, Expr* expr) const;
        bool try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr) const;
        void check_modifiable(Expr* expr);
        bool is_lvalue(Expr* expr);
        void check_division_by_zero(PointSourceLoc error_loc, Expr* expr);
        void create_cast(Expr* expr, Type* to_type);
        bool check_condition(Expr* cond);
        bool is_condition(Expr* cond) const;
        void check_modifier_incompatibilities(Decl* decl);

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