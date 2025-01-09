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
    class FunctionType;
    class PointerType;

    class Sema {
    public:

        Sema(Context& context, SourceFile* file, Logger& logger);

        static bool is_potential_main_function(Context& context, const Func* canidate);
        static bool find_main_function(Context& context);

        static void check_for_duplicate_functions(Namespace* nspace, Context& context);
        static void check_for_duplicate_functions(const FuncList& funcs, Context& context);
        static bool check_for_duplicate_match(const Func* func1, const Func* func2);
        static void check_all_other_duplicates(Module& modl, Context& context);
        static void report_redeclaration(const Decl* decl1, const Decl* decl2, const char* node_kind_str, ErrCode error_code);

        static void check_nodes_wrong_scopes(Module& modl);
        
        static void resolve_imports(Context& context, SourceFile* file);
        static void resolve_import(Context& context, ImportStmt* importn);

        bool check_comptime_cond(Expr* cond);

        void check_function(Func* func);
        void check_variable(Var* var);
        void check_struct(Struct* structn);
        void check_enum(Enum* enumn);

    private:
        Context&    context;
        Logger&     logger;
        Module&     modl;
        Namespace*  nspace;
        SourceFile* file;
        TypeTable&  type_table;

        Func*   cur_func       = nullptr;
        Var*    cur_global_var = nullptr;
        Struct* cur_struct     = nullptr;
        Enum*   cur_enum       = nullptr;

        bool is_comptime_if_cond = false;
        bool should_request_gen_queue;

        // How many nested loops currently within.
        int loop_depth   = 0;

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

        bool check_function_decl(Func* func);

        void check_node(Node* node);

        Type* fixup_type(Type* type);
        Type* fixup_unresolved_bracket_type(Type* type);
        Type* fixup_assign_det_arr_type(Type* type, Var* var);
        Type* fixup_unresolved_composite_type(Type* type);
        Type* fixup_function_type(Type* type);

        // Statement checking
        //--------------------------------------

        void check_return(ReturnStmt* ret);
        void check_if(IfStmt* ifs, bool& all_paths_return);
        void check_predicate_loop(PredicateLoopStmt* loop);
        void check_range_loop(RangeLoopStmt* loop);
        void check_iterator_loop(IteratorLoopStmt* loop);
        void check_loop_control(LoopControlStmt* loop_control);
        void check_loop_scope(ScopeStmt* scope, SemScope* sem_scope);
        void check_switch(SwitchStmt* switchn);
        void check_struct_initializer(StructInitializer* initializer);
        void check_this(This* thisn);
        void check_sizeof(SizeOf* sof);
        void check_moveobj(MoveObj* move_obj, bool has_destination_address);

        SemScope push_scope();
        void pop_scope();
        void check_scope(ScopeStmt* scope);
        // Use this version only if the caller sets up and tears down its own sem_scope.
        void check_scope(ScopeStmt* scope, SemScope* sem_scope);

        void add_variable_to_local_scope(Var* var);

        // Expression checking
        //--------------------------------------        

        void check_binary_op(BinOp* bin_op);
        void check_binary_op_for_enums(BinOp* bin_op, EnumType* lhs_enum_type, EnumType* rhs_enum_type);
        void report_binary_op_cannot_apply(BinOp* bin_op, Expr* expr);
        void report_binary_op_mistmatch_types(BinOp* bin_op);
        Type* get_integer_type_for_binary_op(bool enforce_lhs,
                                             BinOp* bin_op, 
                                             Type* lhs_type,
                                             Type* rhs_type) const;
        void check_unary_op(UnaryOp* unary_op);
        template<bool is_spell_checking = false>
        void check_ident_ref(IdentRef* ref, Namespace* search_nspace, bool is_for_call);
        void check_dot_operator(DotOperator* dot, bool is_for_call);
        void check_function_call(FuncCall* call);
        void check_function_type_call(FuncCall* call, FunctionType* func_type);
        Func* check_function_decl_call(Expr* call_node,
                                       llvm::SmallVector<Expr*>& args,
                                       size_t non_named_args_offset,
                                       FuncList& candidates);
        Func* find_best_call_candidate(FuncList& candidates,
                                       llvm::SmallVector<Expr*>& args,
                                       bool& selected_implicitly_converts_ptr_arg);
        uint32_t get_function_call_score(const Func* candidate, const llvm::SmallVector<Expr*>& args) const;
        enum class CallCompareStatus {
            INCORRECT_ARGS,
            INCORRECT_PARAM_BY_NAME_NOT_FOUND,
            OUT_OF_ORDER_PARAMS,
            ARGS_NOT_ASSIGNABLE,
            SUCCESS
        };
        // Tells weather or not the function is callable and gathers information
        // to indicate if calling the function would be more viable to call than
        // a different overloaded version of the function.
        //
        // @return true if the function is callable with the given arguments.
        template<bool for_score_gathering>
        CallCompareStatus compare_as_call_candidate(const Func* candidate,
                                                    const llvm::SmallVector<Expr*>& args,
                                                    uint32_t& mimatched_types,
                                                    uint32_t& not_assignable_types,
                                                    bool& implicitly_converts_ptr_arg) const;
        bool has_correct_number_of_args(const Func* candidate, 
                                        const llvm::SmallVector<Expr*>& args) const;
        void display_call_mismatch_info(PointSourceLoc error_loc,
                                        const FuncList& candidates,
                                        const llvm::SmallVector<Expr*>& args) const;
        // Displays information for why trying to call a function failed.
        template<typename F>
        void display_call_mismatch_info(const F* candidate,
                                        const llvm::SmallVector<Expr*>& args,
                                        bool indent) const;

        void check_cast(Cast* cast);
        void check_named_value(NamedValue* named_value);
        void check_array(Array* arr, Type* dest_elm_type);
        void check_memory_access(MemoryAccess* mem_access);
        void check_ternary(Ternary* ternary);
        void check_type_expr(TypeExpr* type_expr);
        void check_reflect(Reflect* reflect);

        void ensure_global_variable_checked(SourceLoc error_loc, Var* var);
        bool ensure_struct_checked(SourceLoc error_loc, Struct* structn);
        void ensure_enum_checked(SourceLoc error_loc, Enum* enumn);

        // Utility functions
        //--------------------------------------

        bool is_assignable_to(Type* to_type, Expr* expr) const;
        bool is_castable_to(Type* to_type, Expr* expr) const;
        bool try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr) const;
        bool has_valid_constness(Type* to_type, Type* from_type) const;
        void check_modifiable(Expr* expr);
        bool is_lvalue(Expr* expr) const;
        bool is_readonly_field_without_access(Expr* expr) const;
        bool is_incomplete_type(Type* type) const;
        bool is_incomplete_statement(Node* stmt) const;
        bool may_implicitly_convert_return_ptr(Type* to_type, FuncCall* call) const;
        bool may_implicitly_convert_ptr(PointerType* ptr_type, Expr* from_expr) const;
        void check_division_by_zero(Node* error_node, Expr* expr);
        void create_cast(Expr* expr, Type* to_type);
        bool check_is_condition(Expr* cond);
        bool is_condition(Type* type) const;
        void check_modifier_incompatibilities(Decl* decl);
        void display_circular_dep_error(SourceLoc error_loc, Decl* dep, const char* msg, ErrCode error_code);
        void report_error_cannot_use_variable_before_assigned(SourceLoc error_loc, Var* var);
        Type* get_type_of_type_expr(Expr* expr);

        Decl* find_composite(Identifier name);

        llvm::Constant* gen_constant(Expr* expr);
        
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