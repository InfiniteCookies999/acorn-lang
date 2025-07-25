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
    class ErrorSpellChecker;

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

        static void report_nodes_wrong_scopes(Module& modl);

        static void resolve_imports(Context& context, SourceFile* file);
        static void resolve_import(Context& context, ImportStmt* importn);

        bool check_comptime_cond(Expr* cond, const char* comptime_type_str);

        void check_function(Func* func);
        void check_variable(Var* var);
        void check_struct(Struct* structn);
        void check_enum(Enum* enumn);
        void check_interface(Interface* interfacen);

    private:
        Context&    context;
        Logger&     logger;
        Module&     modl;
        Namespace*  nspace;
        SourceFile* file;
        TypeTable&  type_table;

        Func*      cur_func       = nullptr;
        Func*      cur_func_decl  = nullptr; // current function when checking its declaration.
        Var*       cur_global_var = nullptr;
        Struct*    cur_struct     = nullptr;
        Enum*      cur_enum       = nullptr;
        Interface* cur_interface  = nullptr;

        FuncList temp_ref_functions;

        // Limits to calculate comparison scores for which function to call.
        //
        //     const uint32_t IMPLICIT_MISMATCHED_TYPES_LIMIT = 0;
        static const uint64_t PREFER_NON_CONST_LIMIT                = MAX_FUNC_PARAMS * 2;
        static const uint64_t IS_VARARGS_LIMIT                      = PREFER_NON_CONST_LIMIT * 2;
        static const uint64_t IS_GENERIC_LIMIT                      = IS_VARARGS_LIMIT * 2;
        static const uint64_t IMPLICIT_CAST_TO_ANY_LIMIT            = IS_GENERIC_LIMIT * MAX_FUNC_PARAMS * 2;

        static const uint64_t NON_CONST_FROM_CONST_OBJ_LIMIT        = IMPLICIT_CAST_TO_ANY_LIMIT * 2;
        static const uint64_t NOT_ASSIGNABLE_TYPES_LIMIT            = NON_CONST_FROM_CONST_OBJ_LIMIT * 2;
        static const uint64_t CANNOT_USE_VARARGS_AS_NAMED_ARG_LIMIT = NOT_ASSIGNABLE_TYPES_LIMIT * MAX_FUNC_PARAMS * 2;

        // These get the same value because there is no preference of one over the other.
        static const uint64_t INCORRECT_PARAM_NAME_OR_ORD_LIMIT     = CANNOT_USE_VARARGS_AS_NAMED_ARG_LIMIT * MAX_FUNC_PARAMS * 2;
        static const uint64_t INCORRECT_NUM_ARGS_LIMIT              = CANNOT_USE_VARARGS_AS_NAMED_ARG_LIMIT * MAX_FUNC_PARAMS * 2;
        static const uint64_t CANNOT_ACCESS_PRIVATE_LIMIT           = CANNOT_USE_VARARGS_AS_NAMED_ARG_LIMIT * MAX_FUNC_PARAMS * 2;
        static const uint64_t FORWARD_VARIADIC_WITH_OTHERS_LIMIT    = CANNOT_USE_VARARGS_AS_NAMED_ARG_LIMIT * MAX_FUNC_PARAMS * 2;

        bool is_comptime_if_cond     = false;

        // How many nested loops currently within.
        int loop_depth = 0;

        Try* catch_scope_try = nullptr;


        // When calling a generic function and qualifying the types this
        // is set to be used during type fixup in order to qualify the
        // generic types.
        llvm::SmallVector<Type*>* func_call_generic_bindings = nullptr;

        // A structure to keep track of current scope information
        // to help report errors.
        //
        struct SemScope {
            SemScope* parent = nullptr;
            // If true then on every possible branch
            // path there exists a return statement.
            bool all_paths_return = false;
            // If true then on every possible branch
            // path there exists a branch statement
            // such as 'break', 'return', ect...
            bool all_paths_branch = false;
            // True when encountering a statement that
            // branches.
            //
            // TODO (maddie): is this needed now that there
            // is `all_paths_branch`?
            bool found_terminal = false;

            Try* cur_try = nullptr;

            llvm::SmallVector<Var*> variables;

            // Recursively ascends the current function stack to
            // find the variable or returns nullptr.
            Var* find_variable(Identifier name) const;

        } * cur_scope = nullptr;

        // Declaration checking
        //--------------------------------------

        bool check_function_decl(Func* func);
        bool check_raised_error(RaisedError& raised_error);

        void ensure_global_variable_checked(SourceLoc error_loc, Var* var);
        bool ensure_struct_checked(SourceLoc error_loc, Struct* structn);
        void ensure_enum_checked(SourceLoc error_loc, Enum* enumn);
        void ensure_interface_checked(SourceLoc error_loc, Interface* interfacen);

        void check_modifier_incompatibilities(Decl* decl);
        void check_modifiers_for_composite(Decl* decl, const char* composite_type_str);

        template<bool check_for_interface>
        static bool do_functions_match(const Func* func1, const Func* func2);

        void check_struct_interface_extension(Struct* structn,
                                              Interface* interfacen,
                                              const Struct::UnresolvedExtension& extension);
        bool do_interface_functions_matches(Func* interface_func, Func* func);

        // Statements checking
        //--------------------------------------

        void check_node(Node* node);

        void check_scope(ScopeStmt* scope);
        // Use this version only if the caller sets up and tears down its own sem_scope.
        void check_scope(ScopeStmt* scope, SemScope* sem_scope);

        void check_return(ReturnStmt* ret);
        void check_if(IfStmt* ifs, bool& all_paths_return, bool& all_paths_branch);
        void check_predicate_loop(PredicateLoopStmt* loop);
        void check_range_loop(RangeLoopStmt* loop);
        void check_iterator_loop(IteratorLoopStmt* loop);
        void check_loop_control(LoopControlStmt* loop_control);
        void check_loop_scope(ScopeStmt* scope, SemScope* sem_scope);
        void check_switch(SwitchStmt* switchn);
        void check_raise(RaiseStmt* raise);
        void check_try(Try* tryn, bool assigns);
        void check_recover(RecoverStmt* recover);

        // Expression checking
        //--------------------------------------

        void check_binary_op(BinOp* bin_op);
        // Enums have special checks because when enum values have operations applied the resulting
        // type is an enum container type.
        void check_binary_op_for_enums(BinOp* bin_op, EnumType* lhs_enum_type, EnumType* rhs_enum_type);
        void check_constant_range_for_bigger_lhs(BinOp* bin_op, Type* result_type);
        Type* get_integer_type_for_binary_op(bool enforce_lhs,
                                             BinOp* bin_op,
                                             Type* lhs_type,
                                             Type* rhs_type) const;
        void check_unary_op(UnaryOp* unary_op);
        template<bool is_spell_checking = false>
        void check_ident_ref(IdentRef* ref,
                             Namespace* search_nspace,
                             StructType* search_struct_type,
                             bool is_for_call,
                             bool is_dot_op_site = false);
        void check_dot_operator(DotOperator* dot, bool is_for_call);
        void check_function_call(FuncCall* call);
        void check_generic_bind_function_call(GenericBindFuncCall* call);
        bool compare_generic_bind_candidate_with_named_args(const llvm::SmallVector<Expr*>& args,
                                                            const llvm::SmallVector<Generic*>& generics,
                                                            llvm::SmallVector<Type*>& bound_types);
        bool check_generic_bind_arguments(const llvm::SmallVector<Expr*>& args,
                                          bool& uses_named_values);
        void check_function_type_call(FuncCall* call, FunctionType* func_type);
        Func* check_function_decl_call(Expr* call_node,
                                       llvm::SmallVector<Expr*>& args,
                                       size_t non_named_args_offset,
                                       FuncList& candidates,
                                       bool is_const_object,
                                       const llvm::SmallVector<Type*>& pre_bound_types,
                                       Struct* generic_parent_struct,
                                       Struct*& fully_bound_parent_struct);
        Func* find_best_call_candidate(FuncList& candidates,
                                       llvm::SmallVector<Expr*>& args,
                                       bool& selected_implicitly_converts_ptr_arg,
                                       bool& is_ambiguous,
                                       bool is_const_object,
                                       llvm::SmallVector<Type*>& generic_bindings,
                                       const llvm::SmallVector<Type*>& pre_bound_types);
        enum class CallCompareStatus {
            INCORRECT_ARGS,
            INCORRECT_PARAM_BY_NAME_NOT_FOUND,
            CANNOT_ACCESS_PRIVATE,
            OUT_OF_ORDER_PARAMS,
            ARGS_NOT_ASSIGNABLE,
            NON_CONST_FROM_CONST_OBJECT,
            CANNOT_USE_VARARGS_AS_NAMED_ARG,
            FORWARD_VARIADIC_WITH_OTHERS,
            SUCCESS
        };

        // Tells weather or not the function is callable and gathers information
        // to indicate if calling the function would be more viable to call than
        // a different overloaded version of the function.
        //
        // @return true if the function is callable with the given arguments.
        template<bool for_score_gathering, bool checking_generic>
        CallCompareStatus compare_as_call_candidate(const Func* canidate,
                                                    const llvm::SmallVector<Expr*>& args,
                                                    const bool is_const_object,
                                                    uint64_t& score,
                                                    bool& implicitly_converts_ptr_arg,
                                                    llvm::SmallVector<Type*>& generic_bindings);
        bool has_correct_number_of_args(const Func* candidate,
                                        const llvm::SmallVector<Expr*>& args) const;
        bool check_bind_type_to_generic_type(Type* to_type,   // Type at current level of comparison
                                             Type* from_type, // Type at current level of comparison
                                             llvm::SmallVector<Type*>& bindings,
                                             bool enforce_elm_const = false);
        void check_cast(Cast* cast);
        void check_named_value(NamedValue* named_value);
        void check_array(Array* arr, Type* dest_elm_type);
        void check_memory_access(MemoryAccess* mem_access);
        void check_ternary(Ternary* ternary);
        void check_type_expr(TypeExpr* type_expr);
        void check_reflect(Reflect* reflect);
        void check_struct_initializer(StructInitializer* initializer);
        void check_this(This* thisn);
        void check_sizeof(SizeOf* sof);
        void check_moveobj(MoveObj* move_obj, bool has_destination_address);
        bool check_modifiable(Expr* expr, Expr* error_node, bool is_assignment = true);
        Expr* try_create_bound_expr_for_variable_with_indetermite_type(Var* param);
        void check_division_by_zero(Node* error_node, Expr* expr);
        bool check_is_condition(Expr* cond);

        // Type fixup
        //--------------------------------------

        Type* fixup_type(Type* type, bool is_ptr_elm_type = false);
        Type* fixup_unresolved_array_type(Type* type);
        Type* fixup_assign_det_arr_type(Type* type, Var* var);
        Type* fixup_unresolved_composite_type(Type* type, bool is_ptr_elm_type);
        Type* fixup_unresolved_generic_composite_type(Type* type, bool is_ptr_elm_type);
        Type* fixup_unresolved_generic_composite_type(Decl* found_composite,
                                                      SourceLoc error_loc,
                                                      const llvm::SmallVector<Expr*>& bound_exprs);
        bool get_bound_types_for_generic_type(Decl* found_composite,
                                              SourceLoc error_loc,
                                              const llvm::SmallVector<Expr*>& bound_exprs,
                                              llvm::SmallVector<Type*>& bound_types);

        Type* fixup_unresolved_enum_value_type(Type* type, bool is_ptr_elm_type);
        Decl* find_composite_for_composite_type(Identifier name, SourceLoc error_loc);
        Type* fixup_function_type(Type* type);
        Type* fixup_generic_type(Type* type);
        Type* fixup_partially_bound_struct_type(Type* type, const llvm::SmallVector<Type*>* bound_types);


        // Error reporting
        //--------------------------------------

        void spellcheck_variables_for_ident(const llvm::SmallVector<Var*>& variables,
                                            ErrorSpellChecker& spell_checker,
                                            bool is_for_call);
        void spellcheck_variables_for_ident(const llvm::DenseMap<Identifier, Var*>& variables,
                                            ErrorSpellChecker& spell_checker,
                                            bool is_for_call);
        // Displays information for why trying to call a function failed.
        template<typename F>
        void display_call_mismatch_info(const F* candidate,
                                        const llvm::SmallVector<Expr*>& args,
                                        bool indent,
                                        bool should_show_invidual_underlines,
                                        Node* call_node,
                                        const llvm::SmallVector<Type*>& pre_bound_types);
        void display_call_mismatch_info(PointSourceLoc error_loc,
                                        Node* call_node,
                                        const FuncList& candidates,
                                        const llvm::SmallVector<Expr*>& args,
                                        const llvm::SmallVector<Type*>& pre_bound_types);
        uint64_t get_function_call_score(const Func* candidate,
                                         const llvm::SmallVector<Expr*>& args,
                                         bool is_const_object,
                                         const llvm::SmallVector<Type*>& pre_bound_types);

        void display_call_ambiguous_info(PointSourceLoc error_loc,
                                         FuncList& candidates,
                                         llvm::SmallVector<Expr*>& args,
                                         bool is_const_object,
                                         const llvm::SmallVector<Type*>& pre_bound_types);
        std::string get_type_with_generics_where_msg(Type* type_with_generics,
                                                     const llvm::SmallVector<Type*>& generic_bindings,
                                                     bool indent);
        template<unsigned N>
        void display_ambiguous_functions(const llvm::SmallVector<Func*, N>& ambiguous_funcs);
        void display_call_missing_bindings_info(Expr* call_node,
                                                Func* called_func,
                                                const llvm::SmallVector<Type*>& generic_bindings);
        void display_generic_bind_named_args_fail_info(const llvm::SmallVector<Expr*>& args,
                                                       const llvm::SmallVector<Generic*>& generics);
        void report_binary_op_cannot_apply(BinOp* bin_op, Expr* expr);
        void report_binary_op_mistmatch_types(BinOp* bin_op);
        void display_interface_func_mismatch_info(Func* interface_func,
                                                  Func* func,
                                                  bool indent,
                                                  bool should_show_invidual_underlines);
        std::string get_type_mismatch_error(Type* to_type, Expr* expr, bool has_implicit_pointer = false) const;
        std::string get_type_mismatch_error(Type* to_type, Type* from_type, bool has_implicit_pointer = false) const;
        Type* get_array_type_for_mismatch_error(Array* arr) const;
        Type* get_array_type_for_mismatch_error(Array* arr,
                                                llvm::SmallVector<size_t>& lengths,
                                                size_t depth) const;
        void display_circular_dep_error(SourceLoc error_loc, Decl* dep, const char* msg, ErrCode error_code);
        void report_error_cannot_use_variable_before_assigned(SourceLoc error_loc, Var* var);

        template<typename... TArgs>
        void add_error_line(Node* err_node,
                            bool should_show_invidual_underlines,
                            bool indent,
                            const char* fmt,
                            TArgs&&... args);

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


        // Utility functions
        //--------------------------------------

        SemScope push_scope();
        void pop_scope();
        void add_variable_to_local_scope(Var* var);

        bool is_assignable_to(Type* to_type, Expr* expr);
        bool is_castable_to(Type* to_type, Expr* expr);
        bool is_lvalue(Expr* expr) const;
        bool is_readonly_field_without_access(Expr* expr) const;
        bool is_incomplete_type(Type* type) const;
        bool is_incomplete_statement(Node* stmt) const;
        bool may_implicitly_convert_return_ptr(Type* to_type, FuncCall* call) const;
        bool may_implicitly_convert_ptr(PointerType* ptr_type, Expr* from_expr) const;
        void create_cast(Expr* expr, Type* to_type);
        bool is_condition(Type* type) const;
        Type* get_type_of_type_expr(Expr* expr);
        uint64_t get_total_number_of_values_in_range(BinOp* range);
        Decl* find_composite(Identifier name);
        llvm::Constant* gen_constant(Expr* expr);

    };
}

#endif // SEMA_H
