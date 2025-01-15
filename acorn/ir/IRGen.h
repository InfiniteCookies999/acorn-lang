#ifndef IR_GEN_H
#define IR_GEN_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#include "../AST.h"
#include "../Util.h"
#include "GenTypes.h"

#define emit_dbg(x) if (should_emit_debug_info) { x; }

namespace acorn {

    class DebugInfoEmitter;
    class Context;

    class IRGenerator {
    public:

        IRGenerator(Context& context);

        void gen_function(Func* func);
        void gen_global_variable(Var* var);
        void gen_implicit_function(ImplicitFunc* implicit_func);
        void add_return_to_global_init_function();
        void destroy_global_variables();

        llvm::Value* gen_node(Node* node);

        llvm::Value* gen_rvalue(Expr* node);

        llvm::Type* gen_type(Type* type) const {
            return acorn::gen_type(type, ll_context, ll_module);
        }
        llvm::StructType* gen_struct_type(StructType* struct_type) const {
            return acorn::gen_struct_type(struct_type, ll_context, ll_module);
        }

        llvm::Type* gen_ptrsize_int_type() {
            return acorn::gen_ptrsize_int_type(ll_context, ll_module);
        }

    private:
        Context&           context;
        llvm::LLVMContext& ll_context;
        llvm::Module&      ll_module;
        llvm::IRBuilder<>  builder;
        DebugInfoEmitter*  di_emitter;
        bool               should_emit_debug_info;

        Func*           cur_func;
        Struct*         cur_struct;
        llvm::Function* ll_cur_func;
        // If there are multiple returns this is a block that
        // multiple return statements will jump to.
        llvm::BasicBlock* ll_ret_block;
        union {
            // If there is only one return statement this will be
            // used to store the return value.
            llvm::Value* ll_ret_value;
            // When the function returns multiple values this will
            // be the address that will be used to store values to.
            // Each return statement will store its value to this
            // address then jump to the return block (ll_ret_block).
            llvm::Value* ll_ret_addr;
        };

        // When generating code inside a member function this is a pointer
        // to the struct.
        llvm::Value* ll_this;

        llvm::SmallVector<llvm::BasicBlock*, 8> loop_break_stack;
        llvm::SmallVector<llvm::BasicBlock*, 8> loop_continue_stack;

        struct DestructorObject {
            Type*        type;
            llvm::Value* ll_address;
            SourceLoc    loc;
        };

        struct IRScope {
            IRScope* parent = nullptr;
            // All objects currently initialized within the scope unless the scope is
            // the primary scope of a function and no returns have been encountered
            // in which case they are placed in the always_initialized_destructor_objects
            // list.
            //
            llvm::SmallVector<DestructorObject> objects_needing_destroyed;
            // Is the current scope the scope of a loop.
            bool is_loop_scope = false;
        }* ir_scope = nullptr;

        // Objects which have destructors and need to be destroyed
        // and are encountered before any returns or branching is
        // encountered.
        //
        // These objects can always be gaurenteed to be destroyed so
        // any object in this list has its destructor called no matter
        // where a return occured.
        //
        // The entire point to having this list is to reduce the number
        // of instructions needed to be generated since when there are
        // multiple returns the return statements branch to a common
        // function return.
        // This common return can then manage the cleanup of all of these
        // objects.
        llvm::SmallVector<DestructorObject> always_initialized_destructor_objects;

        bool encountered_return = false;

        void gen_implicit_default_constructor(Struct* structn);
        void gen_implicit_destructor(Struct* structn);
        void gen_implicit_copy_constructor(Struct* structn);
        void gen_implicit_move_constructor(Struct* structn);

        void gen_function_decl(Func* func);
        llvm::Type* gen_function_return_type(Func* func, bool is_main);
        void gen_function_body(Func* func);
        void gen_variable_address(Var* var, llvm::Type* ll_alloc_type);
        llvm::AllocaInst* gen_alloca(llvm::Type* ll_alloc_type, llvm::Twine ll_name);
        llvm::Type* gen_function_param_type(Var* param) const;

        void gen_global_variable_decl(Var* var);
        void gen_global_variable_body(Var* var);
        void finish_incomplete_global_variable(Var* var);
        bool gen_constant_struct_for_global(StructType* struct_type, llvm::Constant*& ll_constant_struct);
        llvm::Constant* gen_constant_array_for_global(Array* arr);

        void finish_incomplete_struct_type_global(llvm::Value* ll_address,
                                                  StructType* struct_type,
                                                  const std::function<llvm::Value*()>& address_getter = {});

        void add_object_with_destructor(Type* type, llvm::Value* ll_address);
        void gen_call_destructors(llvm::SmallVector<DestructorObject>& objects);
        void gen_call_destructors(Type* type, llvm::Value* ll_address);
        void gen_call_loc_scope_destructors();
        void process_destructor_state(Type* type, llvm::Value* ll_address);

        llvm::Value* gen_return(ReturnStmt* ret);
        llvm::Value* gen_if(IfStmt* ifs, llvm::BasicBlock* ll_end_bb = nullptr);
        llvm::Value* gen_predicate_loop(PredicateLoopStmt* loop);
        llvm::Value* gen_range_loop(RangeLoopStmt* loop);
        llvm::Value* gen_iterator_loop(IteratorLoopStmt* loop);
        void gen_cond_branch_for_loop(Expr* cond, llvm::BasicBlock* ll_body_bb, llvm::BasicBlock* ll_end_bb);
        llvm::Value* gen_loop_control(LoopControlStmt* loop_control);
        llvm::Value* gen_switch(SwitchStmt* switchn);
        llvm::Value* gen_switch_non_foldable(SwitchStmt* switchn);
        llvm::Value* gen_switch_foldable(SwitchStmt* switchn);
        llvm::Value* gen_struct_initializer(StructInitializer* initializer, llvm::Value* ll_dest_addr, Node* lvalue = nullptr);
        llvm::Value* gen_this(This* thisn);
        llvm::Value* gen_sizeof(SizeOf* sof);
        void gen_scope(ScopeStmt* scope);
        llvm::Value* gen_scope_with_dbg_scope(ScopeStmt* scope);

        llvm::Value* gen_variable(Var* var);
        llvm::Value* gen_number(Number* number);
        llvm::Value* gen_ident_reference(IdentRef* ref);
        llvm::Value* gen_binary_op(BinOp* bin_op);
        llvm::Value* gen_numeric_binary_op(tokkind op, BinOp* bin_op,
                                           llvm::Value* ll_lhs, llvm::Value* ll_rhs);
        llvm::Value* gen_equal(llvm::Value* ll_lhs, llvm::Value* ll_rhs);
        llvm::Value* gen_ternary(Ternary* ternary,
                                 llvm::Value* ll_dest_addr,
                                 Node* lvalue      = nullptr,
                                 bool is_assign_op = false,
                                 bool try_move     = false);
        llvm::Value* gen_reflect(Reflect* reflect);
        llvm::GlobalVariable* gen_reflect_type_info(Type* type);
        llvm::Constant* gen_reflect_type_of_type_info(Type* type);
        llvm::Constant* gen_reflect_type_info_struct_info(StructType* struct_type);
        llvm::Constant* gen_reflect_type_info_field_info(Var* field,
                                                         uint64_t offset_in_bytes);
        llvm::Constant* gen_reflect_type_info_enum_info(EnumType* enum_type);

        llvm::Value* gen_unary_op(UnaryOp* unary_op);
        llvm::Value* gen_function_call(FuncCall* call, llvm::Value* ll_dest_addr, Node* lvalue = nullptr);
        llvm::Value* gen_function_decl_call(Func* called_func,
                                            SourceLoc call_loc,
                                            llvm::SmallVector<Expr*>& args,
                                            llvm::Value* ll_dest_addr,
                                            llvm::Value* ll_in_this,
                                            bool apply_implicit_return_ptr,
                                            Node* lvalue);
        llvm::Value* gen_function_call_arg(Expr* arg);
        llvm::Value* gen_function_call_arg_for_implicit_ptr(Expr* arg);
        llvm::Value* gen_function_type_call(FuncCall* call, llvm::Value* ll_dest_addr, Node* lvalue);
        void gen_call_return_aggr_type_temporary(Type* return_type,
                                                 bool uses_aggr_param,
                                                 llvm::Value*& ll_dest_addr);
        llvm::Value* gen_intrinsic_call(FuncCall* call);
        llvm::Value* gen_bool(Bool* b);
        llvm::Value* gen_string(String* string);
        llvm::Value* gen_null();
        llvm::Value* gen_cast(Cast* cast);
        llvm::Value* gen_array(Array* arr, llvm::Value* ll_dest_addr, Node* lvalue = nullptr);
        llvm::Constant* gen_constant_array(Array* arr, ArrayType* arr_type, llvm::ArrayType* ll_arr_type);
        llvm::Value* gen_memory_access(MemoryAccess* mem_access);
        llvm::Value* gen_dot_operator(DotOperator* dot);

        void gen_assignment(llvm::Value* ll_address,
                            Type* to_type,
                            Expr* value,
                            Node* lvalue = nullptr,
                            bool is_assign_op = false,
                            bool try_move = false);
        void gen_default_value(llvm::Value* ll_address, Type* type, Node* lvalue = nullptr);
        llvm::Constant* gen_zero(Type* type);
        llvm::Constant* gen_one(Type* type);
        llvm::Value* gen_cast(Type* to_type, Expr* value, llvm::Value* ll_value);
        llvm::Value* gen_enum_index(uint64_t index, Type* index_type);
        llvm::Value* gen_enum_value_from_enum_array(EnumType* enum_type, llvm::Value* ll_index);
        llvm::Value* gen_store_array_in_slice(llvm::Type* ll_slice_type,
                                              llvm::Value* ll_slice_addr,
                                              llvm::Value* ll_array_addr,
                                              ArrayType* arr_type);

        llvm::Value* gen_condition(Expr* cond);

        llvm::BasicBlock* gen_bblock(const char* name, llvm::Function* ll_func = nullptr);
        void insert_bblock_at_end(llvm::BasicBlock* ll_bb);

        llvm::Function* gen_no_param_member_function_decl(Struct* structn, llvm::Twine name);
        void gen_call_default_constructor(llvm::Value* ll_address, Struct* structn);

        void gen_abstract_array_loop(Type* base_type,
                                     llvm::Value* ll_arr_start_ptr,
                                     llvm::Value* ll_total_arr_length,
                                     const std::function<void(llvm::PHINode*)>& codegen_cb);
        void gen_abstract_double_array_loop(Type* base_type,
                                            llvm::Value* ll_to_arr_start_ptr,
                                            llvm::Value* ll_from_arr_start_ptr,
                                            llvm::Value* ll_total_arr_length,
                                            const std::function<void(llvm::PHINode*, llvm::PHINode*)>& codegen_cb);

        llvm::Value* gen_isize(uint64_t v);

        void gen_branch_on_condition(Expr* cond, llvm::BasicBlock* ll_true_bb, llvm::BasicBlock* ll_false_bb);

        // This will only unconditionally branch to the given block as long as
        // the current block does not already end in a branch (terminal).
        //
        // This may occur because certain scopes end in a branch such as
        // a return statement and so instead of continuing with the code after
        // the scope it has to jump to the end of the function.
        //
        // Example of such as case:
        //
        // if a > 47 {
        //     ...
        //     return;
        // }
        // // Code that may be not be reached because of the return jump.
        // int a = 5;
        // ...
        //
        void gen_branch_if_not_term(llvm::BasicBlock* ll_bb);
        void gen_branch_if_not_term_with_dbg_loc(llvm::BasicBlock* ll_bb, SourceLoc branch_loc);
        llvm::Twine get_global_name(const char* name);
        llvm::GlobalVariable* gen_const_global_variable(const llvm::Twine& name,
                                                        llvm::Type* ll_type,
                                                        llvm::Constant* ll_initial_value,
                                                        llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage);
        llvm::GlobalVariable* gen_const_global_struct_variable(const char* name,
                                                              llvm::StructType* ll_struct_type,
                                                               llvm::SmallVector<llvm::Constant*>& ll_values,
                                                              llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage);
        llvm::GlobalVariable* gen_global_variable(const llvm::Twine& name,
                                                  llvm::Type* ll_type,
                                                  bool is_const,
                                                  llvm::Constant* ll_initial_value,
                                                  llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage);

        void gen_store_value_to_any(llvm::Value* ll_any_address, Expr* value, llvm::Value* ll_value);

        llvm::Align get_alignment(Type* type) const { return get_alignment(gen_type(type)); }
        llvm::Align get_alignment(llvm::Type* ll_type) const;
        uint64_t sizeof_type_in_bytes(llvm::Type* ll_type) const;

        llvm::Value* gen_array_memory_access(llvm::Value* ll_address, Type* arr_type, Expr* index);
        llvm::Value* gen_array_memory_access(llvm::Value* ll_address, llvm::Type* arr_type, llvm::Value* ll_index);

        llvm::Function* gen_void_function_decl(llvm::Twine ll_name);

        llvm::AllocaInst* gen_unseen_alloca(Type* type, llvm::Twine ll_name);
        llvm::AllocaInst* gen_unseen_alloca(llvm::Type* ll_type, llvm::Twine ll_name);

        bool is_pointer_lvalue(Expr* expr);

        ImplicitFunc* create_implicit_function(ImplicitFunc::ImplicitKind implicit_kind, Struct* structn);

        void copy_struct_field_constructor(Var* field,
                                           llvm::Value* ll_to_struct_address,
                                           llvm::Value* ll_from_struct_address,
                                           llvm::Type* ll_struct_type);
        void try_move_then_copy_struct_field_constructor(Var* field,
                                                         llvm::Value* ll_to_struct_address,
                                                         llvm::Value* ll_from_struct_address,
                                                         llvm::Type* ll_struct_type);

        void gen_copy_struct(llvm::Value* ll_to_address,
                             llvm::Value* ll_from_address,
                             StructType* struct_type);
        void gen_copy_array(llvm::Value* ll_to_address,
                            llvm::Value* ll_from_address,
                            ArrayType* arr_type,
                            Node* lvalue = nullptr);
        void gen_call_copy_constructor(llvm::Value* ll_to_address,
                                       llvm::Value* ll_from_address,
                                       Struct* structn,
                                       Node* lvalue = nullptr);
        void gen_call_array_copy_constructors(llvm::Value* ll_to_address,
                                              llvm::Value* ll_from_address,
                                              ArrayType* arr_type,
                                              Struct* structn,
                                              Node* lvalue = nullptr);

        void gen_call_array_move_constructors(llvm::Value* ll_to_address,
                                              llvm::Value* ll_from_address,
                                              ArrayType* arr_type,
                                              Struct* structn,
                                              Node* lvalue = nullptr);
        void gen_call_move_constructor(llvm::Value* ll_to_address,
                                       llvm::Value* ll_from_address,
                                       Struct* structn,
                                       Node* lvalue = nullptr);

        void gen_abstract_double_array_loop(llvm::Value* ll_to_address,
                                            llvm::Value* ll_from_address,
                                            ArrayType* arr_type,
                                            Struct* structn,
                                            const std::function<void(llvm::Value*, llvm::Value*)>& gen_cb,
                                            Node* lvalue = nullptr);

        llvm::Value* gen_handle_returned_aggregate_obj(Type* aggr_type, llvm::Value* ll_ret, const char* tmp_obj_name);

        llvm::GlobalVariable* gen_foldable_global_variable(IdentRef* ref);

        void gen_nop();

    };
}

#endif // IR_GEN_H