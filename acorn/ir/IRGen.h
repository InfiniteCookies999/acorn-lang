#ifndef IR_GEN_H
#define IR_GEN_H

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#include "../AST.h"
#include "GenTypes.h"

namespace acorn {
    
    class Context;
    
    class IRGenerator {
    public:

        IRGenerator(Context& context);

        void gen_function(Func* func);
        void gen_global_variable(Var* var);
        void finish_incomplete_global_variables();
        void finish_incomplete_global_variable(Var* var);

        llvm::Value* gen_node(Node* node);

        llvm::Value* gen_rvalue(Expr* node);

        llvm::Type* gen_type(Type* type) const {
            return acorn::gen_type(type, ll_context, ll_module);
        }

        llvm::Type* gen_ptrsize_int_type() {
            return acorn::gen_ptrsize_int_type(ll_context, ll_module);
        }

    private:
        Context&           context;
        llvm::LLVMContext& ll_context;
        llvm::Module&      ll_module;
        llvm::IRBuilder<>  builder;

        Func*           cur_func;
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

        llvm::SmallVector<llvm::BasicBlock*, 8> loop_break_stack;
        llvm::SmallVector<llvm::BasicBlock*, 8> loop_continue_stack;

        static int                         global_counter;
        static llvm::SmallVector<Var*, 32> incomplete_global_variables;
        static llvm::BasicBlock*           ll_global_init_call_bb;

        void gen_function_decl(Func* func);
        llvm::Type* gen_function_return_type(Func* func, bool is_main);
        void gen_function_body(Func* func);
        void gen_variable_address(Var* var, llvm::Type* ll_alloc_type);
        llvm::AllocaInst* gen_alloca(llvm::Type* ll_alloc_type, llvm::Twine ll_name);
        llvm::Type* gen_function_param_type(const Var* param) const;

        void gen_global_variable_decl(Var* var);
        void gen_global_variable_body(Var* var);
        
        llvm::Value* gen_return(ReturnStmt* ret);
        llvm::Value* gen_if(IfStmt* ifs);
        llvm::Value* gen_comptime_if(ComptimeIfStmt* ifs);
        llvm::Value* gen_predicate_loop(PredicateLoopStmt* loop);
        llvm::Value* gen_range_loop(RangeLoopStmt* loop);
        llvm::Value* gen_iterator_loop(IteratorLoopStmt* loop);
        void gen_cond_branch_for_loop(Expr* cond, llvm::BasicBlock* ll_body_bb, llvm::BasicBlock* ll_end_bb);
        llvm::Value* gen_loop_control(LoopControlStmt* loop_control);
        llvm::Value* gen_switch(SwitchStmt* switchn);
        llvm::Value* gen_switch_non_foldable(SwitchStmt* switchn);
        llvm::Value* gen_switch_foldable(SwitchStmt* switchn);
        llvm::Value* gen_scope(ScopeStmt* scope);

        llvm::Value* gen_variable(Var* var);
        llvm::Value* gen_number(Number* number);
        llvm::Value* gen_ident_reference(IdentRef* ref);
        llvm::Value* gen_binary_op(BinOp* bin_op);
        llvm::Value* gen_numeric_binary_op(tokkind op, BinOp* bin_op,
                                           llvm::Value* ll_lhs, llvm::Value* ll_rhs);
        llvm::Value* gen_equal(llvm::Value* ll_lhs, llvm::Value* ll_rhs);
        llvm::Value* gen_unary_op(UnaryOp* unary_op);
        llvm::Value* gen_function_call(FuncCall* call, llvm::Value* ll_dest_address);
        llvm::Value* gen_bool(Bool* b);
        llvm::Value* gen_string(String* string);
        llvm::Value* gen_null();
        llvm::Value* gen_cast(Cast* cast);
        llvm::Value* gen_array(Array* arr, llvm::Value* ll_dest_addr);
        llvm::Constant* gen_constant_array(Array* arr, ArrayType* arr_type, llvm::ArrayType* ll_arr_type);
        llvm::Value* gen_memory_access(MemoryAccess* mem_access);
        llvm::Value* gen_dot_operator(DotOperator* dot);

        void gen_assignment(llvm::Value* ll_address, Type* to_type, Expr* value);
        void gen_default_value(llvm::Value* ll_address, Type* type);
        llvm::Constant* gen_zero(Type* type);
        llvm::Constant* gen_one(Type* type);
        llvm::Value* gen_cast(Type* to_type, Type* from_type, llvm::Value* ll_value);

        llvm::Value* gen_condition(Expr* cond);

        llvm::BasicBlock* gen_bblock(const char* name, llvm::Function* ll_func = nullptr);

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
        //     return
        // }
        // // Code that may be not be reached because of the return jump.
        // int a = 5
        // ...
        // 
        void gen_branch_if_not_term(llvm::BasicBlock* ll_bb);
        llvm::Twine get_global_name(const char* name);
        llvm::GlobalVariable* gen_const_global_variable(const llvm::Twine& name,
                                                        llvm::Type* ll_type,
                                                        llvm::Constant* ll_initial_value,
                                                        llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage);
        llvm::GlobalVariable* gen_global_variable(const llvm::Twine& name,
                                                  llvm::Type* ll_type,
                                                  bool is_const,
                                                  llvm::Constant* ll_initial_value,
                                                  llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage);

        llvm::Align get_alignment(Type* type) const { return get_alignment(gen_type(type)); }
        llvm::Align get_alignment(llvm::Type* ll_type) const;
        uint64_t sizeof_type_in_bytes(llvm::Type* ll_type) const;

        llvm::Value* gen_array_memory_access(llvm::Value* ll_address, Type* arr_type, Expr* index);
        llvm::Value* gen_array_memory_access(llvm::Value* ll_address, llvm::Type* arr_type, llvm::Value* ll_index);

        llvm::Function* gen_void_function_decl(llvm::Twine ll_name);

        llvm::AllocaInst* gen_unseen_alloca(Type* type, llvm::Twine ll_name);
        llvm::AllocaInst* gen_unseen_alloca(llvm::Type* ll_type, llvm::Twine ll_name);

        bool is_decayed_array(Expr* arr);

    };
}

#endif // IR_GEN_H