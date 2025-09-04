#ifndef DEBUG_GEN_H
#define DEBUG_GEN_H

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>

#include "AST.h"

namespace acorn {
    class TypeTable;

    class DebugInfoEmitter {
    public:

        DebugInfoEmitter(Context& context, SourceFile* file);

        void emit_function(Func* func);
        void emit_implicit_function(llvm::Function* ll_func,
                                    SourceFile* file,
                                    Struct* parent_struct,
                                    SourceLoc loc);
        void emit_function_end(Func* func);
        void emit_function_end(llvm::Function* ll_func);
        void emit_struct_this_variable(llvm::Value* ll_this, Func* func, llvm::IRBuilder<>& ir_builder);

        void push_location(llvm::IRBuilder<>& ir_builder, SourceLoc loc);
        void pop_location(llvm::IRBuilder<>& ir_builder);
        void set_location(llvm::Instruction* ll_instruction, SourceLoc loc);

        void emit_function_variable(Var* var, llvm::IRBuilder<>& ir_builder);
        void emit_global_variable(Var* global);

        void emit_scope_start(SourceLoc loc);
        void emit_scope_end();

        void emit_function_scope(llvm::Function* ll_func);

        void finalize();

        static void clear_cache();

    private:
        Context&             context;
        SourceFile*          file;
        llvm::DIBuilder      builder;
        llvm::DICompileUnit* di_unit = nullptr;

        llvm::SmallVector<llvm::DebugLoc, 4> di_location_stack;
        llvm::SmallVector<llvm::DIScope*, 4> di_lexical_scopes;

        static llvm::DenseMap<Type*, llvm::DIType*> di_cached_types;

        void emit_file(SourceFile* file);

        llvm::DIType* emit_type(Type* type);

        llvm::DILocation* create_location(SourceLoc loc);

    };
}

#endif // DEBUG_GEN_H
