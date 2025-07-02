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
        void emit_function_end(Func* func);
        void emit_struct_this_variable(llvm::Value* ll_this, Func* func, llvm::IRBuilder<>& ir_builder);

        // Emit based on the last created instruction.
        void emit_location(llvm::IRBuilder<>& ir_builder, SourceLoc location);
        void emit_location(llvm::Instruction* ll_instruction, SourceLoc location);

        void set_store_node(Node* store_node) {
            this->store_node = store_node;
        }

        void clear_store_node() {
            this->store_node = nullptr;
        }

        Node* get_store_node() const {
            return store_node;
        }

        void emit_function_variable(Var* var, llvm::IRBuilder<>& ir_builder);
        void emit_global_variable(Var* global);

        void emit_scope_start(SourceLoc loc);
        void emit_scope_end();

        void finalize();

        static void clear_cache();

    private:
        Context&             context;
        SourceFile*          file;
        llvm::DIBuilder      builder;
        llvm::DICompileUnit* di_unit = nullptr;
        Node*                store_node = nullptr;

        llvm::SmallVector<llvm::DIScope*> di_lexical_scopes;

        static llvm::DenseMap<Type*, llvm::DIType*> di_cached_types;

        void emit_file(SourceFile* file);

        llvm::DIType* emit_type(Type* type);

    };
}

#endif // DEBUG_GEN_H