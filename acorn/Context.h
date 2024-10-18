#ifndef CONTEXT_H
#define CONTEXT_H

#include <llvm/ADT/DenseMap.h>
#include <mutex>
#include <atomic>

#include "PageAllocator.h"
#include "Token.h"
#include "AST.h"
#include "Identifier.h"
#include "TypeTable.h"

namespace llvm {
    class LLVMContext;
    class Module;
}

namespace acorn {

    class Type;

    class Context {
    public:

        Context(llvm::LLVMContext& ll_context, llvm::Module& ll_module, PageAllocator& allocator);

        PageAllocator& get_allocator() {
            return allocator;
        }

        // static information.
        TypeTable type_table;
        Type* invalid_type;
        Type* void_type;
        Type* int_type;
        Type* const_int_type;
        Type* int8_type;
        Type* int16_type;
        Type* int32_type;
        Type* int64_type;
        Type* uint8_type;
        Type* uint16_type;
        Type* uint32_type;
        Type* uint64_type;
        Type* isize_type;
        Type* usize_type;
        Type* bool_type;
        Type* char_type;
        Type* char16_type;
        Type* char32_type;
        Type* const_char_ptr_type;
        Type* const_char16_ptr_type;
        Type* const_char32_ptr_type;
        Type* const_char_ptr_ptr_type;
        Type* funcs_ref_type;
        Type* module_ref_type;
        Type* null_type;
        Type* void_ptr_type;
        Type* const_void_type;
        Type* empty_array_type;

        // Used to identifier the entry point of
        // the program.
        Identifier main_identifier;
        Identifier length_identifier;
        Identifier access_identifier;
        Identifier namespace_identifier;

        llvm::LLVMContext& get_ll_context() const { return ll_context; }

        llvm::Module& get_ll_module() const { return ll_module; }

        Module* get_or_create_modl(llvm::StringRef mod_name);
        llvm::DenseMap<Identifier, Module*>& get_modules() { return modls; }
        Module* find_module(Identifier name);

        void queue_gen(Decl* decl);
        void add_unchecked_decl(Decl* decl) {
            unchcked_gen_queue.push_back(decl);
        }

        auto get_unchecked() const {
            return unchcked_gen_queue;
        }

        bool decl_queue_empty() const { return decls_gen_queue.empty(); }

        Decl* decl_queue_next() {
            Decl* decl = decls_gen_queue.back();
            decls_gen_queue.pop_back();
            return decl;
        }

        void add_canidate_main_function(Func* main_func);

        llvm::SmallVector<Func*>& get_canidate_main_funcs() {
            return canidate_main_funcs;
        }

        // Returns Token::Invalid if not a keyword.
        tokkind get_keyword_kind(llvm::StringRef word) const;

        // Given a token kind it returns the string representation of the keyword.
        llvm::StringRef get_keyword_from_kind(tokkind kind) const {
            return inv_keyword_mapping.find(kind)->second;
        }

        void set_main_function(Func* func) { main_func = func; }
        Func* get_main_function() const { return main_func; }

        // Returns -1 if invalid.
        int get_op_precedence(Token token) const {
            auto itr = precedence.find(token.kind);
            if (itr == precedence.end()) {
                return -1;
            }
            return itr->second;
        }

        bool has_errors() const {
            return error_count != 0;
        }

        size_t get_max_error_count() {
            return max_error_count;
        }

        void set_max_error_count(size_t max) {
            max_error_count = max;
        }

        bool should_show_error_codes() const {
            return show_error_codes;
        }

        void set_should_show_error_codes() {
            show_error_codes = true;
        }

        // Returns true if it exceeds the maximum allowed number
        // of errors.
        bool inc_error_count();

        Expr* get_universal_constant(Identifier identifier) const;

    private:
        PageAllocator& allocator;
        
        llvm::DenseMap<Identifier, acorn::Module*> modls;

        std::mutex main_function_mtx;
        llvm::SmallVector<Func*> canidate_main_funcs;
        Func* main_func = nullptr;

        std::atomic<size_t> error_count      = 0;
        size_t              max_error_count  = 30;
        bool                show_error_codes = false;

        llvm::LLVMContext& ll_context;
        llvm::Module&      ll_module;

        llvm::SmallVector<Decl*, 1024> decls_gen_queue;
        llvm::SmallVector<Decl*, 1024> unchcked_gen_queue;

        llvm::StringMap<tokkind>                 keyword_mapping;
        llvm::DenseMap<tokkind, llvm::StringRef> inv_keyword_mapping; // Inverse of keyword_mapping.
        llvm::DenseMap<tokkind, int>             precedence;
        llvm::DenseMap<Identifier, Expr*>        universal_constants;

    };
}

#endif // CONTEXT_H