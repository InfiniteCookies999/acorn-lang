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
    class BasicBlock;
    class GlobalVariable;

    namespace Intrinsic {
        typedef unsigned ID;
    }
}

namespace acorn {

    class Type;
    class DebugInfoEmitter;

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
        Type* float32_type;
        Type* float64_type;
        Type* bool_type;
        Type* char_type;
        Type* char16_type;
        Type* const_char_ptr_type;
        Type* char16_ptr_type;
        Type* const_char16_ptr_type;
        Type* const_char_ptr_ptr_type;
        Type* funcs_ref_type;
        Type* namespace_ref_type;
        Type* null_type;
        Type* void_ptr_type;
        Type* const_void_type;
        Type* const_void_ptr_type;
        Type* empty_array_type;
        Type* auto_type;
        Type* auto_ptr_type;
        Type* const_auto_type;
        Type* expr_type;

        // Used to identifier the entry point of
        // the program.
        Identifier main_identifier;
        Identifier length_identifier;
        Identifier ptr_identifier;
        Identifier access_identifier;
        Identifier namespace_identifier;
        Identifier module_identifier;
        Identifier string_struct_identifier;
        Identifier reflect_identifier;
        Identifier type_id_enum_identifier;
        Identifier type_struct_identifier;
        Identifier struct_type_info_struct_identifier;
        Identifier field_type_info_struct_identifier;
        Identifier enum_type_info_struct_identifier;
        Identifier any_struct_identifier;
        Identifier value_identifier;
        Identifier error_interface_identifier;
        Identifier get_name_function_identifier;

        ImportStmt* std_string_struct_import;
        Enum*       std_type_id_enum;
        Struct*     std_type_struct;
        Struct*     std_struct_type_info_struct;
        Struct*     std_field_type_info_struct;
        Struct*     std_enum_type_info_struct;
        Struct*     std_any_struct;
        StructType* std_any_struct_type;
        Type*       const_std_type_ptr;
        Interface*  std_error_interface;
        Func*       std_abort_function;
        Func*       std_error_get_name_func;

        llvm::DenseMap<Type*, llvm::GlobalVariable*> ll_type_info_global_addresses;

        // LLVM generation data.
        struct LLVMIntrinsicDefinition {
            Identifier               name;
            llvm::SmallVector<Type*> param_types;
            Type*                    return_type;
        };

        llvm::DenseMap<Identifier, llvm::Intrinsic::ID> ll_intrinsics_table;
        llvm::SmallVector< LLVMIntrinsicDefinition>     ll_valid_intrinsic_defs;

        int                         global_counter            = 0;
        llvm::Function*             ll_global_init_function   = nullptr;
        llvm::SmallVector<Var*, 32> globals_needing_destroyed;
        llvm::BasicBlock*           ll_global_init_call_bb    = nullptr;
        llvm::BasicBlock*           ll_global_cleanup_call_bb = nullptr;
        // A map between expressions that are foldable and global variables
        // generated for them if the user ever references the memory of the
        // variables. Normally a variable would not be needed and the value
        // would simply be folded but if the user tries to take an address
        // of such a variable then the variable must exist in memory.
        llvm::DenseMap<Var*, llvm::GlobalVariable*> ll_foldable_globals;

        llvm::LLVMContext& get_ll_context() const { return ll_context; }
        llvm::Module& get_ll_module()       const { return ll_module;  }

        Module* get_or_create_modl(llvm::StringRef mod_name);
        llvm::DenseMap<Identifier, Module*>& get_modules() { return modls; }
        Module* find_module(Identifier name);

        void queue_gen(Decl* decl);
        void queue_gen_implicit_function(ImplicitFunc* implicit_func);
        void add_unchecked_decl(Decl* decl) {
            unchcked_gen_queue.push_back(decl);
        }

        auto get_unchecked() const {
            return unchcked_gen_queue;
        }

        bool decl_queue_empty() const { return decls_gen_queue.empty(); }

        Node* decl_queue_next() {
            Node* decl = decls_gen_queue.back();
            decls_gen_queue.pop_back();
            return decl;
        }

        void add_canidate_main_function(Func* main_func);

        inline bool is_std_any_type(Type* type) const {
            return !is_stand_alone && type->remove_all_const()->is(std_any_struct_type);
        }

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

        size_t get_max_error_count() const {
            return max_error_count;
        }

        void set_max_error_count(size_t max) {
            max_error_count = max;
        }

        void set_max_call_err_funcs(size_t max) {
            max_call_err_funcs = max;
        }

        size_t get_max_call_err_funcs() {
            return max_call_err_funcs;
        }

        bool should_show_error_codes() const {
            return show_error_codes;
        }

        void set_should_show_error_codes() {
            show_error_codes = true;
        }

        bool should_emit_debug_info() const {
            return emit_debug_info;
        }

        void set_should_emit_debug_info() {
            emit_debug_info = true;
        }

        void set_dont_show_error_location() {
            show_error_locations = false;
        }

        bool should_show_error_location() const {
            return show_error_locations;
        }

        void set_dont_show_spell_checking() {
            show_spell_checking = false;
        }

        bool should_show_spell_checking() const {
            return show_spell_checking;
        }

        void set_stand_alone() {
            is_stand_alone = true;
        }

        bool should_stand_alone() const {
            return is_stand_alone;
        }

        // Returns true if it exceeds the maximum allowed number
        // of errors.
        bool inc_error_count();

        Expr* get_universal_constant(Identifier identifier) const;

        const llvm::DenseMap<Identifier, Expr*>& get_universal_constants() const {
            return universal_constants;
        }

        ReflectKind get_reflect_kind(llvm::StringRef identifier) const {
            auto itr = reflect_identifiers.find(identifier);
            return itr->second;
        }

    private:
        PageAllocator& allocator;

        llvm::DenseMap<Identifier, acorn::Module*> modls;

        std::mutex main_function_mtx;
        llvm::SmallVector<Func*> canidate_main_funcs;
        Func* main_func = nullptr;

        std::atomic<size_t> error_count          = 0;
        size_t              max_error_count      = 30;
        size_t              max_call_err_funcs   = 3;
        bool                show_error_codes     = false;
        bool                emit_debug_info      = false;
        bool                show_error_locations = true;
        bool                show_spell_checking  = true;
        bool                is_stand_alone       = false;

        llvm::LLVMContext& ll_context;
        llvm::Module&      ll_module;

        llvm::SmallVector<Node*, 1024> decls_gen_queue;
        llvm::SmallVector<Decl*, 1024> unchcked_gen_queue;

        llvm::StringMap<tokkind>                 keyword_mapping;
        llvm::DenseMap<tokkind, llvm::StringRef> inv_keyword_mapping; // Inverse of keyword_mapping.
        llvm::DenseMap<tokkind, int>             precedence;
        llvm::DenseMap<Identifier, Expr*>        universal_constants;
        llvm::StringMap<ReflectKind>             reflect_identifiers;

    };
}

#endif // CONTEXT_H