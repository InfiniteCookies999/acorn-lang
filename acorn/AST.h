#ifndef AST_H
#define AST_H

#include "Identifier.h"
#include "Token.h"
#include "SourceFile.h"

namespace llvm {
    class Function;
    class Value;
}

namespace acorn {

    class Module;
    class Type;
    struct Var;
    struct Expr;
    struct Func;
    struct ScopeStmt;

    const size_t MAX_FUNC_PARAMS = 64;

    using FuncList = llvm::SmallVector<Func*, 256>;

    enum class NodeKind {
        
        Func,
        Var,
        ReturnStmt,
        IfStmt,
        ComptimeIfStmt,
        ScopeStmt,
        ImportStmt,

        ExprStart,
        InvalidExpr,
        BinOp,
        UnaryOp,
        Number,
        Bool,
        IdentRef,
        DotOperator,
        MemoryAccess,
        NamedValue,
        FuncCall,
        String,
        Null,
        Cast,
        Array,
        ExprEnd

    };

    struct Modifier {
        const static uint32_t Start     = 0x01;
        const static uint32_t Native    = 0x01;
        const static uint32_t DllImport = 0x02;
        const static uint32_t End       = 0x02;

        static const char* to_string(uint32_t modifier);
    };

    struct Node {

        NodeKind  kind;
        SourceLoc loc;
        // This is needed because expressions can be folded
        // during parsing causing them to loose their original
        // point of origin. This is set to retain that information.
        PointSourceLoc expanded_loc;
        bool           uses_expanded_loc = false;

        Node(NodeKind kind)
            : kind(kind) {
        }

        // Checks if the node is of the given kind.
        [[nodiscard]] constexpr bool is(NodeKind kind) const noexcept { return this->kind == kind; }

        // Checks if the node is not of the given kind.
        [[nodiscard]] constexpr bool is_not(NodeKind kind) const noexcept { return !is(kind); }

        bool is_expression() const {
            return kind > NodeKind::ExprStart && kind < NodeKind::ExprEnd;
        }
    };

    // Statements
    //--------------------------------------

    struct Decl : Node {
        Decl(NodeKind kind) : Node(kind) {
        }

        bool generated = false;

        SourceFile* file;
        Identifier  name;
        uint32_t    modifiers;

        bool has_modifier(uint32_t modifier) {
            return (modifiers & modifier) != 0;
        }

        // Scans backwords until the modifier text is found. Required
        // since the locations of the modifiers are not stored for
        // performance sake. Luckily they are easily reobtainable by
        // simply iterating backwards.
        SourceLoc get_modifier_location(uint32_t modifier);

        Logger& get_logger() const { return file->logger; }

        Module& get_module() const { return file->modl; }

        void get_declared_msg(Logger& logger) const;

    };

    struct Func : Decl {
        Func() : Decl(NodeKind::Func) {
        }

        llvm::Function* ll_func = nullptr;

        llvm::StringRef linkname;

        Type* return_type;
        llvm::SmallVector<Var*, 8> params;

        uint32_t num_returns = 0;
        llvm::SmallVector<Var*, 16> vars_to_alloc;
        
        ScopeStmt* scope = nullptr;

        Var* find_parameter(Identifier name) const;

    };

    struct Var : Decl {
        static const uint32_t NotParam = static_cast<uint32_t>(-1);

        Var() : Decl(NodeKind::Var) {
        }

        llvm::StringRef linkname;
        
        uint32_t param_idx = NotParam;
        bool     is_global = false;

        Type* type;

        Expr* assignment = nullptr;

        llvm::Value* ll_address;

        bool is_param() const { return param_idx != NotParam; }

    };

    struct ImportStmt : Node {
        ImportStmt() : Node(NodeKind::ImportStmt) {
        }

        SourceFile* file;

        llvm::StringRef location_key; // Full path for resolving the import.
        Identifier      import_key;   // What gets referenced in the code by
                                      // the user when they want to use a
                                      // namespace.

        // Discriminated union.
        enum {
            ModuleKind
        } imported_kind;

        union {
            Module* imported_modl;
        };

        bool is_imported_module() const { return imported_kind == ModuleKind; }

        void set_imported_module(Module* modl) {
            imported_kind = ModuleKind;
            imported_modl = modl;
        }
    };

    struct ReturnStmt : Node {
        ReturnStmt() : Node(NodeKind::ReturnStmt) {
        }

        Expr* value = nullptr;
    };

    struct IfStmt : Node {
        IfStmt() : Node(NodeKind::IfStmt) {
        }
        IfStmt(NodeKind kind) : Node(kind) {
        }

        Node*      cond;
        Expr*      post_variable_cond = nullptr;
        Node*      elseif;
        ScopeStmt* scope;
    };

    struct ComptimeIfStmt : IfStmt {
        ComptimeIfStmt() : IfStmt(NodeKind::ComptimeIfStmt) {
        }

        SourceFile* file;
        bool takes_path;
    };

    struct ScopeStmt : Node, llvm::SmallVector<Node*> {
        ScopeStmt() : Node(NodeKind::ScopeStmt) {
        }
    };
    

    // Expressions
    //--------------------------------------

    struct Expr : Node {
        Expr(NodeKind kind) : Node(kind) {
        }

        bool  is_foldable = true;
        Type* type = nullptr;

        // When this is non-null it indicates that the expression
        // will first need to be casted before use of its computed
        // value.
        //
        // This is equivalent to inserting a cast node into the tree
        // without the extra overhead of creating new nodes.
        // 
        // Transformation:
        //            +                        +
        //          /   \           =>       /   \
        //         /     \                  /     \
        //    num(i16)  num(i32)       cast(i32)  num(i32)
        //                                |
        //                              num(i16)
        //
        Type* cast_type = nullptr;

    };

    struct InvalidExpr : Expr {
        InvalidExpr() : Expr(NodeKind::InvalidExpr) {
        }
    };

    struct BinOp : Expr {
        BinOp() : Expr(NodeKind::BinOp) {
        }

        tokkind op;

        Expr* lhs;
        Expr* rhs;
    };

    struct UnaryOp : Expr {
        UnaryOp() : Expr(NodeKind::UnaryOp) {
        }

        tokkind op;
        Expr*   expr;
    };

    struct Number : Expr {
        Number() : Expr(NodeKind::Number) {
        }

        union {
            uint64_t value_u64;
            int64_t  value_s64;
            uint32_t value_u32;
            int32_t  value_s32;
            uint16_t value_u16;
            int16_t  value_s16;
            uint8_t  value_u8;
            int8_t   value_s8;
        };
    };

    struct Bool : Expr {
        Bool() : Expr(NodeKind::Bool) {
        }

        bool value;
    };

    struct IdentRef : Expr {
        IdentRef() : Expr(NodeKind::IdentRef) {
        }

        IdentRef(NodeKind kind) : Expr(kind) {
        }

        Identifier ident;
    
        bool found_ref() const {
            return found_kind != NoneKind;
        }

        // Discriminated union.
        enum {
            NoneKind,
            VarKind,
            FuncsKind,
            UniversalKind,
            ImportKind,
        } found_kind = NoneKind;

        union {
            Var*        var_ref = nullptr;
            FuncList*   funcs_ref;
            Expr*       universal_ref;
            ImportStmt* import_ref;
        };

        bool is_var_ref() const       { return found_kind == VarKind;       }
        bool is_funcs_ref() const     { return found_kind == FuncsKind;     }
        bool is_universal_ref() const { return found_kind == UniversalKind; }
        bool is_import_ref() const    { return found_kind == ImportKind;    }

        void set_var_ref(Var* var) {
            var_ref    = var;
            found_kind = VarKind;
        }

        void set_funcs_ref(FuncList* funcs) {
            funcs_ref  = funcs;
            found_kind = FuncsKind;
        }

        void set_universal(Expr* universal) {
            universal_ref = universal;
            found_kind = UniversalKind;
        }

        void set_import(ImportStmt* importn) {
            import_ref = importn;
            found_kind = ImportKind;
        }
    };

    struct DotOperator : IdentRef {
        DotOperator() : IdentRef(NodeKind::DotOperator) {
        }

        Expr* site;
    };

    struct MemoryAccess : IdentRef {
        MemoryAccess() : IdentRef(NodeKind::MemoryAccess) {
        }

        Expr* site;
        Expr* index;
    };

    struct NamedValue : Expr {
        NamedValue() : Expr(NodeKind::NamedValue) {
        }

        size_t     mapped_idx;
        Identifier name;
        Expr*      assignment;
    };

    struct FuncCall : Expr {
        FuncCall() : Expr(NodeKind::FuncCall) {
        }

        Expr* site;
        Func* called_func;

        llvm::SmallVector<Expr*, 8> args;

    };

    struct String : Expr {
        String() : Expr(NodeKind::String) {
        }

        enum {
            Str8Bit,
            Str16Bit,
            Str32Bit
        } bit_type;

        std::string    text8bit;
        std::u16string text16bit;
        std::u32string text32bit;
    };

    struct Null : Expr {
        Null() : Expr(NodeKind::Null) {
        }
    };

    struct Cast : Expr {
        Cast() : Expr(NodeKind::Cast) {
        }

        Type* explicit_cast_type;
        Expr* value;
    };

    struct Array : Expr {
        Array() : Expr(NodeKind::Array) {
        }

        llvm::SmallVector<Expr*> elms;
    };
}

#endif // AST_H