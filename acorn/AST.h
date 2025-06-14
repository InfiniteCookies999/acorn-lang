#ifndef AST_H
#define AST_H

#include "Identifier.h"
#include "Token.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseSet.h>

namespace llvm {
    class Function;
    class Value;
    class Type;
    class BasicBlock;

    namespace Intrinsic {
        typedef unsigned ID;
    }
}

namespace acorn {

    class Module;
    class Type;
    struct Var;
    struct Expr;
    struct Func;
    struct ScopeStmt;
    struct Number;
    class SourceFile;
    class Namespace;
    class Logger;
    class StructType;
    class EnumType;
    class InterfaceType;
    struct ComptimeIfStmt;
    struct Struct;
    struct BinOp;
    struct Interface;
    struct Try;

    const size_t MAX_FUNC_PARAMS = 64;

    using FuncList = llvm::SmallVector<Func*>;

    enum class NodeKind {

        Func,
        ImplicitFunc,
        Var,
        VarList,
        Struct,
        Enum,
        Interface,

        ReturnStmt,
        IfStmt,
        ScopeStmt,
        ImportStmt,
        PredicateLoopStmt,
        RangeLoopStmt,
        IteratorLoopStmt,
        ContinueStmt,
        BreakStmt,
        SwitchStmt,
        RaiseStmt,
        RecoverStmt,

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
        StructInitializer,
        String,
        Null,
        Cast,
        Array,
        This,
        SizeOf,
        Ternary,
        MoveObj,
        TypeExpr,
        Reflect,
        Try,
        ExprEnd

    };

    struct Modifier {
        const static uint32_t Start       = 0x01;
        const static uint32_t Native      = 0x01;
        const static uint32_t DllImport   = 0x02;
        const static uint32_t Public      = 0x04;
        const static uint32_t Private     = 0x08;
        const static uint32_t Readonly    = 0x10;
        const static uint32_t AccessMask  = 0x10;
        const static uint32_t AccessShift = 0x08;
        const static uint32_t End         = 0x10;

        static const char* to_string(uint32_t modifier);
    };

    enum class ReflectKind {
        TypeInfo
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
        bool is_being_checked = false;

        SourceFile* file;
        Identifier  name;
        uint32_t    modifiers;

        // When checking declaration it will assign to the
        // current declaration being checked the reference
        // of another declaration it depends on.
        //
        // This is used to help display information about circular
        // dependencies.
        Decl* dependency = nullptr;

        bool has_modifier(uint32_t modifier) const {
            return (modifiers & modifier) != 0;
        }

        // Scans backwords until the modifier text is found. Required
        // since the locations of the modifiers are not stored for
        // performance sake. Luckily they are easily reobtainable by
        // simply iterating backwards.
        SourceLoc get_modifier_location(uint32_t modifier) const;

        Logger& get_logger() const;

        Module& get_module() const;

        void show_prev_declared_msg(Logger& logger) const;
        void show_location_msg(Logger& logger) const;

        const char* get_composite_kind() const;

    };

    struct Func : Decl {
        Func() : Decl(NodeKind::Func) {
        }

        // If not null then the function is a member function.
        Struct* structn = nullptr;
        // If not null then the function is a function is an
        // interface. Note, this is different than a function that
        // implements an interface function, this function is the
        // declaration within the interface.
        Interface* interfacen = nullptr;

        llvm::Function* ll_func = nullptr;
        // If the function is a native intrinsic function then this
        // id is set.
        llvm::Intrinsic::ID ll_intrinsic_id = 0;

        llvm::StringRef linkname;

        Type* parsed_return_type;
        Type* return_type;
        llvm::SmallVector<Var*, 8> params;

        uint32_t num_returns = 0;
        llvm::SmallVector<Var*, 16> vars_to_alloc;

        struct RaisedError {
            Identifier name;
            SourceLoc  error_loc;
            Struct*    structn;
        };
        llvm::SmallVector<RaisedError> raised_errors;

        size_t default_params_offset = static_cast<size_t>(-1);

        // If this function belongs to an interface then this
        // is the n-th function of the interface.
        size_t interface_idx;

        ScopeStmt* scope = nullptr;

        // If this function is the implementation of an interface function
        // then the `mapped_interface_func` is the function of the interface
        // that is implemented.
        Func* mapped_interface_func = nullptr;

        // when the function returns an aggregate type such
        // as an array then if the aggregate type can fit into
        // an integer this is the integer llvm type.
        llvm::Type* ll_aggr_int_ret_type = nullptr;
        // when the function returns an aggregate type the function
        // may use a parameter that points to the return value instead
        // of returning a value directly.
        bool uses_aggr_param = false;

        bool cannot_use_aggr_ret_var = false;
        bool has_checked_declaration = false;
        bool is_checking_declaration = false;
        bool is_constructor          = false;
        bool is_destructor           = false;
        bool is_copy_constructor     = false;
        bool is_move_constructor     = false;
        bool has_implicit_return_ptr = false;
        bool uses_native_varargs     = false;
        bool uses_varargs            = false;
        bool is_constant             = false;
        bool has_errors              = false;
        bool is_dynamic              = false;
        Var* aggr_ret_var = nullptr;

        Var* find_parameter(Identifier name) const;

        // Scans forward until it finds the 'const' keyword after the parameter
        // type information.
        SourceLoc get_function_const_location() const;

        std::string get_decl_string() const;

        bool forwards_varargs(Expr* arg_value) const;

    };

    struct ImplicitFunc : Node {
        ImplicitFunc() : Node(NodeKind::ImplicitFunc) {
        }

        enum class ImplicitKind {
            DefaultConstructor,
            CopyConstructor,
            MoveConstructor,
            Destructor,
            VTableInit
        } implicit_kind;

        Struct* structn;
    };

    struct Var : Decl {
        static const uint32_t NotParam = static_cast<uint32_t>(-1);
        static const uint32_t NotField = static_cast<uint32_t>(-1);

        Var() : Decl(NodeKind::Var) {
        }

        llvm::StringRef linkname;

        Struct* structn;

        uint32_t param_idx = NotParam;
        uint32_t field_idx = NotField;
        bool     is_global = false;

        // This is the type that is parsed and does not change
        // during semantic analysis.
        Type* parsed_type;
        Type* type;

        Expr* assignment = nullptr;

        bool has_been_checked          = false;
        bool is_foldable               = false;
        bool has_implicit_ptr          = false;
        bool should_default_initialize = true;

        // If the variable is foldable then this contains the
        // generated value of the variable.
        llvm::Value* ll_comptime_value = nullptr;

        // If this is set to true then the function passes the
        // struct value as an integer type then converts it back
        // once inside the body of the function.
        bool is_aggr_int_param = true;
        // If this is set to true then struct type parameters
        // are passed as pointers and memcpy is used at the
        // calling location.
        bool is_aggr_param = false;

        llvm::Value* ll_address;

        bool is_param() const { return param_idx != NotParam; }
        bool is_field() const { return field_idx != NotField; }

    };

    struct VarList : Node {
        VarList() : Node(NodeKind::VarList) {
        }

        llvm::SmallVector<Var*> list;
    };

    struct Struct : Decl {
        Struct() : Decl(NodeKind::Struct) {
        }

        StructType* struct_type;

        Namespace* nspace;
        // Ordered list of the fields.
        llvm::SmallVector<Var*> fields;
        struct DuplicateStructFuncInfo {
            Func* duplicate_function;
            Func* prior_function;
        };
        // These functions are specifically for functions such as copy constructors
        // which are not placed in the normal functions list.
        llvm::SmallVector<DuplicateStructFuncInfo> duplicate_struct_func_infos;

        struct UnresolvedExtension {
            Identifier name;
            bool       is_dynamic;
        };

        struct InterfaceExtension {
            Interface* interfacen;
            bool       is_dynamic;
        };

        llvm::SmallVector<UnresolvedExtension> unresolved_extensions;
        llvm::SmallVector<InterfaceExtension>  interface_extensions;

        Func*                    default_constructor = nullptr;
        Func*                    copy_constructor    = nullptr;
        Func*                    move_constructor    = nullptr;
        Func*                    destructor          = nullptr;
        llvm::SmallVector<Func*> constructors;

        llvm::Function* ll_default_constructor = nullptr;
        llvm::Function* ll_destructor          = nullptr;
        llvm::Function* ll_copy_constructor    = nullptr;
        llvm::Function* ll_move_constructor    = nullptr;
        llvm::Function* ll_init_vtable_func    = nullptr;

        bool has_been_checked        = false;
        bool has_errors              = false;
        bool fields_have_assignments = false;
        bool needs_default_call      = false;
        bool needs_destruction       = false;
        bool needs_copy_call         = false;
        bool needs_move_call         = false;
        bool fields_need_destruction = false;
        bool fields_need_copy_call   = false;
        bool fields_need_move_call   = false;
        bool uses_vtable             = false;
        bool aborts_error            = false;

        Var* find_field(Identifier name) const;
        const InterfaceExtension* find_interface_extension(Identifier name) const;

        SourceLoc get_extension_location(Identifier name) const;

    };

    struct Enum : Decl {
        Enum() : Decl(NodeKind::Enum) {
        }

        EnumType* enum_type;

        struct Value {
            uint64_t   index;
            Identifier name;
            SourceLoc  name_loc;
            Expr*      assignment;
        };

        llvm::SmallVector<Value> values;

        // When the values are not simply integers then the values
        // are placed into a global array.
        llvm::Value* ll_array = nullptr;
        bool has_been_checked = false;

    };

    struct Interface : Decl {
        Interface() : Decl(NodeKind::Interface) {
        }

        bool has_been_checked = false;
        InterfaceType* interface_type;

        llvm::SmallVector<Func*> functions;
    };

    struct ImportStmt : Node {
        ImportStmt() : Node(NodeKind::ImportStmt) {
        }

        SourceFile* file;

        bool is_static          = false;
        bool within_same_modl   = false;
        bool within_parent_modl = false;

        struct KeyPart {
            Identifier name;
            SourceLoc  error_loc;
        };

        // Last element is the thing imported. All other elements
        // are the path to that imported value.
        llvm::SmallVector<KeyPart, 4> key;

        // Discriminated union.
        enum {
            NamespaceKind,
            CompositeKind
        } imported_kind;

        union {
            Namespace* imported_nspace;
            Decl*      imported_composite;
        };

        bool is_imported_namespace() const { return imported_kind == NamespaceKind; }
        bool is_imported_composite() const { return imported_kind == CompositeKind; }

        void set_imported_namespace(Namespace* nspace) {
            imported_kind = NamespaceKind;
            imported_nspace = nspace;
        }

        void set_imported_composite(Decl* composite) {
            imported_kind = CompositeKind;
            imported_composite = composite;
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

    struct PredicateLoopStmt : Node {
        PredicateLoopStmt() : Node(NodeKind::PredicateLoopStmt) {
        }

        Expr*      cond = nullptr;
        ScopeStmt* scope;
    };

    struct RangeLoopStmt : Node {
        RangeLoopStmt() : Node(NodeKind::RangeLoopStmt) {
        }

        Node*      init_node = nullptr;
        Expr*      cond = nullptr;
        ScopeStmt* scope;
        Node*      inc = nullptr;
    };

    struct IteratorLoopStmt : Node {
        IteratorLoopStmt() : Node(NodeKind::IteratorLoopStmt) {
        }

        Var*       var;
        Expr*      container;
        ScopeStmt* scope;
        bool       references_memory = false;
    };

    struct LoopControlStmt : Node {
        LoopControlStmt() : Node(NodeKind::InvalidExpr) {
        }
    };

    struct RecoverStmt : Node {
        RecoverStmt() : Node(NodeKind::RecoverStmt) {
        }

        Expr* value;
    };

    struct SwitchCase {
        Expr*      cond;
        ScopeStmt* scope;
    };

    struct SwitchStmt : Node {
        SwitchStmt() : Node(NodeKind::SwitchStmt) {
        }

        bool       all_conds_foldable = true;
        Expr*      on = nullptr;
        ScopeStmt* default_scope = nullptr;
        llvm::SmallVector<SwitchCase, 16> cases;
    };

    struct RaiseStmt : Node {
        RaiseStmt() : Node(NodeKind::RaiseStmt) {
        }

        Expr*   expr;
        Struct* raised_error;
    };

    struct ScopeStmt : Node, llvm::SmallVector<Node*> {
        ScopeStmt() : Node(NodeKind::ScopeStmt) {
        }

        SourceLoc end_loc;
    };


    // Expressions
    //--------------------------------------

    void iterate_over_range_values(BinOp* range, const std::function<void(uint64_t)>& cb);

    uint64_t get_total_range_values(BinOp* range);

    struct Expr : Node {
        Expr(NodeKind kind) : Node(kind) {
        }

        bool  is_foldable = true;
        // The expression is a basic unit that may be interpreted as one of
        // several types depending on context. For example take:
        //
        // int64 a = 5223;
        //
        // The number `5223` is trivially reassignable to numeric types as long
        // as the number can fit into the respective integer size.
        bool  trivially_reassignable = false;
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

        // The expression is wrapped inside a try expression.
        Try* tryn = nullptr;

        Type* get_final_type() const {
            return cast_type ? cast_type : type;
        }
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
            float    value_f32;
            double   value_f64;
        };

        bool uses_strict_type = false;
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

        enum class RelativeEnforcement {
            File,
            Module,
            None
        } relative_enforcement = RelativeEnforcement::None;

        bool found_ref() const {
            return found_kind != NoneKind;
        }

        // Discriminated union.
        enum {
            NoneKind,
            VarKind,
            FuncsKind,
            UniversalKind,
            NamespaceKind,
            CompositeKind,
            EnumValueKind
        } found_kind = NoneKind;

        union {
            Var*         var_ref = nullptr;
            FuncList*    funcs_ref;
            Expr*        universal_ref;
            Namespace*   nspace_ref;
            Decl*        composite_ref;
            Enum::Value* enum_value_ref;
        };

        bool is_var_ref() const        { return found_kind == VarKind;       }
        bool is_funcs_ref() const      { return found_kind == FuncsKind;     }
        bool is_universal_ref() const  { return found_kind == UniversalKind; }
        bool is_namespace_ref() const  { return found_kind == NamespaceKind; }
        bool is_composite_ref() const  { return found_kind == CompositeKind; }
        bool is_enum_value_ref() const { return found_kind == EnumValueKind; }

        void set_var_ref(Var* var) {
            var_ref    = var;
            found_kind = VarKind;
        }

        void set_funcs_ref(FuncList* funcs) {
            funcs_ref  = funcs;
            found_kind = FuncsKind;
        }

        void set_universal_ref(Expr* universal) {
            universal_ref = universal;
            found_kind = UniversalKind;
        }

        void set_namespace_ref(Namespace* nspace) {
            nspace_ref = nspace;
            found_kind = NamespaceKind;
        }

        void set_composite_ref(Decl* composite) {
            composite_ref = composite;
            found_kind = CompositeKind;
        }

        void set_enum_value_ref(Enum::Value* value) {
            enum_value_ref = value;
            found_kind = EnumValueKind;
        }
    };

    struct DotOperator : IdentRef {
        DotOperator() : IdentRef(NodeKind::DotOperator) {
        }

        bool is_array_length = false;
        bool is_slice_ptr    = false;
        bool is_enum_value   = false;
        Expr* site;
    };

    struct TypeExpr : Expr {
        TypeExpr() : Expr(NodeKind::TypeExpr) {
        }

        TypeExpr(NodeKind kind) : Expr(kind) {
        }

        NodeKind prev_node_kind = NodeKind::InvalidExpr;
        Type* parsed_expr_type;
        Type* expr_type;
    };

    struct MemoryAccess : TypeExpr {
        MemoryAccess() : TypeExpr(NodeKind::MemoryAccess) {
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

        size_t non_named_args_offset = -1;
        llvm::SmallVector<Expr*> args;
        bool implicitly_converts_return = false;

    };

    struct StructInitializer : Expr {
        StructInitializer() : Expr(NodeKind::StructInitializer) {
        }

        size_t non_named_vals_offset = 0;
        Struct* structn;
        IdentRef* ref;
        Func* called_constructor = nullptr;
        llvm::SmallVector<Expr*> values;
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

        llvm::SmallVector<Expr*, 8> elms;
    };

    struct This : Expr {
        This() : Expr(NodeKind::This) {
        }
    };

    struct SizeOf : Expr {
        SizeOf() : Expr(NodeKind::SizeOf) {
        }

        Type* parsed_type_with_size;
        Type* type_with_size;
    };

    struct Ternary : Expr {
        Ternary() : Expr(NodeKind::Ternary) {
        }

        Expr* cond;
        Expr* lhs;
        Expr* rhs;
    };

    struct MoveObj : Expr {
        MoveObj() : Expr(NodeKind::MoveObj) {
        }

        Expr* value;
    };

    struct Reflect : Expr {
        Reflect() : Expr(NodeKind::Reflect) {
        }

        ReflectKind reflect_kind;
        Expr* expr;
        Type* type_info_type;
    };

    struct Try : Expr {
        Try() : Expr(NodeKind::Try) {
        }

        // If true then the current function is specified as raising an error and
        // is calling a function that also is specified to raise the same error such
        // that it simply passes the current error into the other function on call.
        bool passes_error_along = false;
        bool generating_expr = false;

        llvm::DenseSet<Struct*> caught_errors;
        Var*                    caught_var = nullptr;
        ScopeStmt*              catch_block = nullptr;
        Expr*                   caught_expr;
        Node*                   catch_recoveree = nullptr;

        llvm::Value* ll_error;
        llvm::BasicBlock* ll_catch_bb;
        llvm::BasicBlock* ll_end_bb;

    };
}

#endif // AST_H