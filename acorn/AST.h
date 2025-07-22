#ifndef AST_H
#define AST_H

#include "Identifier.h"
#include "Token.h"
#include "RaisedError.h"
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
    class GenericType;
    struct ComptimeIfStmt;
    struct Struct;
    struct BinOp;
    struct Interface;
    struct Try;
    struct Generic;
    class PageAllocator;

    const size_t MAX_FUNC_PARAMS = 64;

    using FuncList = llvm::SmallVector<Func*>;

    enum class NodeKind {

        FUNC,
        IMPLICIT_FUNC,
        VAR,
        VAR_LIST,
        STRUCT,
        ENUM,
        INTERFACE,
        GENERIC,

        RETURN_STMT,
        IF_STMT,
        SCOPE_STMT,
        IMPORT_STMT,
        PREDICATE_LOOP_STMT,
        RANGE_LOOP_STMT,
        ITERATOR_LOOP_STMT,
        CONTINUE_STMT,
        BREAK_STMT,
        SWITCH_STMT,
        RAISE_STMT,
        RECOVER_STMT,
        UNINIT_NEW_CALL_STMT,

        EXPR_START,
        INVALID_EXPR,
        BIN_OP,
        UNARY_OP,
        NUMBER,
        BOOL_EXPR,
        IDENT_REF,
        DOT_OPERATOR,
        MEMORY_ACCESS,
        NAMED_VALUE,
        FUNC_CALL,
        STRUCT_INITIALIZER,
        STRING,
        NULL_EXPR,
        CAST,
        ARRAY,
        THIS_EXPR,
        SIZE_OF,
        TERNARY,
        MOVEOBJ,
        TYPE_EXPR,
        REFLECT,
        TRY,
        EXPR_END

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
            return kind > NodeKind::EXPR_START && kind < NodeKind::EXPR_END;
        }
    };

    // Statements
    //--------------------------------------

    struct Generic : Node {
        Generic() : Node(NodeKind::GENERIC) {
        }

        Identifier   name;
        size_t       index;
        GenericType* type;
    };

    struct GenericInstance {
    };

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

    struct GenericFuncInstance : GenericInstance {
        llvm::SmallVector<Type*> bound_types;
        // Parameters have their types fully qualified so that future
        // calls to the generic function know how to properly call the
        // function.
        //
        // Index 0 is the return type.
        llvm::SmallVector<Type*> qualified_decl_types;

        Struct* structn = nullptr;

        llvm::Function* ll_func = nullptr;
    };

    struct Func : Decl {
        Func() : Decl(NodeKind::FUNC) {
        }

        // If not null then the function is a member function.
        Struct* structn = nullptr;
        // This is equal to `structn` if the struct is not generic
        // otherwise this is the version of the struct prior to having
        // specific generic types specified.
        Struct* non_generic_struct_instance = nullptr;


        // If not null then the function is a function of an
        // interface. Note, this is different than a function that
        // implements an interface function, this function is the
        // declaration within the interface.
        Interface* interfacen = nullptr;

        // If this function belongs to an interface then this
        // is the n-th function of the interface.
        size_t interface_idx;

        // If this function is the implementation of an interface function
        // then the `mapped_interface_func` is the function of the interface
        // that is implemented.
        Func* mapped_interface_func = nullptr;

        // If this is a generic function then this is the currently bound
        // generic instance.
        GenericFuncInstance* generic_instance = nullptr;

        Type* parsed_return_type;
        Type* return_type;
        llvm::SmallVector<Var*, 8> params;
        llvm::SmallVector<RaisedError> raised_errors;

        // generic data
        //
        llvm::SmallVector<Generic*>             generics;
        llvm::SmallVector<GenericFuncInstance*> generic_instances;
        // Index 0 is the return type, rest are parameter types.
        llvm::SmallVector<Type*> partially_qualified_types;


        llvm::StringRef linkname;

        uint32_t num_returns = 0;
        llvm::SmallVector<Var*, 16> vars_to_alloc;

        size_t default_params_offset = static_cast<size_t>(-1);

        ScopeStmt* scope = nullptr;

        // when the function returns an aggregate type the function
        // may use a parameter that points to the return value instead
        // of returning a value directly.
        bool uses_aggr_param = false;

        Var* aggr_ret_var = nullptr;

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

        // when the function returns an aggregate type such
        // as an array then if the aggregate type can fit into
        // an integer this is the integer llvm type.
        llvm::Type* ll_aggr_int_ret_type = nullptr;

        // If the function is a native intrinsic function then this
        // id is set.
        llvm::Intrinsic::ID ll_intrinsic_id = 0;

        llvm::Function* ll_func = nullptr;

        Var* find_parameter(Identifier name) const;

        // Scans forward until it finds the 'const' keyword after the parameter
        // type information.
        SourceLoc get_function_const_location() const;

        // Scans forward until it finds the first parameter location with a default
        // parameter value.
        PointSourceLoc get_function_first_default_param_location() const;

        std::string get_decl_string() const;

        bool forwards_varargs(Expr* arg_value) const;

        bool is_generic() const { return !generics.empty(); }

        GenericFuncInstance* get_generic_instance(PageAllocator& allocator,
                                                  llvm::SmallVector<Type*> bound_types,
                                                  llvm::SmallVector<Type*> qualified_param_types,
                                                  Struct* parent_struct);

        void bind_generic_instance(GenericFuncInstance* generic_instance);

    };

    struct ImplicitFunc : Node {
        ImplicitFunc() : Node(NodeKind::IMPLICIT_FUNC) {
        }

        enum class ImplicitKind {
            DEFAULT_CONSTRUCTOR,
            COPY_CONSTRUCTOR,
            MOVE_CONSTRUCTOR,
            DESTRUCTOR,
            VTABLE_INIT
        } implicit_kind;

        Struct* structn;
    };

    struct Var : Decl {
        static const uint32_t NotParam = static_cast<uint32_t>(-1);
        static const uint32_t NotField = static_cast<uint32_t>(-1);

        Var() : Decl(NodeKind::VAR) {
        }

        llvm::StringRef linkname;

        Struct* structn = nullptr;
        // This is equal to `structn` if the struct is not generic
        // otherwise this is the version of the struct prior to having
        // specific generic types specified.
        Struct* non_generic_struct_instance = nullptr;

        uint32_t param_idx = NotParam;
        uint32_t field_idx = NotField;
        uint32_t ll_field_idx;
        bool     is_global = false;

        // This is the type that is parsed and does not change
        // during semantic analysis.
        Type* parsed_type;
        Type* type;

        Expr* assignment = nullptr;

        bool has_been_checked             = false;
        bool is_foldable                  = false;
        bool has_implicit_ptr             = false;
        bool should_default_initialize    = true;
        bool assignment_contains_generics = false;

        // If the variable is foldable then this contains the
        // generated value of the variable.
        llvm::Value* ll_comptime_value = nullptr;
        llvm::Value* ll_address;

        bool is_param() const { return param_idx != NotParam; }
        bool is_field() const { return field_idx != NotField; }

    };

    struct VarList : Node {
        VarList() : Node(NodeKind::VAR_LIST) {
        }

        llvm::SmallVector<Var*, 2> vars;
    };

    struct Struct : Decl {
        Struct() : Decl(NodeKind::STRUCT) {
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
            SourceLoc  error_loc;
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
        bool is_default_foldable     = true;
        bool is_generic              = false;
        // DUMMY variable for debugging.
        bool is_struct_instance_copy = false;

        Var* find_field(Identifier name) const;
        const InterfaceExtension* find_interface_extension(Identifier name) const;

    };

    struct GenericStructInstance : Struct, GenericInstance {
        GenericStructInstance() : Struct() {}

        llvm::SmallVector<Type*> bound_types;

        // Need these in order to properly set the generic instance information
        // when generating the llvm function declaration in implicit contexts.
        GenericFuncInstance* generic_default_constructor_instance;
        GenericFuncInstance* generic_destructor_instance;
        GenericFuncInstance* generic_move_constructor_instance;
        GenericFuncInstance* generic_copy_constructor_instance;
    };

    struct UnboundGenericStruct : Struct {
        UnboundGenericStruct() : Struct() {
        }

        llvm::SmallVector<Generic*>               generics;
        llvm::SmallVector<GenericStructInstance*> generic_instances;

        GenericStructInstance* get_generic_instance(PageAllocator& allocator,
                                                    llvm::SmallVector<Type*> bound_types);

    };

    struct Enum : Decl {
        Enum() : Decl(NodeKind::ENUM) {
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
        Interface() : Decl(NodeKind::INTERFACE) {
        }

        bool has_been_checked = false;
        InterfaceType* interface_type;

        llvm::SmallVector<Func*> functions;
    };

    struct ImportStmt : Node {
        ImportStmt() : Node(NodeKind::IMPORT_STMT) {
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
            NAMESPACE_KIND,
            COMPOSITE_KIND
        } imported_kind;

        union {
            Namespace* imported_nspace;
            Decl*      imported_composite;
        };

        bool is_imported_namespace() const { return imported_kind == NAMESPACE_KIND; }
        bool is_imported_composite() const { return imported_kind == COMPOSITE_KIND; }

        PointSourceLoc get_key_location(bool center_by_last) const;

        void set_imported_namespace(Namespace* nspace) {
            imported_kind = NAMESPACE_KIND;
            imported_nspace = nspace;
        }

        void set_imported_composite(Decl* composite) {
            imported_kind = COMPOSITE_KIND;
            imported_composite = composite;
        }
    };

    struct ReturnStmt : Node {
        ReturnStmt() : Node(NodeKind::RETURN_STMT) {
        }

        Expr* value = nullptr;
    };

    struct IfStmt : Node {
        IfStmt() : Node(NodeKind::IF_STMT) {
        }
        IfStmt(NodeKind kind) : Node(kind) {
        }

        Node*      cond;
        Expr*      post_variable_cond = nullptr;
        Node*      elseif;
        ScopeStmt* scope;
    };

    struct PredicateLoopStmt : Node {
        PredicateLoopStmt() : Node(NodeKind::PREDICATE_LOOP_STMT) {
        }

        Expr*      cond = nullptr;
        ScopeStmt* scope;
    };

    struct RangeLoopStmt : Node {
        RangeLoopStmt() : Node(NodeKind::RANGE_LOOP_STMT) {
        }

        Node*      init_node = nullptr;
        Expr*      cond = nullptr;
        ScopeStmt* scope;
        Node*      inc = nullptr;
    };

    struct IteratorLoopStmt : Node {
        IteratorLoopStmt() : Node(NodeKind::ITERATOR_LOOP_STMT) {
        }

        Node*                   vars;
        bool                    var_auto_ptr = false;
        Expr*                   container;
        ScopeStmt*              scope;
        // Instead of copying the value into the variable each loop it instead
        // stores a pointer to each element in the container.
        bool                    references_memory = false;
    };

    struct LoopControlStmt : Node {
        LoopControlStmt() : Node(NodeKind::INVALID_EXPR) {
        }
    };

    struct RecoverStmt : Node {
        RecoverStmt() : Node(NodeKind::RECOVER_STMT) {
        }

        Expr* value;
    };

    struct SwitchCase {
        Expr*      cond;
        ScopeStmt* scope;
    };

    struct SwitchStmt : Node {
        SwitchStmt() : Node(NodeKind::SWITCH_STMT) {
        }

        bool       all_conds_foldable = true;
        Expr*      on = nullptr;
        ScopeStmt* default_scope = nullptr;
        llvm::SmallVector<SwitchCase, 16> cases;
    };

    struct RaiseStmt : Node {
        RaiseStmt() : Node(NodeKind::RAISE_STMT) {
        }

        Expr*   expr;
        Struct* raised_error;
    };

    struct ScopeStmt : Node, llvm::SmallVector<Node*> {
        ScopeStmt() : Node(NodeKind::SCOPE_STMT) {
        }

        SourceLoc end_loc;
    };

    struct UninitNewCallStmt : Node {
        UninitNewCallStmt() : Node(NodeKind::UNINIT_NEW_CALL_STMT) {}

        Expr* address;
        Expr* value;
    };


    // Expressions
    //--------------------------------------

    struct Expr : Node {
        Expr(NodeKind kind) : Node(kind) {
        }

        bool  is_foldable = true;
        // The expression is a basic unit that may be interpreted as one of
        // several types depending on context. For example take:
        //
        // a: int64 = 5223;
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
        InvalidExpr() : Expr(NodeKind::INVALID_EXPR) {
        }
    };

    struct BinOp : Expr {
        BinOp() : Expr(NodeKind::BIN_OP) {
        }

        TokenKind op;

        Expr* lhs;
        Expr* rhs;
    };

    struct UnaryOp : Expr {
        UnaryOp() : Expr(NodeKind::UNARY_OP) {
        }

        TokenKind op;
        Expr*     expr;
    };

    struct Number : Expr {
        Number() : Expr(NodeKind::NUMBER) {
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
    };

    struct Bool : Expr {
        Bool() : Expr(NodeKind::BOOL_EXPR) {
        }

        bool value;
    };

    struct IdentRef : Expr {
        IdentRef() : Expr(NodeKind::IDENT_REF) {
        }

        IdentRef(NodeKind kind) : Expr(kind) {
        }

        Identifier ident;
        bool explicitly_binds_generics = false;

        enum class RelativeEnforcement {
            FILE,
            MODULE,
            NONE
        } relative_enforcement = RelativeEnforcement::NONE;

        bool found_ref() const {
            return found_kind != NONE_KIND;
        }

        // Discriminated union.
        enum {
            NONE_KIND,
            VAR_KIND,
            FUNCS_KIND,
            UNIVERAL_KIND,
            NAMESPACE_KIND,
            COMPOSITE_KIND,
            ENUM_VALUE_KIND,
            GENERIC_TYPE_KIND
        } found_kind = NONE_KIND;

        union {
            Var*         var_ref = nullptr;
            FuncList*    funcs_ref;
            Expr*        universal_ref;
            Namespace*   nspace_ref;
            Decl*        composite_ref;
            Enum::Value* enum_value_ref;
            GenericType* generic_type_ref;
        };

        bool is_var_ref() const          { return found_kind == VAR_KIND;          }
        bool is_funcs_ref() const        { return found_kind == FUNCS_KIND;        }
        bool is_universal_ref() const    { return found_kind == UNIVERAL_KIND;     }
        bool is_namespace_ref() const    { return found_kind == NAMESPACE_KIND;    }
        bool is_composite_ref() const    { return found_kind == COMPOSITE_KIND;    }
        bool is_enum_value_ref() const   { return found_kind == ENUM_VALUE_KIND;   }
        bool is_generic_type_ref() const { return found_kind == GENERIC_TYPE_KIND; }

        void set_var_ref(Var* var) {
            var_ref    = var;
            found_kind = VAR_KIND;
        }

        void set_funcs_ref(FuncList* funcs) {
            funcs_ref  = funcs;
            found_kind = FUNCS_KIND;
        }

        void set_universal_ref(Expr* universal) {
            universal_ref = universal;
            found_kind = UNIVERAL_KIND;
        }

        void set_namespace_ref(Namespace* nspace) {
            nspace_ref = nspace;
            found_kind = NAMESPACE_KIND;
        }

        void set_composite_ref(Decl* composite) {
            composite_ref = composite;
            found_kind = COMPOSITE_KIND;
        }

        void set_enum_value_ref(Enum::Value* value) {
            enum_value_ref = value;
            found_kind = ENUM_VALUE_KIND;
        }

        void set_generic_type_ref(GenericType* generic_type) {
            generic_type_ref = generic_type;
            found_kind = GENERIC_TYPE_KIND;
        }
    };

    struct DotOperator : IdentRef {
        DotOperator() : IdentRef(NodeKind::DOT_OPERATOR) {
        }

        bool is_array_length = false;
        bool is_slice_ptr    = false;
        bool is_enum_value   = false;
        Expr* site;

        PointSourceLoc expand_access_only() const;

    };

    struct GenericBindFuncCall : IdentRef {
        GenericBindFuncCall() : IdentRef(NodeKind::IDENT_REF) {
        }

        size_t non_named_args_offset = -1;
        llvm::SmallVector<Expr*> args;
        llvm::SmallVector<Type*> bound_types;
    };

    struct TypeExpr : Expr {
        TypeExpr() : Expr(NodeKind::TYPE_EXPR) {
        }

        TypeExpr(NodeKind kind) : Expr(kind) {
        }

        Type* parsed_expr_type;
        Type* expr_type;
    };

    struct MemoryAccess : TypeExpr {
        MemoryAccess() : TypeExpr(NodeKind::MEMORY_ACCESS) {
        }

        Expr* site;
        Expr* index;
    };

    struct NamedValue : Expr {
        NamedValue() : Expr(NodeKind::NAMED_VALUE) {
        }

        size_t     mapped_idx;
        Identifier name;
        Expr*      assignment;

        SourceLoc get_name_location() const;
    };

    struct FuncCall : Expr {
        FuncCall() : Expr(NodeKind::FUNC_CALL) {
        }
        FuncCall(NodeKind kind) : Expr(kind) {
        }

        Expr*                site;
        Func*                called_func;
        GenericFuncInstance* generic_instance = nullptr;
        Type*                type_for_type_expr = nullptr;

        size_t non_named_args_offset = -1;
        llvm::SmallVector<Expr*> args;
        llvm::SmallVector<Expr*> indeterminate_inferred_default_args;
        bool implicitly_converts_return = false;

    };

    struct StructInitializer : Expr {
        StructInitializer() : Expr(NodeKind::STRUCT_INITIALIZER) {
        }

        size_t non_named_vals_offset = 0;
        Struct* structn;
        Expr*  site;
        Func* called_constructor = nullptr;
        GenericFuncInstance* generic_called_instance = nullptr;
        llvm::SmallVector<Expr*> values;
        llvm::SmallVector<Expr*> indeterminate_inferred_default_values;
    };

    struct String : Expr {
        String() : Expr(NodeKind::STRING) {
        }

        std::string text;
    };

    struct Null : Expr {
        Null() : Expr(NodeKind::NULL_EXPR) {
        }
    };

    struct Cast : Expr {
        Cast() : Expr(NodeKind::CAST) {
        }

        Type* explicit_cast_type;
        Expr* value;
    };

    struct Array : Expr {
        Array() : Expr(NodeKind::ARRAY) {
        }

        llvm::SmallVector<Expr*, 8> elms;
    };

    struct This : Expr {
        This() : Expr(NodeKind::THIS_EXPR) {
        }
    };

    struct SizeOf : Expr {
        SizeOf() : Expr(NodeKind::SIZE_OF) {
        }

        //Type* parsed_type_with_size;
        Expr* value;
        Type* type_with_size;
    };

    struct Ternary : Expr {
        Ternary() : Expr(NodeKind::TERNARY) {
        }

        Expr* cond;
        Expr* lhs;
        Expr* rhs;
    };

    struct MoveObj : Expr {
        MoveObj() : Expr(NodeKind::MOVEOBJ) {
        }

        Expr* value;
    };

    struct Reflect : Expr {
        Reflect() : Expr(NodeKind::REFLECT) {
        }

        ReflectKind reflect_kind;
        Expr* expr;
        Type* type_info_type;
    };

    struct Try : Expr {
        Try() : Expr(NodeKind::TRY) {
        }

        // If true then the current function is specified as raising an error and
        // is calling a function that also is specified to raise the same error such
        // that it simply passes the current error into the other function on call.
        bool passes_error_along = false;
        bool generating_expr = false;

        llvm::SmallVector<Struct*> caught_errors;
        Var*                       caught_var = nullptr;
        ScopeStmt*                 catch_scope = nullptr;
        Expr*                      caught_expr;
        Node*                      catch_recoveree = nullptr;

        llvm::Value* ll_error;
        llvm::BasicBlock* ll_catch_bb;
        llvm::BasicBlock* ll_end_bb;

    };
}

#endif // AST_H
