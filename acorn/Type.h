#ifndef TYPE_H
#define TYPE_H

#include <string>
#include <cstdint>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Hashing.h>
#include <llvm/ADT/DenseMapInfo.h>

#include "Identifier.h"
#include "Source.h"
#include "RaisedError.h"

namespace llvm {
    class StructType;
}

namespace acorn {

    class Type;
    class PageAllocator;
    class TypeTable;
    struct Expr;
    class Context;
    struct Struct;
    struct Enum;
    struct Interface;
    class ContainerType;
    class PointerType;
    class ArrayType;
    class RangeType;
    class FunctionType;
    class StructType;
    class EnumType;
    class Generic;
    class GenericType;

    enum class TypeKind {

        Int,

        Int8,
        Int16,
        Int32,
        Int64,
        ISize,

        UInt8,
        UInt16,
        UInt32,
        UInt64,
        USize,

        Char,
        Char16,

        Float32,
        Float64,

        Void,
        Bool,
        FuncsRef,      // A reference to an identifier to overloaded functions.
        NamespaceRef,  // A reference to an identifier to a namespace.
        Pointer,
        Array,
        Slice,
        EmptyArray,
        Null,
        AssignDeterminedArray,
        Function,
        UnresolvedComposite,
        Struct,
        Enum,
        Interface,
        EnumContainer,
        Range,
        Auto,
        Expr, // A type that appears as part of an expression in code.
        Generic,
        UnresolvedGenericComposite,
        Invalid,

    };

    bool try_remove_const_for_compare(Type*& to_type, Type*& from_type, Expr* expr);
    bool has_valid_constness(Type* to_type, Type* from_type);

    class Type {
    public:
        friend TypeTable;

        static Type* create(PageAllocator& allocator, TypeKind kind, bool is_const = false);

        TypeKind get_kind() const { return kind; }

        bool is_const() const { return vconst; }
        bool does_contain_const() const { return contains_const; }
        bool does_contain_generics() const { return contains_generics; }
        Type* remove_all_const() const { return non_const_version; };

        bool is(const Type* type)     const { return type == this; }
        bool is_not(const Type* type) const { return type != this; }
        bool is_ignore_const(const Type* type) const {
            return remove_all_const()->is(type->remove_all_const());
        }

        bool is_number() const {
            return kind >= TypeKind::Int && kind <= TypeKind::Float64;
        }

        bool is_integer() const {
            return kind <= TypeKind::Char16;
        }

        bool is_signed() const {
            return (kind >= TypeKind::Int && kind <= TypeKind::ISize) ||
                    kind == TypeKind::Float32 || kind == TypeKind::Float64;
        }

        bool is_float() const {
            return kind == TypeKind::Float32 || kind == TypeKind::Float64;
        }

        bool is_unsigned() const {
            return !is_signed();
        }

        bool is_comparable() const;

        bool is_container() const {
            return is_pointer() || is_array() || is_slice();
        }

        bool is_aggregate() const {
            return is_array() || is_struct() || is_slice();
        }

        bool is_default_foldable() const;

        bool is_pointer() const   { return kind == TypeKind::Pointer;   }
        bool is_array() const     { return kind == TypeKind::Array;     }
        bool is_bool() const      { return kind == TypeKind::Bool;      }
        bool is_range() const     { return kind == TypeKind::Range;     }
        bool is_function() const  { return kind == TypeKind::Function;  }
        bool is_struct() const    { return kind == TypeKind::Struct;    }
        bool is_enum() const      { return kind == TypeKind::Enum;      }
        bool is_slice() const     { return kind == TypeKind::Slice;     }
        bool is_interface() const { return kind == TypeKind::Interface; }
        bool is_generic() const   { return kind == TypeKind::Generic;   }

        // Any type that has its underlying memory represented as a pointer.
        bool is_real_pointer() const {
            return kind == TypeKind::Pointer || kind == TypeKind::Null || kind == TypeKind::Function;
        }

        bool is_callable() const {
            return is_function();
        }

        bool is_sized() const;

        uint32_t get_number_of_bits() const;

        // True if the type needs to have it's destructor called.
        bool needs_destruction() const;

        std::string to_string() const;

        EnumType* get_container_enum_type() const {
            return container_enum_type;
        }

        void get_generic_types(llvm::SmallVector<const GenericType*>& generics) const;

    protected:
        Type(TypeKind kind, bool is_const)
            : kind(kind), vconst(is_const) {
        }

        // We store a version of this type except with all of it's
        // constness removed for efficiency and ease of use.
        Type* non_const_version;
        // If it is an enum type container then this is the enum type that
        // contains this type. Because the container types of the enums in
        // all instances except for checking assignability need to take into
        // account all the behavior of the type they contain instead of creating
        // a enum container type and wrapping the wrapping is done in the inverse.
        EnumType* container_enum_type = nullptr;
        TypeKind kind;
        bool     vconst;
        // This is different than the type itself being const
        // this means that it is either itself const or has an
        // element type that is const.
        bool     contains_const;
        bool     contains_generics = false;
    };

    class ContainerType : public Type {
    public:

        Type* get_elm_type() const { return elm_type; }

        Type* get_base_type() const;

        size_t get_depth() const;

    protected:
        ContainerType(TypeKind kind, bool is_const, Type* elm_type)
            : Type(kind, is_const), elm_type(elm_type) {
        }

        Type* elm_type;
    };

    class PointerType : public ContainerType {
    public:

        static PointerType* create(PageAllocator& allocator, Type* elm_type, bool is_const = false);

        std::string to_string() const;

    protected:
        PointerType(bool is_const, Type* elm_type)
            : ContainerType(TypeKind::Pointer, is_const, elm_type) {
        }
    };

    class ArrayType : public ContainerType {
    public:

        static ArrayType* create(PageAllocator& allocator,
                                 Type* elm_type,
                                 Expr* expr,
                                 bool is_const = false);

        static ArrayType* create(PageAllocator& allocator,
                                 Type* elm_type,
                                 uint32_t length,
                                 bool is_const = false);

        uint32_t get_length() const { return length; }

        uint64_t get_total_linear_length() const;

        std::string to_string() const;

        Expr* get_expr() const { return expr; }

        bool has_determined_length() const {
            return determined_length;
        }

    private:
        ArrayType(bool is_const, Expr* expr, Type* elm_type) :
            ContainerType(TypeKind::Array, is_const, elm_type), expr(expr) {
        }

        ArrayType(bool is_const, uint32_t length, Type* elm_type) :
            ContainerType(TypeKind::Array, is_const, elm_type), length(length) {
        }

        bool determined_length = false;
        uint32_t length;
        Expr* expr; // Set if not resolved during parsing.
    };

    class SliceType : public ContainerType {
    public:

        static SliceType* create(PageAllocator& allocator,
                                 Type* elm_type,
                                 bool is_const = false);

        std::string to_string() const;

        llvm::StructType* get_ll_struct_type() const {
            return ll_struct_type;
        }

        void set_ll_struct_type(llvm::StructType* ll_struct_type) {
            this->ll_struct_type = ll_struct_type;
        }

    protected:
        SliceType(bool is_const, Type* elm_type)
            : ContainerType(TypeKind::Slice, is_const, elm_type) {
        }

        // The struct type containing the pointer to memory and the length.
        llvm::StructType* ll_struct_type = nullptr;

    };

    // Ex. `int[] a = [ 1, 2 ,3 ]`
    class AssignDeterminedArrayType : public ContainerType {
    public:

        static Type* create(PageAllocator& allocator,
                            Type* elm_type,
                            bool is_const = false);

        std::string to_string() const;

    protected:
        AssignDeterminedArrayType(bool is_const, Type* elm_type)
            : ContainerType(TypeKind::AssignDeterminedArray, is_const, elm_type) {
        }
    };

    class RangeType : public Type {
    public:

        static RangeType* create(PageAllocator& allocator,
                                 Type* value_type,
                                 bool is_const = false);

        Type* get_value_type() const { return value_type; }

        std::string to_string() const;

    protected:
        RangeType(bool is_const, Type* value_type)
            : Type(TypeKind::Range, is_const), value_type(value_type) {
        }

        Type* value_type;
    };

    struct FunctionTypeKey {
        Type*                          return_type;
        llvm::SmallVector<Type*>       param_types;
        llvm::SmallVector<RaisedError> raised_errors;
        bool                           uses_native_varargs;

        FunctionTypeKey(Type* return_type,
                        llvm::SmallVector<Type*> param_types,
                        llvm::SmallVector<RaisedError> raised_errors,
                        bool uses_native_varargs)
            : return_type(return_type),
              param_types(std::move(param_types)),
              raised_errors(std::move(raised_errors)),
              uses_native_varargs(uses_native_varargs) {
        }
    };

    class FunctionType : public Type {
    public:

        static FunctionType* create(PageAllocator& allocator,
                                    FunctionTypeKey* key,
                                    bool is_const = false);

        Type* get_return_type() const {
            return key->return_type;
        }

        const llvm::SmallVector<Type*>& get_param_types() const {
            return key->param_types;
        }

        llvm::SmallVector<RaisedError>& get_raised_errors() const {
            return key->raised_errors;
        }

        bool uses_native_varargs() const {
            return key->uses_native_varargs;
        }

        FunctionTypeKey* get_key() const {
            return key;
        }

        std::string to_string() const;

    protected:
        FunctionType(bool is_const, FunctionTypeKey* key)
            : Type(TypeKind::Function, is_const),
              key(key) {
        }

        FunctionTypeKey* key;
    };

    class UnresolvedCompositeType : public Type {
    public:

        static Type* create(PageAllocator& allocator,
                            Identifier name,
                            SourceLoc  name_location,
                            llvm::SmallVector<Expr*> generic_exprs,
                            bool is_const = false);

        Identifier get_composite_name() const {
            return name;
        }

        SourceLoc get_error_location() const {
            return error_location;
        }

        llvm::SmallVector<Expr*>& get_generic_exprs() {
            return generic_exprs;
        }

    protected:
        UnresolvedCompositeType(bool is_const,
                                Identifier name,
                                SourceLoc error_location,
                                llvm::SmallVector<Expr*> generic_exprs)
            : Type(TypeKind::UnresolvedComposite, is_const),
              name(name),
              error_location(error_location),
              generic_exprs(std::move(generic_exprs)) {
        }

        SourceLoc                error_location;
        Identifier               name;
        llvm::SmallVector<Expr*> generic_exprs;
    };

    class StructType : public Type {
    public:

        static StructType* create(PageAllocator& allocator,
                                  Struct* nstruct,
                                  bool is_const = false);

        std::string to_string() const;

        Struct* get_struct() const {
            return structn;
        }

        void set_ll_struct_type(llvm::StructType* ll_type) {
            ll_struct_type = ll_type;
        }

        llvm::StructType* get_ll_struct_type() const {
            return ll_struct_type;
        }

    protected:
        StructType(bool is_const, Struct* structn)
            : Type(TypeKind::Struct, is_const), structn(structn) {
        }

        llvm::StructType* ll_struct_type = nullptr;
        Struct*           structn;
    };

    class EnumType : public Type {
    public:

        static EnumType* create(PageAllocator& allocator,
                                Enum* enumn,
                                bool is_const = false);

        std::string to_string() const;

        Enum* get_enum() const {
            return enumn;
        }

        void set_index_type(Type* type) {
            index_type = type;
        }

        Type* get_index_type() const {
            return index_type;
        }

        void set_default_index(uint64_t index) {
            default_index = index;
        }

        uint64_t get_default_index() const {
            return default_index;
        }

        void set_values_type(Type* type) {
            values_type = type;
        }

        Type* get_values_type() const {
            return values_type;
        }

    protected:
        EnumType(bool is_const, Enum* enumn)
            : Type(TypeKind::Enum, is_const), enumn(enumn) {
        }

        uint64_t default_index;
        Type* index_type;
        Type* values_type = nullptr;
        Enum* enumn;
    };

    class InterfaceType : public Type {
    public:

        static InterfaceType* create(PageAllocator& allocator,
                                     Interface* interfacen,
                                     bool is_const = false);

        std::string to_string() const;

        Interface* get_interface() const {
            return interfacen;
        }

    protected:
        InterfaceType(bool is_const, Interface* interfacen)
            : Type(TypeKind::Interface, is_const), interfacen(interfacen) {
        }

        Interface* interfacen;
    };

    class GenericType : public Type {
    public:

        static GenericType* create(PageAllocator& allocator, Generic* generic, bool is_const);

        std::string to_string() const;

        Generic* get_generic() const {
            return generic;
        }

        size_t get_generic_index() const;

    protected:
        GenericType(Generic* generic, bool is_const = false)
            : Type(TypeKind::Generic, is_const), generic(generic) {
        }

        Generic* generic;
    };
}

// Hashing for FunctionTypeKey.
namespace llvm {
template<>
struct DenseMapInfo<acorn::FunctionTypeKey*> {

    static acorn::FunctionTypeKey* getEmptyKey() {
        return reinterpret_cast<acorn::FunctionTypeKey*>(-1);
    }

    static acorn::FunctionTypeKey* getTombstoneKey() {
        return reinterpret_cast<acorn::FunctionTypeKey*>(-2);
    }

    static unsigned getHashValue(const acorn::FunctionTypeKey* key) {
        return static_cast<unsigned>(
            hash_combine(key->return_type,
                         hash_combine_range(key->param_types.begin(), key->param_types.end())));
    }

    static bool isEqual(const acorn::FunctionTypeKey* lhs,
                        const acorn::FunctionTypeKey* rhs) {
        if (lhs == getEmptyKey() || rhs == getEmptyKey()) {
            return lhs == rhs;
        }
        if (lhs == getTombstoneKey() || rhs == getTombstoneKey()) {
            return lhs == rhs;
        }

        if (lhs->return_type->is_not(rhs->return_type)) {
            return false;
        }
        if (lhs->param_types.size() != rhs->param_types.size()) {
            return false;
        }
        if (lhs->raised_errors.size() != rhs->raised_errors.size()) {
            return false;
        }

        for (size_t i = 0; i < lhs->param_types.size(); i++) {
            if (lhs->param_types[i]->is_not(rhs->param_types[i])) {
                return false;
            }
        }

        for (size_t i = 0; i < lhs->raised_errors.size(); i++) {
            if (lhs->raised_errors[i].structn != rhs->raised_errors[i].structn) {
                return false;
            }
        }

        if (lhs->uses_native_varargs != rhs->uses_native_varargs) {
            return false;
        }

        return true;
    }
};

}

#endif // TYPE_H