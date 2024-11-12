#ifndef TYPE_H
#define TYPE_H

#include <string>
#include <cstdint>

namespace acorn {
    
    class PageAllocator;
    class TypeTable;
    struct Expr;
    class Context;

    enum class TypeKind {
        Invalid,

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
        Char32,

        Float32,
        Float64,

        Void,
        Bool,
        FuncsRef,   // A reference to an identifier to overloaded functions.
        ModuleRef,  // A reference to an identifier to a module.
        Pointer,
        Array,
        UnresolvedArrayType, // Length could not be resolved during parsing.
        Null,
        EmptyArrayType,
        AssignDeterminedArray,

        Range,
        
    };
    
    class Type {
    public:
        friend TypeTable;
        
        static Type* create(PageAllocator& allocator, TypeKind kind, bool is_const = false);

        TypeKind get_kind() const { return kind; }

        bool is_const() const { return vconst; }
        bool does_contain_const() const {
            return contains_const;
        }
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
            return kind >= TypeKind::Int && kind <= TypeKind::Char32;
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
        bool is_container() const { return is_pointer() || is_array(); }
        bool is_pointer() const   { return kind == TypeKind::Pointer;  }
        bool is_array() const     { return kind == TypeKind::Array;    }
        bool is_aggregate() const { return is_array();                 }
        bool is_bool() const      { return kind == TypeKind::Bool;     }
        bool is_range() const     { return kind == TypeKind::Range;    }

        // Any type that has its underlying memory represented as a pointer.
        bool is_real_pointer() const {
            return kind == TypeKind::Pointer || kind == TypeKind::Null;
        }
             
        uint32_t get_number_of_bits() const;

        std::string to_string() const;


    protected:
        Type(TypeKind kind, bool vconst)
            : kind(kind), vconst(vconst) {
        }

        // We store a version of this type except with all of it's
        // constness removed for efficiency and ease of use.
        Type* non_const_version;
        TypeKind kind;
        bool     vconst;
        // This is different than the type itself being const
        // this means that it is either itself const or has an
        // element type that is const.
        bool     contains_const;
    };

    class ContainerType : public Type {
    public:

        Type* get_elm_type() const { return elm_type; }

        Type* get_base_type() const;

    protected:
        ContainerType(TypeKind kind, bool vconst, Type* elm_type)
            : Type(kind, vconst), elm_type(elm_type) {
        }

        Type* elm_type;
    };

    class PointerType : public ContainerType {
    public:

        static Type* create(PageAllocator& allocator, Type* elm_type, bool is_const = false);

        std::string to_string() const;

    protected:
        PointerType(bool vconst, Type* elm_type) 
            : ContainerType(TypeKind::Pointer, vconst, elm_type) {
        }
    };

    class UnresolvedArrayType : public ContainerType {
    public:

        static Type* create(PageAllocator& allocator, Type* elm_type,
                            Expr* length_expr, bool is_const = false);

        Expr* get_length_expr() const { return length_expr; }

    private:
        UnresolvedArrayType(bool vconst, Expr* length_expr, Type* elm_type) :
            ContainerType(TypeKind::UnresolvedArrayType, vconst, elm_type) {
        }
        
        Expr* length_expr;
    };

    class ArrayType : public ContainerType {
    public:

        static Type* create(PageAllocator& allocator, Type* elm_type,
                            uint32_t length, bool is_const = false);

        uint32_t get_length() const { return length; }

        uint64_t get_total_linear_length() const;

        std::string to_string() const;

    protected:
        ArrayType(bool vconst, Type* elm_type, uint32_t length)
            : ContainerType(TypeKind::Array, vconst, elm_type), length(length) {
        }

        uint32_t length;
    };

    class AssignDeterminedArrayType : public ContainerType {
    public:

        static Type* create(PageAllocator& allocator, 
                            Type* elm_type, 
                            bool is_const = false);

        std::string to_string() const;

    protected:
        AssignDeterminedArrayType(bool vconst, Type* elm_type)
            : ContainerType(TypeKind::AssignDeterminedArray, vconst, elm_type) {
        }
    };

    class RangeType : public Type {
    public:

        static Type* create(PageAllocator& allocator, 
                            Type* value_type, 
                            bool is_const = false);

        Type* get_value_type() const { return value_type; }

        std::string to_string() const;

    protected:
        RangeType(bool vconst, Type* value_type)
            : Type(TypeKind::Range, vconst), value_type(value_type) {
        }

        Type* value_type;
    };
}

#endif // TYPE_H