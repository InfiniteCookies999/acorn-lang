#ifndef TYPE_H
#define TYPE_H

#include <string>
#include <cstdint>

namespace acorn {
    
    class PageAllocator;
    class TypeTable;

    enum class TypeKind {
        Invalid,

        Int,

        Int8,
        Int16,
        Int32,
        Int64,

        UInt8,
        UInt16,
        UInt32,
        UInt64,

        Char,
        Char16,
        Char32,

        Void,
        Bool,
        FuncsRef, // A reference to an identifier to overloaded functions.
        Pointer,
        Null,

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

        bool is_number() const {
            return is_integer();
        }

        bool is_integer() const {
            return kind >= TypeKind::Int && kind <= TypeKind::Char32;
        }

        bool is_signed() const {
            return kind >= TypeKind::Int && kind <= TypeKind::Int64;
        }

        bool is_unsigned() const {
            return !is_signed();
        }

        bool is_comparable() const;
        bool is_pointer() const { return kind == TypeKind::Pointer; }

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

    class PointerType : public Type {
    public:

        static Type* create(PageAllocator& allocator, Type* elm_type, bool is_const = false);

        Type* get_elm_type() const { return elm_type; }

        std::string to_string() const;

    protected:
        PointerType(bool vconst, Type* elm_type) 
            : Type(TypeKind::Pointer, vconst), elm_type(elm_type)
        {}

        Type* elm_type;
    };
}

#endif // TYPE_H