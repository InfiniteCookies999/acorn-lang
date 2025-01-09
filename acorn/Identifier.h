#ifndef IDENTIFIER_H
#define IDENTIFIER_H

#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/DenseMap.h>

#include <string_view>
#include <unordered_map>
#include <mutex>

namespace acorn {

    class Context;

    // A useful class for quickly determining if two identifiers in the language
    // are the same by comparing an integer rather than a string.
    //
    class Identifier {
    public:
        const static uint32_t Invalid = 0;

        friend bool operator==(Identifier identifier, size_t value);
        friend bool operator!=(Identifier identifier, size_t value);

        friend bool operator==(Identifier lhs, Identifier rhs);
        friend bool operator!=(Identifier lhs, Identifier rhs);

        Identifier() : id(Invalid) {
        }

        static Identifier get(llvm::StringRef identifier);

        uint32_t get_id() const { return id; }

        llvm::StringRef to_string() const {
            std::lock_guard lock(mtx);
            return name_mapping[id];
        }

        static void clear_cache();

    private:
        static std::mutex                                mtx;
        static llvm::StringMap<uint32_t>                 mapped_identifiers;
        static llvm::DenseMap<uint32_t, llvm::StringRef> name_mapping;
        static uint32_t                                  id_counter;

        Identifier(uint32_t id)
            : id(id) {
        }

        uint32_t id;

    };

    inline bool operator==(Identifier identifier, size_t value) {
        return identifier.id == value;
    }

    inline bool operator!=(Identifier identifier, size_t value) {
        return identifier.id != value;
    }

    inline bool operator==(Identifier lhs, Identifier rhs) {
        return lhs.id == rhs.id;
    }

    inline bool operator!=(Identifier lhs, Identifier rhs) {
        return lhs.id != rhs.id;
    }
}

namespace llvm {

    // Defining key information for the acorn::Identifier so it may be used as
    // a key in maps. Relies on the unique id of the acorn::Identifier.
    template<> struct DenseMapInfo<acorn::Identifier> {
        static bool isEqual(const acorn::Identifier& rhs, const acorn::Identifier& lhs) {
            return rhs == lhs;
        }
        static acorn::Identifier getTombstoneKey() {
            // Do not remove identifiers from maps.
            return acorn::Identifier();
        }
        static acorn::Identifier getEmptyKey() {
            return acorn::Identifier();
        }
        static size_t getHashValue(const acorn::Identifier& value) {
            return value.get_id();
        }
    };
}

#endif // IDENTIFIER_H