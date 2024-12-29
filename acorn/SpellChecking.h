#ifndef SPELL_CHECKING_H
#define SPELL_CHECKING_H

#include <string>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseMap.h>

#include "Identifier.h"

namespace acorn {
    
    class Logger;

    llvm::StringRef find_closest_spelling_match(const llvm::SmallVector<llvm::StringRef, 64>& comparisons,
                                                const llvm::StringRef& find_for);

    // TODO: should this limit the search count?
    class ErrorSpellChecker {
    public:

        ErrorSpellChecker(bool should_show)
            : should_show(should_show) {
        }

        template<typename T>
        void add_searches(const llvm::DenseMap<Identifier, T>& search_map) {
            for (auto& kv : search_map) {
                all_searches.push_back(kv.first.to_string());
            }
        }

        template<typename T>
        void add_searches(const llvm::SmallVector<T*>& search_list) {
            for (const T* decl : search_list) {
                all_searches.push_back(decl->name.to_string());
            }
        }

        void add_search(Identifier ident) {
            all_searches.push_back(ident.to_string());
        }

        bool search(Logger& logger, Identifier search_identifier);

    private:
        llvm::SmallVector<llvm::StringRef, 64> all_searches;
        bool should_show;
    };

}

#endif // SPELL_CHECKING_H