#include "Identifier.h"

llvm::StringMap<uint32_t>                 acorn::Identifier::mapped_identifiers;
llvm::DenseMap<uint32_t, llvm::StringRef> acorn::Identifier::name_mapping;

acorn::Identifier acorn::Identifier::get(llvm::StringRef identifier) {
    
    static uint32_t id_counter = 1;

    // Insert into the map if it does not exist.
    auto [itr, success] = mapped_identifiers.try_emplace(identifier, id_counter);
    if (success) {
        name_mapping.insert({ id_counter, identifier });
        ++id_counter;
    }

    return itr->second;
}
