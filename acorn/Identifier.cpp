#include "Identifier.h"

#include "Context.h"

std::mutex                                acorn::Identifier::mtx;
llvm::StringMap<uint32_t>                 acorn::Identifier::mapped_identifiers;
llvm::DenseMap<uint32_t, llvm::StringRef> acorn::Identifier::name_mapping;
uint32_t                                  acorn::Identifier::id_counter;

acorn::Identifier acorn::Identifier::get(llvm::StringRef identifier) {
    std::lock_guard lock(mtx);

    // Insert into the map if it does not exist.
    auto [itr, success] = mapped_identifiers.try_emplace(identifier, id_counter);
    if (success) {
        name_mapping.insert({ id_counter, identifier });
        ++id_counter;
    }

    return itr->second;
}

void acorn::Identifier::clear_cache() {
    mapped_identifiers.clear();
    name_mapping.clear();
    id_counter = 1;
}