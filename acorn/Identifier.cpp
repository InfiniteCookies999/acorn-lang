#include "Identifier.h"

#include "Context.h"

std::mutex                                acorn::Identifier::mtx;
llvm::StringMap<uint32_t>                 acorn::Identifier::mapped_identifiers;
llvm::DenseMap<uint32_t, llvm::StringRef> acorn::Identifier::name_mapping;
uint32_t                                  acorn::Identifier::id_counter = 1;

acorn::Identifier acorn::Identifier::get(llvm::StringRef identifier) {
    // Unfortunately we cannot use a shared_lock for reading
    // here because it is possible for two threads with the
    // same identifier to get to the unique lock and pass the
    // shared_lock check resulting in one two seperate writes
    // of the identifier to the map.
    //
    // TODO: It is still possible to use a use a shared lock for
    // reading if we double check when writing.
    //
    std::lock_guard lock(mtx);

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