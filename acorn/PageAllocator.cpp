#include "PageAllocator.h"

#include "Util.h"

#if WIN_OS
#include <Windows.h>
#undef min
#undef max
#elif UNIX_OS
#include <sys/mman.h>
#include <unistd.h>
#endif

#include <cstdlib>
#include <assert.h>
#include <new>

#include "Logger.h"

void* acorn::PageAllocator::allocate(size_t size) {

    // Just give more memory if it is not aligned.
    size_t aligned_size = align_up(size);

    std::lock_guard<std::mutex> lock(mtx);
    if (aligned_size + offset > cur_page_size) {
        alloc_new_page(next_multiple(aligned_size, page_size));
    }

    void* new_alloc = static_cast<char*>(cur_page) + offset;
    offset += aligned_size;

    return new_alloc;
}

void acorn::PageAllocator::dealloc_all() {
    for (void* page : pages) {
#if WIN_OS
        VirtualFree(page, 0, MEM_RELEASE);
#elif UNIX_OS
        munmap(page, page_size);
#endif
    }
}

void acorn::PageAllocator::alloc_new_page(size_t new_page_size) {

#if WIN_OS
    cur_page = VirtualAlloc(nullptr, new_page_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
#elif UNIX_OS
    void* addr = mmap(nullptr, new_page_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    cur_page = (addr == MAP_FAILED) ? nullptr : addr;
#endif

    if (!cur_page) {
        acorn_fatal("Failed to allocate memory. Likely ran out");
        throw std::bad_alloc();
    }

    cur_page_size = new_page_size;
    offset = 0;
    pages.push_back(cur_page);
}
