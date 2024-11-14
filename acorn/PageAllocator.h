#ifndef PAGE_ALLOCATOR_H
#define PAGE_ALLOCATOR_H

#include <mutex>

#include <llvm/ADT/SmallVector.h>

namespace acorn {

    class PageAllocator {
    public:
        PageAllocator(size_t page_size, size_t alignment = sizeof(void*) * 2)
            : page_size(page_size), alignment(alignment) {
            assert((alignment & (alignment - 1)) == 0 && "Alignment must be a power of 2");

            alloc_new_page(page_size);
        }

        template<typename T>
        T* alloc_type() {
            return static_cast<T*>(allocate(sizeof(T)));
        }

        void* allocate(size_t size);

        void dealloc_all();

    private:

        size_t align_up(size_t size) const {
            return (size + (alignment - 1)) & ~(alignment - 1);
        }

        size_t next_multiple(size_t a, size_t n) const {
            size_t r = ((a + n - 1) / n) * n;
            assert(r >= a && "the next multiple is not larger enough");
            assert(r % n == 0 && "the next multiple is not a multiple of n");
            return r;
        }

        void alloc_new_page(size_t new_page_size);

        size_t page_size;
        size_t alignment;
        size_t offset;
        void*  cur_page;
        size_t cur_page_size;

        std::mutex mtx;
        
        llvm::SmallVector<void*, 32> pages;
        
    };
}

#endif // PAGE_ALLOCATOR_H