#ifndef SOURCE_H
#define SOURCE_H

#include <cstdint>
#include <string_view>

namespace acorn {

    const char NUMBER_SEPERATOR = '_';

    class Context;

    struct Buffer {
        char*  content;
        size_t length;
    };

    struct SourceLoc {
        const char* ptr;
        // When displaying error locations the reporting will try to use this
        // central point to render the error around as it behaves as the central
        // location where the error happened.
        //
        // For example, given the expression:   `a + b`
        // If `a` and `b` cannot be added then the entire expression is underlined,
        // but the central location is at the `+` operator since the error happened
        // on the `+` operator.
        //
        const char* central_pt;
        uint32_t    length;

        static SourceLoc from_ptrs(const char* beg_ptr, const char* end_ptr) {
            return SourceLoc{
                .ptr        = beg_ptr,
                .central_pt = beg_ptr,
                .length     = static_cast<uint32_t>(end_ptr - beg_ptr)
            };
        }

        static SourceLoc from_ptrs(const char* beg_ptr, const char* end_ptr, const char* central_pt) {
            return SourceLoc{
                .ptr        = beg_ptr,
                .central_pt = central_pt,
                .length     = static_cast<uint32_t>(end_ptr - beg_ptr)
            };
        }

        static SourceLoc from_ptr_and_length(const char* beg_ptr, uint32_t length) {
            return SourceLoc{
                .ptr        = beg_ptr,
                .central_pt = beg_ptr,
                .length     = length
            };
        }

        const char* begin() const {
            return ptr;
        }

        const char* end() const {
            return ptr + length;
        }
    };
}

#endif // SOURCE_H
