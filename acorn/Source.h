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

    // SourceLoc but with a pointer to the most relevant location
    // of the error. This is needed because if an error is too long
    // and needs to be cutoff then it can use this point and branch
    // forwards and backwards from this point.
    //
    // NOTE: Not extending the SourceLoc because we do not want any
    //       possible implicit conversions.
    //
    struct PointSourceLoc {
        const char* ptr;
        uint16_t    length;
        const char* point;
        uint16_t    point_length;

        const char* end() const {
            return ptr + length;
        }
    };

    struct SourceLoc {
        // TODO: consider exchanging this for a 32 bit integer.
        // This might improve performance since it would allow
        // tokens to fit into 8 bytes.
        const char* ptr;
        uint16_t    length;

        static SourceLoc from_ptrs(const char* start_ptr, const char* end_ptr) {
            return SourceLoc{
                .ptr = start_ptr,
                .length = static_cast<uint16_t>(end_ptr - start_ptr)
            };
        }

        PointSourceLoc to_point_source() const {
            return PointSourceLoc{
                .ptr = ptr,
                .length = length,
                .point = ptr,
                .point_length = length
            };
        }

        const char* end() const {
            return ptr + length;
        }
    };
}

#endif // SOURCE_H