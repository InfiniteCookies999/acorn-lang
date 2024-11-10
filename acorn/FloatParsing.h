#ifndef FLOAT_PARSING_H
#define FLOAT_PARSING_H

#include <llvm/ADT/StringRef.h>

namespace acorn {

    class PageAllocator;

    enum class FloatParseError {
        None,
        Overflow,
        Underflow
    };

    void initialize_float_parsing(PageAllocator& allocator);

    std::pair<float, FloatParseError> parse_float32_bits(PageAllocator& allocator, llvm::StringRef text);

    std::pair<double, FloatParseError> parse_float64_bits(PageAllocator& allocator, llvm::StringRef text);

}

#endif // FLOAT_PARSING_H