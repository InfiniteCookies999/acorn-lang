#ifndef UTIL_H
#define UTIL_H

#include <fstream>

#include "PageAllocator.h"

// Force inlining
#ifdef _MSC_VER
#define finline [[msvc::forceinline]]
#else
#define finline __attribute__((always_inline))
#endif

namespace acorn {

    template<typename T, typename V>
    T as(V&& v) {
        return static_cast<T>(v);
    }

    template<typename P>
    bool read_entire_file(const P& path, char*& buffer, size_t& length, PageAllocator& allocator) {
        std::ifstream stream(path, std::ios::binary);

        if (!stream) {
            return false;
        }

        stream.seekg(0, std::ios::end);
        std::streamsize tlength = stream.tellg();
        stream.seekg(0, std::ios::beg);

        length = as<size_t>(tlength);
        buffer = as<char*>(allocator.allocate(length + 1));

        stream.read(buffer, length);
        buffer[length] = 0; // Null terminate.

        return true;
    }

    enum class Color {

        Black,
        DarkGray,

        Blue,
        BrightBlue,

        Green,
        BrightGreen,

        Cyan,
        BrightCyan,

        Red,
        BrightRed,

        Magenta,
        BrightMagenta,

        Yellow,
        BrightYellow,

        White,
        BrightWhite,

    };

    template<typename T, typename V>
    static bool fits_in_range(V value) {
        bool b = value <= std::numeric_limits<T>::max();
        return value <= std::numeric_limits<T>::max();
    }

    void set_color(Color color);

    std::string& trim_leading(std::string& s);

    std::string& trim_trailing(std::string& s);

    std::string& trim(std::string& s);

    size_t get_system_page_size();

}

#endif // UTIL_H