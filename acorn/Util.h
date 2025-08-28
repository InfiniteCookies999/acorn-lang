#ifndef UTIL_H
#define UTIL_H

#include <llvm/ADT/SmallVector.h>

#include "PageAllocator.h"

// Force inlining
#ifdef _MSC_VER
#define finline [[msvc::forceinline]]
#else
#define finline __attribute__((always_inline))
#endif

#define WIN_OS  (_WIN32 != 0)
#define MAC_OS  (defined(__APPLE__) && defined(__MACH__))
#define UNIX_OS (defined(__unix__) || defined(__unix) || defined(__linux__) || MAC_OS)

#define IS_64_BITS (defined(__x86_64__) || defined(__AMD64__) || _WIN64 || defined(__aarch64__) || defined(__arm64__))

namespace acorn {

    uint64_t next_pow2(uint64_t value);

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

    enum class Stream {
        StdOut,
        StdErr
    };

    template<typename F>
    struct Deferer {
        F cb;
        Deferer(F&& cb) : cb(cb) {}
        ~Deferer() { cb(); }
    };

    template<typename F>
    Deferer<F> new_deferer(F&& cb) {
        return Deferer<F>(std::forward<F>(cb));
    }

#define defer_name3(x, y) x##y
#define defer_name2(x, y) defer_name3(x, y)
#define defer_name(x) defer_name2(x, __COUNTER__)
#define defer(code) auto defer_name(_defer_val) = new_deferer([&] { code; });


    template<typename T, typename V>
    static bool fits_in_range(V value) {
        if constexpr (std::is_signed_v<T> && std::is_signed_v<V>) {
            return value <= std::numeric_limits<T>::max() &&
                   value >= std::numeric_limits<T>::min();
        }
        return value <= std::numeric_limits<T>::max();
    }

    void set_terminal_codepoint_to_utf8();

    extern bool disable_terminal_colors;
    void set_terminal_color(Stream stream, Color color);

    std::string trim_leading(const std::string& s);

    std::string trim_trailing(const std::string& s);

    std::string trim(const std::string& s);

    // Our own is_space because std::isspace if passed a char
    // may think it is utf8 then fail asserts.
    bool is_whitespace(char c);

    llvm::SmallVector<std::string> split_by_whitespace(const std::string& s);

    std::string utf16_to_utf8(const wchar_t* str);
    std::string utf16_to_utf8(const wchar_t* str, size_t length);

    std::wstring utf8_to_utf16(const char* str);
    std::wstring utf8_to_utf16(const char* str, size_t byte_length);

    size_t get_system_page_size();

    bool is_valid_utf8_codepoint(uint32_t codepoint, size_t num_bytes, bool& is_overlong);

    uint32_t get_utf8_codepoint(const char* ptr, size_t& num_bytes, bool& is_valid, bool& is_overlong);
    size_t get_utf8_byte_distance(const char* ptr, bool& is_valid, bool& is_overlong);

    void codepoint_to_utf8(uint32_t codepoint, std::string& dest_string, bool& is_valid);

}

#endif // UTIL_H
