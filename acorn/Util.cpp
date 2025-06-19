#include "Util.h"

#ifdef _WIN32
#include <Windows.h>
#undef min
#undef max
#else
#include <unistd.h>
#endif

#include <cctype>
#include <iomanip>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>
#include <sstream>
#include <fstream>

#include "Logger.h"
#include "SystemFiles.h"

// Forward declaration.
bool acorn::disable_terminal_colors = false;

void acorn::set_terminal_codepoint_to_utf8() {
#if _WIN32
    SetConsoleOutputCP(CP_UTF8);
#endif
}

bool acorn::read_entire_file(const SystemPath& path, char*& buffer, size_t& length, PageAllocator& allocator) {
#if _WIN32
    // On windows ifstream expects it as a wide path.
    auto wpath = path.to_wide_string();
    std::ifstream stream(wpath, std::ios::binary);
#else
    auto utf8_path = path.to_utf8_string();
    std::ifstream stream(utf8_path, std::ios::binary);
#endif

    if (!stream) {
        return false;
    }

    stream.seekg(0, std::ios::end);
    std::streamsize tlength = stream.tellg();
    stream.seekg(0, std::ios::beg);

    length = static_cast<size_t>(tlength);
    buffer = static_cast<char*>(allocator.allocate(length + 1));

    stream.read(buffer, length);
    buffer[length] = 0; // Null terminate.

    return true;
}

void acorn::set_terminal_color(Stream stream, Color color) {
    if (disable_terminal_colors) {
        return;
    }

#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    static std::unordered_map<Color, WORD> color_map = {
        { Color::Black        , 0  },
        { Color::DarkGray     , 8  },
        { Color::Blue         , 1  },
        { Color::BrightBlue   , 9  },
        { Color::Green        , 2  },
        { Color::BrightGreen  , 10 },
        { Color::Cyan         , 3  },
        { Color::BrightCyan   , 11 },
        { Color::Magenta      , 5  },
        { Color::BrightMagenta, 13 },
        { Color::Red          , 4  },
        { Color::BrightRed    , 12 },
        { Color::Yellow       , 6  },
        { Color::BrightYellow , 14 },
        { Color::White        , 7  },
        { Color::BrightWhite  , 15 },
    };

    SetConsoleTextAttribute(handle, color_map[color]);
#else
    static std::unordered_map<Color, std::string> color_map ={
        { Color::Black        , "\033[0;30m" },
        { Color::DarkGray     , "\033[1;30m" },
        { Color::Blue         , "\033[0;34m" },
        { Color::BrightBlue   , "\033[1;34m" },
        { Color::Green        , "\033[0;32m" },
        { Color::BrightGreen  , "\033[1;32m" },
        { Color::Cyan         , "\033[0;36m" },
        { Color::BrightCyan   , "\033[1;36m" },
        { Color::Magenta      , "\033[0;35m" },
        { Color::BrightMagenta, "\033[1;35m" },
        { Color::Red          , "\033[0;31m" },
        { Color::BrightRed    , "\033[1;31m" },
        { Color::Yellow       , "\033[0;33m" },
        { Color::BrightYellow , "\033[1;33m" },
        { Color::White        , "\033[0;37m" },
        { Color::BrightWhite  , "\033[1;37m" },
    };
    int handle = stream == Stream::StdOut ? STDOUT_FILENO : STDERR_FILENO;
    auto code = color_map[color];
    // At the moment some of the code is using std::cout which means writing
    // directly will not work since there is a buffer used for std::cout. Simply
    // calling std::flush to force it to finish writing whatever it has in its
    // buffer.
    std::cout << std::flush;
    write(handle, code.c_str(), code.length());

#endif
}

uint64_t acorn::next_pow2(uint64_t value) {
    if (value <= 1) return 1;

    --value;

    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    value |= value >> 32;

    return value + 1;
}

std::string acorn::trim_leading(const std::string& s) {
    auto view = s | std::views::drop_while(is_whitespace);
    std::string result(view.begin(), view.end());
    return result;
}

std::string acorn::trim_trailing(const std::string& s) {
    auto view = s | std::views::reverse
                  | std::views::drop_while(is_whitespace)
                  | std::views::reverse;
    std::string result(view.begin(), view.end());
    return result;
}

std::string acorn::trim(const std::string& s) {
    return trim_leading(trim_trailing(s));
}

bool acorn::is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' || c == '\f';
}

llvm::SmallVector<std::string> acorn::split_by_whitespace(const std::string& s) {
    std::istringstream stream(s);
    llvm::SmallVector<std::string> tokens;

    std::string word;
    while (stream >> word) {
        tokens.push_back(word);
    }

    return tokens;
}

std::string acorn::wide_to_utf8(const wchar_t* str) {
    int length = static_cast<int>(wcslen(str));
    return wide_to_utf8(str, length);
}

std::string acorn::wide_to_utf8(const wchar_t* str, size_t length) {
#if WIN_OS
    if (length == 0) {
        return ""; // Documentation tells us it fails when length is zero.
    }

    // First determining the length of the resulting multibyte string.
    int utf8_length = WideCharToMultiByte(CP_UTF8,  // UTF-8 conversion
                                          0,        // TODO (maddie): special flags?
                                          str,      // Pointer to string to convert
                                          length,   // Length of wide string. May be -1 if null terminated
                                          nullptr,  // Pointer to the resulting utf8 string (Need to determine length first)
                                          0,        // Size of the utf8 string (Need to determine length first)
                                          nullptr,  // Pointer to a character if a character cannot be interpreted
                                          nullptr   // Pointer to a flag to indicate if the function uses a default character
    );
    if (utf8_length <= 0) {
        acorn_fatal("Failed to convert wide to utf8");
    }

    // Converting!
    std::string result(utf8_length, 0);
    WideCharToMultiByte(CP_UTF8,
                        0,
                        str,
                        length,
                        result.data(),
                        utf8_length,
                        nullptr,
                        nullptr
    );
    return result;
#else
    acorn_fatal("wide_to_utf8: not supported outside windows");
    return "";
#endif
}

std::wstring acorn::utf8_to_wide(const char* str) {
    int byte_length = static_cast<int>(strlen(str));
    return utf8_to_wide(str, byte_length);
}

std::wstring acorn::utf8_to_wide(const char* str, size_t byte_length) {
#if WIN_OS
    if (byte_length == 0) {
        return L""; // Documentation tells us it fails when length is zero.
    }

    // First determining the length of the resulting wide string.
    int wide_length = MultiByteToWideChar(CP_UTF8,      // UTF-8 conversion
                                          0,            // TODO (maddie): special flags?
                                          str,          // Pointer to string to convert
                                          byte_length,  // Length of utf8 string (in bytes). May be -1 if null terminated
                                          nullptr,      // Pointer to the resulting wide string (Need to determine length first)
                                          0             // Size of the wide string (Need to determine length first)
    );
    if (wide_length <= 0) {
        acorn_fatal("Failed to convert utf8 to wide");
    }

    // Converting!
    std::wstring result(wide_length, 0);
    MultiByteToWideChar(CP_UTF8,
                        0,
                        str,
                        byte_length,
                        result.data(),
                        wide_length

    );
    return result;
#else
    acorn_fatal("utf8_to_wide: not supported outside windows");
    return L"";
#endif
}

size_t acorn::get_system_page_size() {
#ifdef _WIN32
    SYSTEM_INFO sys_info;
    GetSystemInfo(&sys_info);
    auto page_size = static_cast<size_t>(sys_info.dwPageSize);
    return page_size < 1024 ? 1024 : page_size;
#else
    long page_size = sysconf(_SC_PAGE_SIZE);
    if (page_size == -1) {
        return 1024;
    }
    return page_size < 1024 ? 1024 : page_size;
#endif
}

