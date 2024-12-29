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

// Forward declaration.
bool acorn::disable_terminal_colors = false;

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

std::string& acorn::trim_leading(std::string& s) {
    auto view = s | std::views::drop_while(isspace);
    s.assign(view.begin(), view.end());
    return s;
}

std::string& acorn::trim_trailing(std::string& s) {
    auto view = s | std::views::reverse
                  | std::views::drop_while(isspace)
                  | std::views::reverse;
    s.assign(view.begin(), view.end());
    return s;
}

std::string& acorn::trim(std::string& s) {
    return trim_leading(trim_trailing(s));
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