#include "Util.h"

#ifdef _WIN32
#include <Windows.h>
#undef min
#undef max
#endif

#include <cctype>
#include <iomanip>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>

void acorn::set_color(Color color) {
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
#endif
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

size_t acorn::get_system_page_size() {
#ifdef _WIN32
    SYSTEM_INFO sys_info;
    GetSystemInfo(&sys_info);
    auto page_size = static_cast<size_t>(sys_info.dwPageSize);
    return page_size < 1024 ? 1024 : page_size;
#endif
}