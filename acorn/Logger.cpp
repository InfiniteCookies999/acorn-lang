#include "Logger.h"

#include <unordered_map>
#include <algorithm>
#include <ranges>
#include <regex>

#if WIN_OS
#include <Windows.h>
#undef min
#undef max
#elif UNIX_OS
#include <unistd.h>
#include <stdio.h>
#endif

#include "Context.h"
#include "Type.h"
#include "SourceFile.h"

using enum acorn::Color;
using enum acorn::Stream;

// Forward declaring
std::function<void()> acorn::fatal_interceptor;

static size_t count_digits(size_t number) {
    size_t digits = 0;
    do {
        digits++;
        number /= 10;
    } while (number != 0);
    return digits;
}

// We want to use a recursive mutext because it is possible that
// on the same thread we are trying to print an error message we encounter
// a fatal error which needs to print. This would result in a recursive
// deadlock using a normal mutex.
static std::recursive_mutex mtx;

static std::atomic<bool> fatal_state_encountered = false;

// Explicit instantiation.
template class acorn::AbstractLogger<acorn::Logger>;
template class acorn::AbstractLogger<acorn::GlobalLogger>;

template<typename L>
void acorn::AbstractLogger<L>::print_exceeded_errors_msg(Context& context) {
    fmt_print("%s>>%s Exceeded maximum number of allowed errors (%s).\n"
              "   Exiting early.\n", BrightRed, White, context.get_max_error_count());
}

template<typename L>
void acorn::AbstractLogger<L>::print(Stream stream, Type* type) {
    print(stream, type->to_string());
}

template<typename L>
void acorn::AbstractLogger<L>::print(Stream stream, const std::wstring& s) {

#if WIN_OS

    bool is_wide = std::ranges::any_of(s, [](wchar_t c) {
        return c > 0x7F;
    });
        
    if (!is_wide) {
        std::string narrow_string;
        narrow_string.reserve(s.size());
        for (wchar_t wc : s) {
            narrow_string += static_cast<char>(wc);
        }

        HANDLE handle = get_handle(stream);
        DWORD written;
        WriteFile(handle, narrow_string.c_str(), static_cast<DWORD>(narrow_string.length()), &written, nullptr);
    } else {
        HANDLE handle = get_handle(stream);
        DWORD written;
        WriteFile(handle, s.c_str(), static_cast<DWORD>(s.length()) * sizeof(wchar_t), &written, nullptr);
    }
#elif UNIX_OS
    
    // Unix cannot have wide directory paths so we just convert to narrow.
    std::string narrow_string;
    narrow_string.reserve(s.size());
    for (wchar_t wc : s) {
        narrow_string += static_cast<char>(wc);
    }

    int handle = get_handle(stream);
    write(handle, narrow_string.c_str(), narrow_string.length());

#endif

}

template<typename L>
void acorn::AbstractLogger<L>::print(Stream stream, const char* s, size_t length) {
#if WIN_OS
    HANDLE handle = get_handle(stream);
    DWORD written;
    WriteConsoleA(handle, s, static_cast<DWORD>(length), &written, nullptr);
#elif UNIX_OS
    int handle = get_handle(stream);
    write(handle, s, length);
#endif
}

#if _WIN32
template<typename L>
void* acorn::AbstractLogger<L>::get_handle(Stream stream) {
#else
template<typename L>
int acorn::AbstractLogger<L>::get_handle(Stream stream) {
#endif

#if WIN_OS
    return GetStdHandle((stream == StdOut) ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE);
#elif UNIX_OS
    return stream == StdOut ? STDOUT_FILENO : STDERR_FILENO;
#endif
}


void acorn::Logger::set_color(Stream stream, Color color) {
    acorn::set_terminal_color(stream, color);
}

acorn::Logger& acorn::Logger::begin_error(PointSourceLoc location, const std::function<void()>& print_cb) {

    main_location = location;
    primary_print_cb = print_cb;

    return *this;
}

void acorn::GlobalLogger::end_error(ErrCode error_code) {
    
    if (fatal_state_encountered) {
        // Do not want to print other error messages when fatal errors occure
        // because we want fatal errors to appear as the last message!
        return;
    }

    mtx.lock();

    int facing_length = 0;

    fmt_print("[%serror%s]", BrightRed, White), facing_length += 7;
    if (context.should_show_error_codes()) {
        fmt_print("%s[%s%s%s]", White, BrightBlue, static_cast<unsigned>(error_code), White);
        facing_length += 2 + static_cast<int>(count_digits(static_cast<unsigned>(error_code)));
    }
    fmt_print("%s -> ", BrightWhite), facing_length += 4;
    set_terminal_color(stream, White);

    print_cb();

    fmt_print("%s.\n", White);

    if (context.inc_error_count()) {
        print_exceeded_errors_msg(context);
        mtx.unlock();
        exit(1);
    }

    for (const auto& printer : line_printers) {
        print(std::string(facing_length, ' '));
        printer.print_cb(*this);
        if (printer.add_period) {
            fmt_print("%s.", White);
        }
        print("\n");
    }

    mtx.unlock();
}

void acorn::Logger::debug(const std::function<void()>& print_cb) {
    std::lock_guard lock(mtx);

    fmt_print(Stream::StdOut, "(%sDebug%s): ", Color::BrightBlue, Color::White);
    print_cb();
    print(Stream::StdOut, "\n");
}

void acorn::Logger::info(const std::function<void()>& print_cb) {
    std::lock_guard lock(mtx);

    fmt_print(Stream::StdOut, "%s--%s ", Color::BrightCyan, Color::White);
    print_cb();
    print(Stream::StdOut, ".\n");
}

void acorn::Logger::fatal_internal(const char* cpp_file, int line, const std::function<void()>& print_cb) {
    std::string relative_file(cpp_file);
    size_t pos = relative_file.find_last_of("/\\");
    if (pos != std::string::npos) {
        relative_file = relative_file.substr(pos + 1);
    }

    // The mutex is recursive so if a thread happenings to fatal error while
    // writing it's own fatal error message then it will re-enter and write
    // the second error instead.
    {
        std::lock_guard lock(mtx);
        fmt_print(StdErr, "\n[%sInternal Error%s] ", BrightRed, White);
        fmt_print(StdErr, "@ (%s:%s) ", relative_file, line);
        print_cb();
        print(StdErr, "\n\n");
    }

    if (fatal_state_encountered) {
        throw FatalException();
    }
    
    fatal_state_encountered = true;

    if (fatal_interceptor) {
        fatal_interceptor();
    }
    exit(1);
}


static const long long CUTOFF_LIMIT = 30;

namespace acorn {

    // Gets characters behind the error location for helping display
    // where the error took place at.
    static const char* capture_backwards(Buffer buffer, const char* ptr) {
        const char* buf_beg = buffer.content;
        const char* start = ptr;

        // Move backwards till we hit a new line or start of the buffer.
        size_t count = 0;
        while (ptr > buf_beg && *ptr != '\n' && *ptr != '\r' && count++ < CUTOFF_LIMIT) {
            --ptr;
        }

        if (*ptr == '\r') {
            ++ptr;
        }
        if (*ptr == '\n') {
            ++ptr;
        }

        // Subtract one because it starts within the token.
        return ptr;
    }

    static std::string tabs_to_spaces(const std::string& s) {
        return std::regex_replace(s, std::regex("\t"), "    ");
    }

    static Logger::InfoLines convert_to_lines(const char* start, const char* end) {
        
        Logger::InfoLines lines;

        std::string cur_line;
        const char* cur_line_ptr = start;

        const char* ptr = start;
        while (ptr != end) {
            if (*ptr == '\r' || *ptr == '\n') {
                
                if (*ptr == '\r' && *(ptr + 1) == '\n') {
                    ptr += 2;
                } else {
                    ptr += 1;
                }

                lines.push_back({ cur_line, cur_line_ptr });
                cur_line = "";
                cur_line_ptr = ptr;

            } else {
                cur_line += *ptr;
                ++ptr;
            }
        }

        if (!cur_line.empty()) {
            lines.push_back({ cur_line, cur_line_ptr });
        }

        return lines;
    }

    static size_t count_leading_spaces(const std::string& s) {
        auto count = s.find_first_not_of(' ');
        return count == std::string::npos ? 0 : count;
    }

    struct PtrCalcInfo {
        const char* ptr;
        bool        exceeded;
    };

    static PtrCalcInfo calc_start_ptr(const char* ptr, const char* buffer_start, const char* low_point) {
        
        long long count = 0;
        while (ptr > buffer_start) {
            long long whitespace_count = 0;
            while (ptr > buffer_start && (*ptr == ' ' || *ptr == '\t')) {
                if (*ptr == ' ') whitespace_count += 1;
                else             whitespace_count += 4;
                --ptr;
            }
            if (*ptr == '\n' || *ptr == '\r') {
                // The whitespace was leading.
                while (ptr > buffer_start && (*ptr == '\n' || *ptr == '\r')) {
                    if (ptr < low_point) {
                        count += whitespace_count;
                        goto CalcStartFinishedLab;
                    }
                    --ptr;
                }
                // Skipping over trailing whitespace so it does
                // not get included in the count.
                while (ptr > buffer_start && (*ptr == ' ' || *ptr == '\t')) --ptr;
            } else {
                count += whitespace_count;
                if (count >= CUTOFF_LIMIT) goto CalcStartFinishedLab;
            }
            // Eating characters while not whitespace or new lines.
            while (ptr > buffer_start &&
                   *ptr != ' ' && *ptr != '\t' && *ptr != '\n' && *ptr != '\r') {
                --ptr;
                ++count;
                if (count >= CUTOFF_LIMIT) {
                    goto CalcStartFinishedLab;
                }
            }
        }

    CalcStartFinishedLab:
        ptr += std::max(count - CUTOFF_LIMIT, 0ll);
        if (*ptr == '\r' || *ptr == '\n') {
            ++ptr;
        }

        return {ptr, count >= CUTOFF_LIMIT};
    }

    static PtrCalcInfo calc_end_ptr(const char* ptr, const char* buffer_end, const char* high_point) {
        
        // TODO: There are still cases where this leads to less than ideal output. Because
        //       we are not including the leading whitespace it is possible that there is
        //       is way more leading whitespace on the next new line than the first line
        //       resulting in a very long line output. What should happen is it should start
        //       calculating whitespace again if it exceeds the line with the shortest leading
        //       whitespace. Obviously since we don't have the line information at this point
        //       this approach is not possible.
        const char* start = ptr;
        long long count = 0;
        while (ptr < buffer_end) {
            long long whitespace_count = 0;
            while (ptr < buffer_end && (*ptr == ' ' || *ptr == '\t')) {
                if (*ptr == ' ') whitespace_count += 1;
                else             whitespace_count += 4;
                ++ptr;
            }
            if (*ptr == '\n' || *ptr == '\r') {
                // The whitespace was trailing whitespace so it
                // does not get included in the count.
                while (ptr < buffer_end && (*ptr == '\n' || *ptr == '\r')) {
                    if (ptr > high_point) {
                        count += whitespace_count;
                        goto CalcEndFinishedLab;
                    }
                    ++ptr;
                }
                // Skipping over leading whitespace.
                while (ptr < buffer_end && (*ptr == ' ' || *ptr == '\t')) ++ptr;
            } else {
                count += whitespace_count;
                if (count >= CUTOFF_LIMIT) {
                    goto CalcEndFinishedLab;
                }
            }
            // Eating characters while not whitespace or new lines.
            while (ptr < buffer_end &&
                   *ptr != ' ' && *ptr != '\t' && *ptr != '\n' && *ptr != '\r') {
                ++ptr;
                ++count;
                if (count >= CUTOFF_LIMIT) {
                    goto CalcEndFinishedLab;
                }
            }
        }

    CalcEndFinishedLab:
        ptr -= std::max(count - CUTOFF_LIMIT, 0ll);
        if (*ptr == '\r' || *ptr == '\n' || ptr == buffer_end) {
            --ptr;
        }

        // We do not want to include the trailing whitespace because
        // later on calculations are done using the end point we calculate
        // here and each line is trailed which can lead to inconsistencies.
        while (*ptr == ' ' || *ptr == '\t' && ptr > start) {
            --ptr;
        }
        
        // If the end of the line is all whitespace then there is no reason
        // to consider it as having exceeded and include the ... later on
        // since there are no more characters that are meaningfully relevent
        // to the line.
        bool ends_in_whitespace = true;
        const char* eptr = ptr + 1;
        while (true) {
            if (*eptr == ' ' || *eptr == '\t') {
                ++eptr;
            } else if (*eptr == '\r' || *eptr == '\n') {
                break;
            } else {
                ends_in_whitespace = false;
                break;
            }
        }

        return {ptr, count >= CUTOFF_LIMIT && !ends_in_whitespace};
    }

    static std::pair<PtrCalcInfo, PtrCalcInfo> get_range_pointers(PointSourceLoc location, Buffer buffer) {
        const char* start_ptr = location.point;
        const char* end_ptr   = location.point + location.point_length;
        
        auto start_info = calc_start_ptr(start_ptr, buffer.content, location.ptr);
        auto end_info   = calc_end_ptr(end_ptr, buffer.content + buffer.length, location.ptr + location.length - 1);
        return {start_info, end_info};
    }

    static Logger::ErrorInfo collect_error_information(PointSourceLoc location, SourceFile& file) {
       
        //  [ start_width ]
        //  |             |
        //  |             location.ptr (Because the error is about the assignment)
        //  |             |
        //  |             |
        //  |             |                        [ end width        ]
        //  v             v                        v                  v
        //  int*        a = 14142 + 5233 + 66 + 212; // example comment
        //  ^           ~~~~~~~~~~~~~~~~~~~~~~~~~~~^
        //  |                                      |
        //  start_info.ptr                         end_info.ptr

        auto [start_info, end_info] = get_range_pointers(location, file.buffer);
        Logger::InfoLines lines = convert_to_lines(start_info.ptr, end_info.ptr + 1);

        // Convert tabs to spaces for all ours lines.
        std::ranges::for_each(lines, [](auto& line_info) {
            std::string trimmed_line = trim_trailing(std::get<0>(line_info));
            return std::pair<std::string, const char*>{
                trimmed_line,
                std::get<1>(line_info)
            };
        });

        auto [start_line_number, _] = file.line_table.get_line_and_column_number(start_info.ptr);

        auto last_line_number = start_line_number + lines.size() - 1;
        const auto line_number_pad = std::string(count_digits(last_line_number), ' ');

        const auto end_pt = end_info.ptr + 1, org_end_pt = location.ptr + location.length;
        return Logger::ErrorInfo{
            std::move(lines),
            start_line_number,
            last_line_number,
            line_number_pad,
            start_info.exceeded, end_info.exceeded,
            // start_width
            static_cast<size_t>(location.ptr > start_info.ptr ? location.ptr - start_info.ptr : 0),
            // end_width
            static_cast<size_t>(end_pt > org_end_pt ? end_pt - org_end_pt : 0)
        };
    }
}


void acorn::Logger::print_header(ErrCode error_code, const std::string& line_number_pad) {
    
    fmt_print("%serror", BrightRed), facing_length += 5;
    if (context.should_show_error_codes()) {
        fmt_print("%s[%s%s%s]", White, BrightBlue, static_cast<unsigned>(error_code), White);
        facing_length += 2 + count_digits(static_cast<unsigned>(error_code));
    }
    fmt_print("%s -> ", BrightWhite), facing_length += 4;

    const auto& path = file.path.empty() ? L"[buffer]" : file.path;
    fmt_print("%s%s", BrightCyan, path);
    facing_length += path.length();

    auto [line_number, column_number] = file.line_table.get_line_and_column_number(main_location.point);

    fmt_print("%s:%s%s", BrightWhite, BrightYellow, line_number);
    facing_length += std::to_string(line_number).length() + 1;
    fmt_print("%s:%s%s%s: ", BrightWhite, BrightYellow, column_number, BrightWhite);
    facing_length += std::to_string(column_number).length() + 3;

    primary_print_cb();

    fmt_print("%s.\n", White);

    facing_length -= line_number_pad.size() + 4;
    bool first_line = true;
    for (const auto& printer : line_printers) {
        fmt_print(" %s %s%s%s ", line_number_pad, BrightWhite, first_line ? '|' : '.', White);
        if (first_line)  first_line = false;
        print(std::string(facing_length, ' '));
        printer.print_cb(*this);
        if (printer.add_period) {
            fmt_print("%s.", White);
        }
        print("\n");
    }
}

void acorn::Logger::print_error_location(const ErrorInfo& info) {
    auto print_bar = [this, pad = info.line_number_pad](bool include_new_line = true){
        fmt_print(" %s %s|%s ", pad, BrightWhite, White);
        if (include_new_line) print("\n");
    };
    
    print_bar();

    size_t line_number = info.start_line_number;

    bool has_arrow_msg = !arrow_msg.msg.empty();
    bool arrow_after            = has_arrow_msg && arrow_msg.position == ArrowPosition::After;
    bool is_at_arrow_msg        = has_arrow_msg && arrow_msg.position == ArrowPosition::At;
    bool is_alongside_arrow_msg = has_arrow_msg && arrow_msg.position == ArrowPosition::Alongside;

    for (const auto& line_info : info.lines) {
        bool is_first = &line_info == &info.lines.front();
        bool is_last  = &line_info == &info.lines.back();
        
        // Displaying the line number and bar.
        auto line_number_pad_width = count_digits(info.last_line_number) - count_digits(line_number);
        auto line_number_pad = std::string(line_number_pad_width, ' ');
        fmt_print(" %s%s%s %s|%s ", BrightYellow, line_number++, line_number_pad, BrightWhite, White);

        // Printing the line.
        if (is_first && info.exceeded_start)
            fmt_print("%s...%s ", Green, White);
        else if (info.exceeded_start)
            print("    "); // Want to make sure the lines are still aligned as seen in the file.
        
        std::string line = trim_trailing(std::get<0>(line_info));
        print(line);

        if (is_last && info.exceeded_end)
            fmt_print(" %s...%s", Green, White);

        bool alongside_arrow_msg_on_line = false, alongside_arrow_msg_after_dots = false;
        if (is_last && is_alongside_arrow_msg) {

            auto line_length = std::get<0>(line_info).length();
            const char* line_start_ptr = std::get<1>(line_info);
            const char* line_end_ptr = line_start_ptr + line_length;
            const char* msg_start_ptr = arrow_msg.location.ptr;
            alongside_arrow_msg_on_line = msg_start_ptr >= line_start_ptr && msg_start_ptr <= line_end_ptr;
            if (!alongside_arrow_msg_on_line) {
                // Still want to print the alongside message. It may be on a seperate non-included
                // line or it may be on the same line but cutoff due to line printing constraints.
                size_t last_line_number = line_number - 1;

                const char* msg_start_ptr = arrow_msg.location.ptr;
                auto [line_number, _] = file.line_table.get_line_and_column_number(msg_start_ptr);

                alongside_arrow_msg_after_dots = line_number == last_line_number;
                if (alongside_arrow_msg_after_dots) {
                    if (msg_start_ptr != file.buffer.content) {
                        char previous_char = *(msg_start_ptr - 1);
                        char next_char     = *(msg_start_ptr + 1);
                        fmt_print(" %s", BrightWhite);
                        if (!std::isspace(previous_char)) {
                            print(previous_char);
                        }
                        print(*msg_start_ptr);
                        if (!std::isspace(next_char)) {
                            print(next_char);
                        }
                    }
                }    
            }
        }

        print("\n");
        
        // Displaying the underline/arrow and bar.
        size_t dots_width = info.exceeded_start ? 4 : 0;

        size_t total_printed_characters_for_line = 0;
        if (arrow_after) {
            if (is_last) {
                // This can happen sometimes when at the end of the buffer or within a new line.
                size_t offset = line.length() + dots_width;
                if (offset == 0) offset = 1;
                offset -= info.end_width;

                print_bar(false);
                total_printed_characters_for_line += offset + 1; // +1 for the caret ^
                print(std::string(offset, ' '));
                fmt_print("%s%s", BrightRed, '^');
            }
        } else {

            size_t underscore_offset = dots_width;
            if (is_first)
                underscore_offset += info.start_width;
            else underscore_offset += count_leading_spaces(line);

            print_bar(false);
            total_printed_characters_for_line += underscore_offset;
            print(std::string(underscore_offset, ' '));

            size_t underscore_width = line.length() - underscore_offset + dots_width;
            // This can happen sometimes when at the end of the buffer or within a new line.
            if (underscore_width == 0) underscore_width = 1;
            if (is_last) underscore_width -= info.end_width;

            total_printed_characters_for_line += underscore_width;
            std::string underscore = std::string(underscore_width, '~');
            if (is_at_arrow_msg && is_last) {
                underscore.back() = '^';
                total_printed_characters_for_line += 1;
            }
            
            fmt_print("%s%s", BrightRed, underscore);
        }

        total_printed_characters_for_line -= dots_width;

        if (!is_alongside_arrow_msg) {
            if (has_arrow_msg && is_last) {
                fmt_print(" %s", arrow_msg.msg);
            }
        } else if (is_last && alongside_arrow_msg_on_line) {
            const char* line_start_ptr = std::get<1>(line_info);
            const char* msg_start_ptr = arrow_msg.location.ptr;

            std::string up_to_msg_str = std::string(line_start_ptr, msg_start_ptr - line_start_ptr);
            up_to_msg_str = tabs_to_spaces(up_to_msg_str);

            size_t remaining_spaces = up_to_msg_str.size() - total_printed_characters_for_line;
            fmt_print("%s%s%s%s", BrightRed, std::string(remaining_spaces, ' '), "^ ",
                      arrow_msg.msg);

        } else if (alongside_arrow_msg_after_dots) {
            auto line_length = std::get<0>(line_info).length();
            const char* line_start_ptr = std::get<1>(line_info);
            const char* line_end_ptr   = line_start_ptr + line_length;
            const char* msg_start_ptr = arrow_msg.location.ptr;

            std::string up_to_msg_str = std::string(line_start_ptr, line_end_ptr - line_start_ptr);
            up_to_msg_str = tabs_to_spaces(up_to_msg_str);

            size_t remaining_spaces = up_to_msg_str.size() - total_printed_characters_for_line;
            remaining_spaces += 5; // For dots and dot spaces

            char previous_char = *(msg_start_ptr - 1);
            if (!std::isspace(previous_char)) {
                remaining_spaces += 1;
            }

            fmt_print("%s%s%s%s", BrightRed, std::string(remaining_spaces, ' '), "^ ",
                      arrow_msg.msg);
        }
        fmt_print("%s\n", White);

    }
}

void acorn::Logger::end_error(ErrCode error_code) {

    if (fatal_state_encountered) {
        // Do not want to print other error messages when fatal errors occure
        // because we want fatal errors to appear as the last message!
        return;
    }

    if (error_code_interceptor) {
        mtx.lock();
        if (context.inc_error_count()) {
            print_exceeded_errors_msg(context);
            mtx.unlock();
            exit(1);
        }
        auto [start_line_number, _] = file.line_table.get_line_and_column_number(main_location.point);
        std::string narrow_path;
        narrow_path.reserve(file.path.size());
        for (wchar_t wc : file.path) {
            narrow_path += static_cast<char>(wc);
        }
        error_code_interceptor(error_code, narrow_path, static_cast<int>(start_line_number));
        mtx.unlock();
        return;
    }

    ErrorInfo info = collect_error_information(main_location, file);

    // Placed after collecting information to reduce the time locked.
    mtx.lock();

    print_header(error_code, info.line_number_pad);

    if (context.should_show_error_location()) {
        print_error_location(info);
        print("\n");
    }

    ++num_errors;
    if (context.inc_error_count()) {
        print_exceeded_errors_msg(context);
        mtx.unlock();
        exit(1);
    }
    mtx.unlock();

    // Cleaning up!
    main_location = {};
    arrow_msg = {};
    facing_length = 0;
    main_location = {};
    primary_print_cb = {};
    line_printers.clear();

}
