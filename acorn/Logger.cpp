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
using mtx_type = std::recursive_mutex;
static mtx_type mtx;


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
    acorn::set_color(stream, color);
}

acorn::Logger& acorn::Logger::begin_error(PointSourceLoc location, const std::function<void()>& print_cb) {

    main_location = location;
    primary_print_cb = print_cb;

    return *this;
}

void acorn::GlobalLogger::end_error(ErrCode error_code) {
    mtx.lock();

    int facing_length = 0;

    fmt_print("[%serror%s]", BrightRed, White), facing_length += 7;
    if (context.should_show_error_codes()) {
        fmt_print("%s[%s%s%s]", White, BrightBlue, static_cast<unsigned>(error_code), White);
        facing_length += 2 + count_digits(static_cast<unsigned>(error_code));
    }
    fmt_print("%s -> ", BrightWhite), facing_length += 4;
    set_color(stream, White);

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
    std::lock_guard<mtx_type> lock(mtx);

    fmt_print(Stream::StdOut, "(%sDebug%s): ", Color::BrightBlue, Color::White);
    print_cb();
    print(Stream::StdOut, "\n");
}

void acorn::Logger::info(const std::function<void()>& print_cb) {
    std::lock_guard<mtx_type> lock(mtx);

    fmt_print(Stream::StdOut, "%s--%s ", Color::BrightCyan, Color::White);
    print_cb();
    print(Stream::StdOut, ".\n");
}


void acorn::Logger::fatal_internal(const char* cpp_file, int line, const char* msg) {
    std::string relative_file(cpp_file);
    size_t pos = relative_file.find_last_of("/\\");
    if (pos != std::string::npos) {
        relative_file = relative_file.substr(pos + 1);
    }

    std::lock_guard<mtx_type> lock(mtx);
    fmt_print(StdErr, "\n[%sInternal Error%s] ", BrightRed, White);
    fmt_print(StdErr, "@ (%s:%s) %s\n\n", relative_file, line, msg);
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

    using InfoLines = llvm::SmallVector<std::string, 8>;

    static InfoLines split_lines(const std::string& str) {
        InfoLines lines;

        std::regex rgx("(\r\n|\n|\r)");
        std::sregex_token_iterator itr(str.begin(), str.end(), rgx, -1);
        std::sregex_token_iterator end;
        for (; itr != end; ++itr) {
            lines.push_back(*itr);
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
                if (count >= CUTOFF_LIMIT) goto CalcEndFinishedLab;
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
            if (*ptr == ' ') count -= 1;
            else             count -= 4;
            --ptr;
        }

        return {ptr, count >= CUTOFF_LIMIT};
    }

    static std::pair<PtrCalcInfo, PtrCalcInfo> get_range_pointers(PointSourceLoc location, Buffer buffer) {
        const char* start_ptr = location.point;
        const char* end_ptr   = location.point + location.point_length;
        
        auto start_info = calc_start_ptr(start_ptr, buffer.content, location.ptr);
        auto end_info   = calc_end_ptr(end_ptr, buffer.content + buffer.length, location.ptr + location.length - 1);
        return {start_info, end_info};
    }

    struct ErrorInfo {
        InfoLines   lines;
        size_t      start_line_number;
        size_t      last_line_number;
        std::string line_number_pad;
        bool        exceeded_start, exceeded_end;
        size_t      start_width;
        size_t      end_width;
    };

    static ErrorInfo collect_error_information(PointSourceLoc location, SourceFile& file) {
       
        auto [start_info, end_info] = get_range_pointers(location, file.buffer);
        std::string error_display = std::string(start_info.ptr, end_info.ptr - start_info.ptr + 1);
        error_display = tabs_to_spaces(error_display);
        

        InfoLines lines = split_lines(error_display);
        std::ranges::for_each(lines, trim_trailing);

        auto [start_line_number, _] = file.line_table.get_line_and_column_number(start_info.ptr);

        auto last_line_number = start_line_number + lines.size() - 1;
        const auto line_number_pad = std::string(count_digits(last_line_number), ' ');

        const auto end_pt = end_info.ptr + 1, org_end_pt = location.ptr + location.length;
        return ErrorInfo{
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

void acorn::Logger::end_error(ErrCode error_code) {
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
        error_code_interceptor(error_code, narrow_path, start_line_number);
        mtx.unlock();
        return;
    }

    ErrorInfo info = collect_error_information(main_location, file);

    // Placed after collecting information to reduce the time locked.
    mtx.lock();

    print_header(error_code, info.line_number_pad);

    auto print_bar = [this, pad = info.line_number_pad](bool include_new_line = true){
        fmt_print(" %s %s|%s ", pad, BrightWhite, White);
        if (include_new_line) print("\n");
    };
    
    print_bar();

    size_t line_number = info.start_line_number;
    for (const auto& line : info.lines) {
        bool is_first = &line == &info.lines.front();
        bool is_last  = &line == &info.lines.back();
        
        // Displaying the line number and bar.
        auto line_number_pad_width = count_digits(info.last_line_number) - count_digits(line_number);
        auto line_number_pad = std::string(line_number_pad_width, ' ');
        fmt_print(" %s%s%s %s|%s ", BrightYellow, line_number++, line_number_pad, BrightWhite, White);

        // Printing the line.
        if (is_first && info.exceeded_start)
            fmt_print("%s...%s ", Green, White);
        else if (info.exceeded_start)
            print("    "); // Want to make sure the lines are still aligned as seen in the file.
        
        print(line);

        if (is_last && info.exceeded_end)
            fmt_print(" %s...%s", Green, White);
        print("\n");
        
        // Displaying the underline/arrow and bar.
        bool arrow_after = !arrow_msg.msg.empty() && arrow_msg.loc == ArrowLoc::After;
        size_t dots_width = info.exceeded_start ? 4 : 0;
        size_t underscore_offset = dots_width;
        if (is_first)
            underscore_offset += info.start_width;
        else underscore_offset += count_leading_spaces(line);
        if (arrow_after) {
            if (is_last) {
                print_bar(false);
                print(std::string(underscore_offset + 1, ' '));
                fmt_print("%s%s", BrightRed, '^');
            }
        } else {
            print_bar(false);
            print(std::string(underscore_offset, ' '));

            size_t underscore_width = line.length() - underscore_offset + dots_width;
            // This can happen sometimes when at the end of the buffer of within a new line.
            if (underscore_width == 0) underscore_width = 1;
            if (is_last) underscore_width -= info.end_width;

            std::string underscore = std::string(underscore_width, '~');
            if (!arrow_msg.msg.empty() && is_last) {
                underscore.back() = '^';
            }
            
            fmt_print("%s%s", BrightRed, underscore);
        }
        if (!arrow_msg.msg.empty() && is_last) {
            fmt_print(" %s", arrow_msg.msg);
        }
        fmt_print("%s\n", White);

    }
    print("\n");

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
