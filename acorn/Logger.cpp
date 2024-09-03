#include "Logger.h"

#include <unordered_map>
#include <algorithm>
#include <ranges>
#include <regex>

#ifdef _WIN32
#include <Windows.h>
#undef min
#undef max
#endif

#include "Context.h"
#include "Type.h"
#include "SourceFile.h"

using enum acorn::Color;
using enum acorn::Logger::Stream;

acorn::Logger::mtx_type acorn::Logger::mtx;

void acorn::Logger::set_color(Stream stream, Color color) {
    acorn::set_color(color);
}

void acorn::Logger::print(Stream stream, Type* type) {
    print(stream, type->to_string());
}

acorn::Logger& acorn::Logger::begin_error(PointSourceLoc location, const std::function<void()>& print_cb) {

    main_location = location;
    primary_print_cb = print_cb;

    return *this;
}

acorn::Logger& acorn::Logger::add_line(const std::function<void()>& print_cb) {
    line_printers.push_back({ true, print_cb });
    return *this;
}

void acorn::Logger::global_error(Context& context, const std::function<void()>& print_cb) {
    mtx.lock();

    fmt_print(StdErr, "[%serror%s]", BrightRed, White);
    fmt_print(StdErr, "%s -> ", BrightWhite);
    set_color(StdErr, White);

    print_cb();

    fmt_print(StdErr, "%s.\n", White);

    if (context.inc_error_count()) {
        print_exceeded_errors_msg(context);
        mtx.unlock();
        exit(1);
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


void* acorn::Logger::get_handle(Stream stream) {
#ifdef _WIN32
    return GetStdHandle((stream == StdOut) ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE);
#endif
}

acorn::Logger::Logger(Context& context, SourceFile& file)
    : context(context), file(file)
{
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

    static size_t count_digits(size_t number) {
        size_t digits = 0;
        do {
            digits++;
            number /= 10;
        } while (number != 0);
        return digits;
    }

    struct PtrCalcInfo {
        const char* ptr;
        bool        exceeded;
    };

    static PtrCalcInfo calc_start_ptr(const char* ptr, const char* buffer_start, const char* low_point) {
        acorn_assert(*ptr != '\r' && *ptr != '\n', "Start pointer cannot contain new line");

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
            static_cast<size_t>(location.ptr > start_info.ptr ? location.ptr - start_info.ptr : 0),
            static_cast<size_t>(end_pt > org_end_pt ? end_pt - org_end_pt : 0)
        };
    }
}


void acorn::Logger::print_header(ErrCode error_code) {
    
    fmt_print(StdErr, "%serror", BrightRed), facing_length += 5;
    if (context.should_show_error_codes()) {
        fmt_print(StdErr, "%s[%s%s%s]", White, BrightBlue, static_cast<unsigned>(error_code), White);
        facing_length += 2 + count_digits(static_cast<unsigned>(error_code));
    }
    fmt_print(StdErr, "%s -> ", BrightWhite), facing_length += 4;

    const auto& path = file.path.empty() ? L"[buffer]" : file.path;
    fmt_print(StdErr, "%s%s", BrightCyan, path);
    facing_length += path.length();

    auto [line_number, column_number] = file.line_table.get_line_and_column_number(main_location.point);

    fmt_print(StdErr, "%s:%s%s", BrightWhite, BrightYellow, line_number);
    facing_length += std::to_string(line_number).length() + 1;
    fmt_print(StdErr, "%s:%s%s%s: ", BrightWhite, BrightYellow, column_number, BrightWhite);
    facing_length += std::to_string(column_number).length() + 3;

    primary_print_cb();

    fmt_print(StdErr, "%s.\n", White);

    for (const auto& printer : line_printers) {
        print(StdErr, std::string(facing_length, ' '));
        printer.print_cb();
        if (printer.add_period) {
            fmt_print(StdErr, "%s.", White);
        }
        print(StdErr, "\n");
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
        error_code_interceptor(error_code);
        mtx.unlock();
        return;
    }

    ErrorInfo info = collect_error_information(main_location, file);

    // Placed after collecting information to reduce the time locked.
    mtx.lock();

    print_header(error_code);

    auto print_bar = [pad = info.line_number_pad](bool include_new_line = true){
        fmt_print(StdErr, " %s %s|%s ", pad, BrightWhite, White);
        if (include_new_line) print(StdErr, "\n");
    };
    
    print_bar();

    size_t line_number = info.start_line_number;
    for (const auto& line : info.lines) {
        bool is_first = &line == &info.lines.front();
        bool is_last  = &line == &info.lines.back();
        
        // Displaying the line number and bar.
        auto line_number_pad_width = count_digits(info.last_line_number) - count_digits(line_number);
        auto line_number_pad = std::string(line_number_pad_width, ' ');
        fmt_print(StdErr, " %s%s%s %s| ", BrightYellow, line_number++, line_number_pad, White);

        // Printing the line.
        if (is_first && info.exceeded_start)
            fmt_print(StdErr, "%s...%s ", Green, White);
        else if (info.exceeded_start)
            print(StdErr, "    "); // Want to make sure the lines are still aligned as seen in the file.
        
        print(StdErr, line);

        if (is_last && info.exceeded_end)
            fmt_print(StdErr, " %s...%s", Green, White);
        print(StdErr, "\n");
        
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
                print(StdErr, std::string(underscore_offset + 1, ' '));
                fmt_print(StdErr, "%s%s", BrightRed, '^');
            }
        } else {
            print_bar(false);
            print(StdErr, std::string(underscore_offset, ' '));

            size_t underscore_width = line.length() - underscore_offset + dots_width;
            if (is_last) underscore_width -= info.end_width;

            std::string underscore = std::string(underscore_width, '~');
            if (!arrow_msg.msg.empty() && is_last) {
                underscore.back() = '^';
            }
            
            fmt_print(StdErr, "%s%s", BrightRed, underscore);
        }
        if (!arrow_msg.msg.empty() && is_last) {
            fmt_print(StdErr, " %s", arrow_msg.msg);
        }
        fmt_print(StdErr, "%s\n", White);

    }
    print(StdErr, "\n");

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

void acorn::Logger::print_exceeded_errors_msg(Context& context) {
    fmt_print(StdErr, "%s>>%s Exceeded maximum number of allowed errors (%s).\n"
                      "   Exiting early.\n", BrightRed, White, context.get_max_error_count());
}

void acorn::Logger::print(Stream stream, const std::wstring& s) {

    bool is_wide = std::ranges::any_of(s, [](wchar_t c) {
        return c > 0x7F;
        });
    if (is_wide) {
#ifdef _WIN32
        HANDLE handle = get_handle(stream);
        DWORD written;
        WriteConsoleA(handle, s.c_str(), static_cast<DWORD>(s.length()), &written, nullptr);
#endif
    } else {
        // narrowing the wstring to string.
        std::string narrow_string;
        narrow_string.reserve(s.size());
        for (wchar_t wc : s) {
            narrow_string += static_cast<char>(wc);
        }
        print(stream, narrow_string);
    }
}

void acorn::Logger::print(Stream stream, const char* s, size_t length) {
#ifdef _WIN32
    HANDLE handle = get_handle(stream);
    DWORD written;
    WriteConsoleA(handle, s, static_cast<DWORD>(length), &written, nullptr);
#endif
}
