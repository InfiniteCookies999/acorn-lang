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

    static std::string tabs_to_spaces(const std::string& s) {
        return std::regex_replace(s, std::regex("\t"), "    ");
    }

    static Logger::InfoLines convert_to_lines(const char* start, const char* end) {

        Logger::InfoLines lines;

        std::string cur_line;
        const char* cur_line_ptr = start;

        const char* ptr = start;
        while (ptr < end) {
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

    static bool is_newline(char ch) {
        return ch == '\r' || ch == '\n';
    }

    static bool compare_leading_whitespace(int* count,
                                           int leading_count,
                                           int pline_leading_count,
                                           int pline_leading_cutoff_count) {
        int cutoff_whitespace = leading_count - pline_leading_count;
        if (cutoff_whitespace < 0) {
            // TODO
            // It is possible the characters are behind the cut off so checking
            // that and discardinging the whole line if so.

            // Characters cut off due to them being behind the cutoff.
            //
            // (66 + 33
            //       ...            44) + 66;
            //
            int is_behind = (leading_count - pline_leading_cutoff_count) < 0;
            if (is_behind) {
                return false;
            }
            // else there is nothing to add to the count the characters are behind
            // the primary line but not to the point that they are cut off.
        } else {
            // the whitespace goes past the shared whitespace with the primary
            // line so this must be considered part of the count.
            *count += cutoff_whitespace;
        }

        return true;
    }

    static PtrCalcInfo traverse_backwards(const char* ptr,
                                          const char* low_point,
                                          const char* buffer_start,
                                          int& pline_leading_count,
                                          int& pline_leading_cutoff_count,
                                          int& total_backwards_line_characters) {

        int count = 0;

        auto eat_line_characters = [buffer_start, &count](const char*& ptr) {
            int possible_leading_count = 0;
            while (ptr > buffer_start && !is_newline(*ptr)) {
                if (is_whitespace(*ptr)) {
                    if (*ptr == '\t') possible_leading_count += 4;
                    else              possible_leading_count += 1;
                } else {
                    count += possible_leading_count + 1;
                    possible_leading_count = 0;

                    if (count >= CUTOFF_LIMIT) {
                        break;
                    }
                }
                --ptr;
            }
            return possible_leading_count;
        };

        // Handling the primary line first since it has extra rules
        // that must be handled very carefully.

        pline_leading_count = eat_line_characters(ptr);

        total_backwards_line_characters = count + pline_leading_count;

        // Try and take 30 characters and the remainder are cut off.
        pline_leading_cutoff_count = total_backwards_line_characters - CUTOFF_LIMIT;
        if (pline_leading_cutoff_count < 0) pline_leading_cutoff_count = 0;

        // Checking if the primary line has too many characters and ending early.
        if (count >= CUTOFF_LIMIT) {
            return { ptr, true };
        }

        // Should not be possible that the non cut off leading count is larger than
        // the entire leading whitespace amount.
        assert(pline_leading_cutoff_count >= 0 && "");

        int line_count = 0;
        const char* prev_line_start = ptr;
        while (ptr > buffer_start) {

            ++line_count;
            if (line_count >= 2 || ptr <= low_point) {
                break;
            }

            prev_line_start = ptr;

            // Eating the new line. New lines are not included in the count.
            if (*ptr == '\n' && ptr - 1 > buffer_start && *(ptr - 1) == '\r') {
                ptr -= 2; // Skip over '\r\n'.
            } else {
                ptr -= 1; // Skip over '\r'.
            }

            // Traversing backwards so it is completely fine to simply
            // eat the characters without incrementing the count since
            // it is trailing.
            while (is_whitespace(*ptr) && !is_newline(*ptr)) {
                --ptr;
            }

            int leading_count = eat_line_characters(ptr);

            if (!compare_leading_whitespace(&count, leading_count, pline_leading_count, pline_leading_cutoff_count)) {
                // Well the characters were cut off so discarding this line entirely.
                ptr = prev_line_start;
                break;
            }

            // Not stopping the pointer traversal when there is leading whitespace
            // is good since common whitespace will just be cut off later and the
            // `start_width` will be processed to handle it correctly.
            if (count >= CUTOFF_LIMIT) {
                // Just cut off the line because it is too far over to the right.
                ptr = prev_line_start;
                break;
            }
        }

        // The comparison to `low_point` must above the next loop because we traverse
        // forward over whitespace to prevent empty lines but that also traverses forward
        // over trailing whitespace which means the `ptr` may have reached the `low_point`
        // but then once the pointer is moved forward it no longer has that information.
        bool exceeded = count >= CUTOFF_LIMIT || pline_leading_cutoff_count > 0 || ptr > low_point;

        // Traversing forward again to see if the lines are only made
        // up of whitespace. This can happen since it is possible that
        // when a line is discarded that there was an empty line between
        // the discarded line and a previously included line leading to
        // an empty line being includeded.
        const char* forward_ptr = ptr;
        while (is_whitespace(*forward_ptr)) {
            if (is_newline(*forward_ptr)) {
                // Hit a new line before encountering non-whitespace
                // means the whole line gets discarded!
                if (*ptr == '\r' && *(ptr + 1) == '\n') {
                    ptr += 2; // Skip '\r\n'.
                } else {
                    ptr += 1;
                }

                ptr = forward_ptr;
            }
            ++forward_ptr;
        }

        // Moving the pointer forward one if it did not hit
        // the beginning of the buffer because the pointer
        // is moved before checking the character.
        if (ptr != buffer_start) {
            ++ptr;
        }
        return { ptr, exceeded };
    }


    static PtrCalcInfo traverse_forward(const char* ptr,
                                        const char* high_point,
                                        const char* buffer_end,
                                        int pline_leading_count,
                                        int pline_leading_cutoff_count,
                                        int total_backwards_line_characters) {

        const char* ptr_line_start = nullptr;

        int count      = 0;
        int line_count = 0;

        int possible_trailing_whitespace = 0;
        const char* prev_line_start = ptr;
        while (ptr < buffer_end) {

            if (is_newline(*ptr)) {

                // Reset trailing whitespace for the next line.
                possible_trailing_whitespace = 0;

                ++line_count;
                if (line_count >= 4 || ptr >= high_point) {
                    break;
                }

                prev_line_start = ptr;

                // Eating the new line. New lines are not included in the count.
                if (*ptr == '\r' && ptr + 1 < buffer_end && *(ptr + 1) == '\n') {
                    ptr += 2; // Skip '\r\n'.
                } else {
                    ptr += 1;
                }

                // Counting leading whitespace to determine how much of it
                // can be cut off.
                int leading_count = 0;
                while (is_whitespace(*ptr) && !is_newline(*ptr)) {
                    if (*ptr == '\t') leading_count += 4;
                    else              leading_count += 1;
                    ++ptr;
                }

                // Want to ignore the leading whitespace that exists up until the point
                // so `total_backwards_line_characters` is ignored.
                //
                //                  |-- point
                //                  V
                //         int[2] a = 314 + 5234
                //                  + 1223 + 21;
                //         |        |
                //         *--------*
                //         |
                //         whitespae needing ignored
                if (!compare_leading_whitespace(&count, leading_count, total_backwards_line_characters, pline_leading_cutoff_count)) {
                    // Well the characters were cut off so discarding this line entirely.
                    ptr = prev_line_start;
                    break;
                }
            }

            if (is_whitespace(*ptr)) {
                if (*ptr == '\t') possible_trailing_whitespace += 4;
                else              possible_trailing_whitespace += 1;
            } else {
                count += possible_trailing_whitespace + 1;
                possible_trailing_whitespace = 0;

                if (count >= CUTOFF_LIMIT) {

                    // Need to traverse backwards a bit to not include
                    // a bunch of extra characters that are not meant to be included.
                    while (count > CUTOFF_LIMIT) {
                        if (*ptr == '\t') count -= 4;
                        else              count -= 1;
                        --ptr;
                    }

                    // It is possible the last character lands on the end point in which
                    // case the exceeded clause should not be considered overflowed since
                    // every relevent character in on the line.
                    if (ptr == high_point) {
                        --count;
                    }

                    break;
                }
            }

            ++ptr;
        }

        // The comparison to `high_point` must above the next loop because we traverse
        // back over whitespace to prevent empty lines but that also traverses backwards
        // over trailing whitespace which means the `ptr` may have reached the `high_point`
        // but then once the pointer is moved forward it no longer has that information.
        bool exceeded = count >= CUTOFF_LIMIT || ptr < high_point;

        // Because the algorithm will continue to the next line when the count
        // has not been reached it is possible that it gets to the next line finds out
        // that the whitespace overflows the line. This can lead to empty lines so now
        // traversing backwards to avoid any empty.
        while (is_whitespace(*ptr)) {
            --ptr;
        }

        return { ptr, exceeded };
    }

    static std::pair<PtrCalcInfo, PtrCalcInfo> get_range_pointers(PointSourceLoc location,
                                                                  Buffer buffer,
                                                                  Logger::ArrowPosition arrow_position,
                                                                  int& pline_leading_cutoff_count) {

        auto point_begin  = location.point;
        auto point_end    = location.point + location.point_length;

        auto low_point    = location.ptr;
        auto high_point   = location.ptr + location.length;

        auto buffer_begin = buffer.content;
        auto buffer_end   = buffer.content + buffer.length;

        // Check for case in which a single token is really long such as a very
        // long string.
        if (location.point_length > 30) {
            if (arrow_position == Logger::ArrowPosition::After) {
                point_begin = point_end - 20;
            } else {
                point_end = point_begin + 20;
            }
        }

        int pline_leading_count        = 0;
        int total_backwards_line_characters = 0;

        auto start_info = traverse_backwards(point_begin,
                                             low_point,
                                             buffer_begin,
                                             pline_leading_count,
                                             pline_leading_cutoff_count,
                                             total_backwards_line_characters);
        auto end_info   = traverse_forward(point_end,
                                           high_point,
                                           buffer_end,
                                           pline_leading_count,
                                           pline_leading_cutoff_count,
                                           total_backwards_line_characters);

        return { start_info, end_info };
    }

    static Logger::ErrorInfo collect_error_information(PointSourceLoc location,
                                                       SourceFile& file,
                                                       Logger::ArrowPosition arrow_position) {

        int prim_line_cutoff_lead_trim = 0;
        auto [start_info, end_info] = get_range_pointers(location,
                                                         file.buffer,
                                                         arrow_position,
                                                         prim_line_cutoff_lead_trim);
        Logger::InfoLines lines = convert_to_lines(start_info.ptr, end_info.ptr + 1);

        // Convert tabs to spaces for all ours lines.
        lines = lines
              | std::views::transform([](auto& line_info) {
                   return std::pair<std::string, const char*>{
                       trim_trailing(std::get<0>(line_info)),
                       std::get<1>(line_info)
                   };
                })
              | std::ranges::to<Logger::InfoLines>();

        auto [start_line_number    , _1] = file.line_table.get_line_and_column_number(start_info.ptr);
        auto [primary_line_number  , _2] = file.line_table.get_line_and_column_number(location.point);
        auto [location_start_number, _3] = file.line_table.get_line_and_column_number(location.ptr);

        auto last_line_number = start_line_number + lines.size() - 1;
        const auto line_number_pad = std::string(count_digits(last_line_number), ' ');

        const char* end_pt = end_info.ptr + 1;
        const char* org_end_pt = location.ptr + location.length;

        size_t start_width = 0, end_width = 0;
        if (!lines.empty()) {

            { // Calculate the start width
                const char* ptr = start_info.ptr;

                bool traverse_to_loc = location_start_number == primary_line_number;

                while ((!traverse_to_loc && is_whitespace(*ptr)) ||
                       (traverse_to_loc && ptr < location.ptr)) {
                    if (*ptr == '\t') start_width += 4;
                    else              start_width += 1;
                    ++ptr;
                }

                start_width -= prim_line_cutoff_lead_trim;
            }

            // Calculate the end width
            if (org_end_pt < end_pt) {
                const char* ptr = org_end_pt;
                size_t trail_count = 0;
                while (ptr != end_pt) {
                    if (is_whitespace(*ptr)) {
                        // May be trailing in which case it is not included
                        // in the end_width since calculations made here are
                        // related to what is displayed.
                        if (*ptr == '\t') trail_count += 4;
                        else              trail_count += 1;
                    } else {
                        end_width += trail_count + 1;
                        trail_count = 0;
                    }
                    ++ptr;
                }
            } // else had to cut off early so end width.
        }

        return Logger::ErrorInfo{
            std::move(lines),
            start_line_number,
            last_line_number,
            line_number_pad,
            start_info.exceeded, end_info.exceeded,
            start_width,
            end_width,
            prim_line_cutoff_lead_trim,
            primary_line_number
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

    size_t primary_line_leading_trim = static_cast<size_t>(info.prim_line_cutoff_lead_trim);
    for (const auto& line_info : info.lines) {
        bool is_first = &line_info == &info.lines.front();
        bool is_last  = &line_info == &info.lines.back();

        std::string line = tabs_to_spaces(std::get<0>(line_info));
        line = line.substr(primary_line_leading_trim, line.length());

        // Displaying the line number and bar.
        auto line_number_pad_width = count_digits(info.last_line_number) - count_digits(line_number);
        auto line_number_pad = std::string(line_number_pad_width, ' ');
        fmt_print(" %s%s%s %s|%s ",
                  line_number == info.primary_line_number ? BrightYellow : DarkGray,
                  line_number,
                  line_number_pad,
                  BrightWhite,
                  White);
        ++line_number;

        // Printing the line.
        if (is_first && info.exceeded_start)
            fmt_print("%s...%s ", Green, White);
        else if (info.exceeded_start)
            print("    "); // Want to make sure the lines are still aligned as seen in the file.

        if (line.empty()) {
            print("\n");
            continue;
        }


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
                        if (!is_whitespace(previous_char)) {
                            print(previous_char);
                        }
                        print(*msg_start_ptr);
                        if (!is_whitespace(next_char)) {
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
            // TODO: shouldn't this be count leading whitespace?
            //       because this gets used to calculate underscore_width
            //       this messing up for \v or other odd characters could
            //       potentially break.
            else underscore_offset += count_leading_spaces(line);

            print_bar(false);
            total_printed_characters_for_line += underscore_offset;
            print(std::string(underscore_offset, ' '));

            size_t underscore_width = (line.length() + dots_width) - underscore_offset;
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
            if (!is_whitespace(previous_char)) {
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

    ErrorInfo info = collect_error_information(main_location,
                                               file,
                                               arrow_msg.msg.empty() ? ArrowPosition::None : arrow_msg.position);

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
