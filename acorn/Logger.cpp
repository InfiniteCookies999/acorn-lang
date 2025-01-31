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

    print(White);
    if (primary_has_period) {
        print(".");
    }
    print("\n");

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

    static size_t count_leading_whitespace(const std::string& s) {
        auto count = s.find_first_not_of(" \t\n\r\v\f");
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
                                           int leading_ws_count,
                                           int pline_leading_cutoff_count,
                                           int pline_total_backwards_characters) {
        int remaining_whitespace = leading_ws_count - pline_leading_cutoff_count;
        if (remaining_whitespace < 0) {
            // There was not enough whitespace after cutting character off so
            // the text falls behind the buffer and must be removed from the display.
            //
            //          |-- this text is behind the display buffer and must be discarded.
            //          v
            // (66 + 33 +
            //             ...        44 + 66;
            //
            return false;
        } else {
            // While it might seem as though we can just go ahead and add these characters
            // to the count this leads to a less than ideal situation. Adding the whitespace
            // characters to the count when these characters do not even exceed the backwards
            // traversal count would mean arbitrarily displaying less characters because
            // the whole window was shifted over.
            int chars_past_backwards_chars_count = remaining_whitespace - pline_total_backwards_characters;
            if (chars_past_backwards_chars_count > 0) {
                *count += chars_past_backwards_chars_count;
            }
        }
        return true;
    }

    // This function helps in collecting the start of where within the file buffer
    // to begin showing a visual display of the error.
    //
    // This function takes `ptr`, which is the start of the primary location of the
    // error, and tries to move the `ptr` backwards until it has either reached the
    // start of the line that `low_point` lies on (where `low_point` is the start of
    // the part of an expression that errored out) or until a condition is reached in
    // which it would be unreasonable to keep collecting characters since it would
    // cause the display to look bad.
    //
    // Stopping conditions:
    // 1. If processing the line that the `ptr` starts on it will stop if the total
    //    non-whitespace characters reaches `CUTOFF_LIMIT`.
    // 2. If processing lines other than the line that `ptr` starts on then it will
    //    stop and discard the line entirely if the non-whitespace character plus
    //    leading whitespace that is not shared between this line and the line that
    //    `ptr` starts on reaches `CUTOFF_LIMIT`. This is because it would result in
    //    display of characters that are pushed very far over to the right and not
    //    look correct.
    // 3. The characters wind up behind the visual display and would be cutoff due
    //    to the trimming of leading whitespace from shared lines.
    // 4. If the line count reaches above 2.
    //
    static PtrCalcInfo traverse_backwards(const char* ptr,
                                          const char* low_point,
                                          const char* buffer_start,
                                          int& pline_leading_ws_count,
                                          int& pline_leading_ws_cutoff_count,
                                          int& pline_leading_non_ws_cutoff_count,
                                          int& pline_total_backwards_characters) {

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

        pline_leading_ws_count = eat_line_characters(ptr);

        pline_total_backwards_characters = count + pline_leading_ws_count;

        // Try and take `CUTOFF_LIMIT` characters and the remainder are cut off.
        pline_leading_ws_cutoff_count = pline_total_backwards_characters - CUTOFF_LIMIT;
        if (pline_leading_ws_cutoff_count < 0) pline_leading_ws_cutoff_count = 0;

        // Checking if the primary line has too many characters and ending early.
        if (count >= CUTOFF_LIMIT) {

            if (!is_newline(*ptr) && ptr > buffer_start) {
                const char* count_ptr = ptr - 1;
                while (!is_newline(*count_ptr) && count_ptr > buffer_start) {
                    ++pline_leading_non_ws_cutoff_count;
                    --count_ptr;
                }
            }

            return { ptr, true };
        }

        // Should not be possible that the non cut off leading count is larger than
        // the entire leading whitespace amount.
        assert(pline_leading_ws_cutoff_count >= 0 && "");

        int total_pline_leading_cutoff = pline_leading_ws_cutoff_count + pline_leading_non_ws_cutoff_count;
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

            if (!compare_leading_whitespace(&count, leading_count, total_pline_leading_cutoff, pline_total_backwards_characters)) {
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
        bool exceeded = count >= CUTOFF_LIMIT || pline_leading_ws_cutoff_count > 0 || ptr > low_point;

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

    // This function helps in collecting the end of where within the file buffer
    // to show a visual display of the error.
    //
    // This function takes `ptr`, which is the end of the primary location of the
    // error, and tries to move the `ptr` forwards until it has either reached the
    // end of the line that `high_point` lies on (where `high_point` is the end of
    // the expression that errored out) or until a condition is reached in which it
    // would be unreasonable to keep collecting characters since it would cause the
    // display to look bad.
    //
    // Stopping conditions:
    // 1. If the total distance between where `ptr` started and where `ptr` is at
    //    has reached above `CUTOFF_LIMIT`.
    // 2. The characters wind up behind the visual display and would be cutoff due
    //    to the trimming of leading whitespace from shared lines.
    // 3. If the line count reaches above 4.
    //
    static PtrCalcInfo traverse_forward(const char* ptr,
                                        const char* high_point,
                                        const char* buffer_end,
                                        int pline_leading_ws_count,
                                        int pline_leading_ws_cutoff_count,
                                        int pline_leading_non_ws_cutoff_count,
                                        int pline_total_backwards_characters) {

        const char* ptr_line_start = nullptr;

        int count      = 0;
        int line_count = 0;

        int total_pline_leading_cutoff = pline_leading_ws_cutoff_count + pline_leading_non_ws_cutoff_count;
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
                if (!compare_leading_whitespace(&count, leading_count, total_pline_leading_cutoff, pline_total_backwards_characters)) {
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

    // Get pointers that represent the start and end of where within the file to show a visual
    // display of the error.
    //
    static std::pair<PtrCalcInfo, PtrCalcInfo> get_range_pointers(PointSourceLoc location,
                                                                  Buffer buffer,
                                                                  Logger::ArrowPosition arrow_position,
                                                                  int& pline_leading_ws_cutoff_count,
                                                                  int& pline_leading_non_ws_cutoff_count,
                                                                  const llvm::SmallVector<PointSourceLoc>& individual_underlines) {

        auto point_begin  = location.point;
        auto point_end    = location.point + location.point_length;

        auto low_point    = location.ptr;
        auto high_point   = location.ptr + location.length;

        auto buffer_begin = buffer.content;
        auto buffer_end   = buffer.content + buffer.length;

        if (!individual_underlines.empty()) {

            for (const auto& underline_loc : individual_underlines) {
                auto underline_loc_end = underline_loc.point + underline_loc.point_length;
                if (underline_loc.point < point_begin) point_begin = underline_loc.point;
                if (underline_loc_end > point_end)     point_end = underline_loc_end;
            }

        }

        // Check for case in which a single token is really long such as a very
        // long string.
        if (individual_underlines.empty()) {
            if (location.point_length > 30) {
                if (arrow_position == Logger::ArrowPosition::After) {
                    point_begin = point_end - 20;
                } else {
                    point_end = point_begin + 20;
                }
            }
        }

        int pline_leading_ws_count = 0;
        int pline_total_backwards_characters = 0;

        auto start_info = traverse_backwards(point_begin,
                                             low_point,
                                             buffer_begin,
                                             pline_leading_ws_count,
                                             pline_leading_ws_cutoff_count,
                                             pline_leading_non_ws_cutoff_count,
                                             pline_total_backwards_characters);
        auto end_info   = traverse_forward(point_end,
                                           high_point,
                                           buffer_end,
                                           pline_leading_ws_count,
                                           pline_leading_ws_cutoff_count,
                                           pline_leading_non_ws_cutoff_count,
                                           pline_total_backwards_characters);

        return { start_info, end_info };
    }

    static Logger::ErrorInfo collect_error_information(PointSourceLoc location,
                                                       SourceFile& file,
                                                       Logger::ArrowPosition arrow_position,
                                                       const llvm::SmallVector<PointSourceLoc>& individual_underlines) {

        int pline_leading_ws_cutoff_count = 0;
        int pline_leading_non_ws_cutoff_count = 0;
        auto [start_info, end_info] = get_range_pointers(location,
                                                         file.buffer,
                                                         arrow_position,
                                                         pline_leading_ws_cutoff_count,
                                                         pline_leading_non_ws_cutoff_count,
                                                         individual_underlines);
        Logger::InfoLines lines = convert_to_lines(start_info.ptr, end_info.ptr + 1);

        // Convert tabs to spaces for all ours lines.
        lines = lines
              | std::views::transform([&lines, &start_info](auto& line_info) {
                   return Logger::LineInfo {
                       .unprocessed_text = trim_trailing(line_info.unprocessed_text),
                       .line_start_ptr   = line_info.line_start_ptr,
                       .is_first         = &line_info == &lines.front(),
                       .is_last          = &line_info == &lines.back(),
                       .dots_width       = static_cast<size_t>(start_info.exceeded ? 4 : 0)
                   };
                })
              | std::ranges::to<Logger::InfoLines>();

        size_t start_line_number          = file.line_table.get_line_number(start_info.ptr);
        size_t primary_line_number        = file.line_table.get_line_number(location.point);
        size_t location_start_line_number = file.line_table.get_line_number(location.ptr);

        auto last_line_number = start_line_number + lines.size() - 1;
        const auto line_number_pad = std::string(count_digits(last_line_number), ' ');

        const char* end_pt = end_info.ptr + 1;
        const char* org_end_pt = location.ptr + location.length;

        size_t start_width = 0, end_width = 0;
        if (!lines.empty()) {

            { // Calculate the start width
                const char* ptr = start_info.ptr;

                bool traverse_to_loc = location_start_line_number == primary_line_number;

                while ((!traverse_to_loc && is_whitespace(*ptr)) ||
                       (traverse_to_loc && ptr < location.ptr)) {
                    if (*ptr == '\t') start_width += 4;
                    else              start_width += 1;
                    ++ptr;
                }

                start_width -= pline_leading_ws_cutoff_count;
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
            pline_leading_ws_cutoff_count,
            pline_leading_non_ws_cutoff_count,
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

    print(White);
    if (primary_has_period) {
        print(".");
    }
    print("\n");

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

    print_line_bar(info);

    size_t line_number = info.start_line_number;

    bool has_arrow_msg = !arrow_msg.msg.empty();
    bool is_arrow_after         = has_arrow_msg && arrow_msg.position == ArrowPosition::After;
    bool is_at_arrow_msg        = has_arrow_msg && arrow_msg.position == ArrowPosition::At;
    bool is_alongside_arrow_msg = has_arrow_msg && arrow_msg.position == ArrowPosition::Alongside;

    {
        // Later calculations require this to be ordered linearly by where it appears in
        // the buffer.
        std::ranges::sort(individual_underlines, [](const auto& lhs, const auto& rhs) {
            return lhs.ptr < rhs.ptr;
        });
        // Must make this unique because otherwise it will try and display the underline for the
        // same expression multiple times.
        auto new_end = std::unique(individual_underlines.begin(), individual_underlines.end(),
                                   [](const auto& lhs, const auto& rhs) {
            return lhs.ptr == rhs.ptr;
        });
        individual_underlines.erase(new_end, individual_underlines.end());
    }

    for (const auto& line_info : info.lines) {

        print_error_location_line(info,
                                  line_info,
                                  has_arrow_msg,
                                  is_alongside_arrow_msg,
                                  is_at_arrow_msg,
                                  is_arrow_after,
                                  line_number);
        ++line_number;
    }
}

void acorn::Logger::print_error_location_line(const ErrorInfo& info,
                                              const LineInfo& line_info,
                                              bool has_arrow_msg,
                                              bool is_alongside_arrow_msg,
                                              bool is_at_arrow_msg,
                                              bool is_arrow_after,
                                              const size_t line_number) {

    size_t cutoff = static_cast<size_t>(info.pline_leading_ws_cutoff_count);
    if (line_number != info.primary_line_number) {
        cutoff += static_cast<size_t>(info.pline_leading_non_ws_cutoff_count);
    }

    std::string line = tabs_to_spaces(line_info.unprocessed_text);
    line = line.substr(cutoff, line.length());

    print_line_number_bar(info, line_number);

    // Printing the line.
    //
    if (line_info.is_first && info.exceeded_start)
        fmt_print("%s...%s ", Green, White);
    else if (info.exceeded_start)
        print("    "); // Want to make sure the lines are still aligned as seen in the file.

    if (line.empty()) {
        print("\n");
        return;
    }

    print(line);

    if (line_info.is_last && info.exceeded_end)
        fmt_print(" %s...%s", Green, White);

    bool alongside_arrow_msg_on_line = false, alongside_arrow_msg_after_dots = false;
    if (line_info.is_last && is_alongside_arrow_msg) {
        calc_alongwith_arrow_msg_info_and_print_location(line_info,
                                                         line_number,
                                                         alongside_arrow_msg_on_line,
                                                         alongside_arrow_msg_after_dots);
    }

    print("\n");

    // Print the characters that indicate which part of what is being printed needs to be
    // paid attention to such as underlining the error location.
    //
    size_t total_printed_characters_for_line = 0;
    bool printed_marker = true;
    if (is_arrow_after) {
        print_arrow_after_msg(info, line_info, line, total_printed_characters_for_line);
    } else {
        if (individual_underlines.empty()) {
            print_underline(info, line_info, is_at_arrow_msg, line, total_printed_characters_for_line);
        } else {
            if (!print_individual_underlines(info, line_info, cutoff, total_printed_characters_for_line)) {
                printed_marker = false;
            }
        }
    }
    total_printed_characters_for_line -= line_info.dots_width;

    // Printing out the arrow message.
    //
    if (!is_alongside_arrow_msg) {
        if (has_arrow_msg && line_info.is_last) {
            fmt_print(" %s", arrow_msg.msg);
        }
    } else if (line_info.is_last && alongside_arrow_msg_on_line) {
        print_alongwith_arrow_msg_on_last_line(line_info, total_printed_characters_for_line);
    } else if (alongside_arrow_msg_after_dots) {
        print_alongwith_arrow_msg_after_dots(line_info, total_printed_characters_for_line);
    }

    if (printed_marker) {
        print("\n");
    }

    print(White);

}

void acorn::Logger::print_line_number_bar(const ErrorInfo& info, size_t line_number) {
    auto line_number_pad_width = count_digits(info.last_line_number) - count_digits(line_number);
    auto line_number_pad = std::string(line_number_pad_width, ' ');
    fmt_print(" %s%s%s %s|%s ",
              line_number == info.primary_line_number ? BrightYellow : DarkGray,
              line_number,
              line_number_pad,
              BrightWhite,
              White);
}

void acorn::Logger::print_arrow_after_msg(const ErrorInfo& info,
                                          const LineInfo& line_info,
                                          const std::string& line,
                                          size_t& total_printed_characters_for_line) {
    if (!line_info.is_last) return;

    size_t offset = line.length() + line_info.dots_width;
    // This can happen sometimes when at the end of the buffer or within a new line.
    if (offset == 0) offset = 1;
    offset -= info.end_width;

    print_line_bar(info, false);
    total_printed_characters_for_line += offset + 1; // +1 for the caret ^
    print(std::string(offset, ' '));
    fmt_print("%s%s", BrightRed, '^');

}

void acorn::Logger::print_underline(const ErrorInfo& info,
                                    const LineInfo& line_info,
                                    bool is_at_arrow_msg,
                                    const std::string& line,
                                    size_t& total_printed_characters_for_line) {

    print_line_bar(info, false);

    size_t underscore_offset = line_info.dots_width;
    if (line_info.is_first)
        underscore_offset += info.start_width;
    else underscore_offset += count_leading_whitespace(line);

    total_printed_characters_for_line += underscore_offset;
    print(std::string(underscore_offset, ' '));

    size_t underscore_width = (line.length() + line_info.dots_width) - underscore_offset;
    // This can happen sometimes when at the end of the buffer or within a new line.
    if (underscore_width == 0) underscore_width = 1;
    if (line_info.is_last) underscore_width -= info.end_width;

    total_printed_characters_for_line += underscore_width;
    std::string underscore = std::string(underscore_width, '~');
    if (is_at_arrow_msg && line_info.is_last) {
        underscore.back() = '^';
        total_printed_characters_for_line += 1;
    }

    fmt_print("%s%s", BrightRed, underscore);

}

bool acorn::Logger::print_individual_underlines(const ErrorInfo& info,
                                                const LineInfo& line_info,
                                                size_t primary_line_leading_trim,
                                                size_t& total_printed_characters_for_line) {

    const char* print_ptr = line_info.line_start_ptr;

    // Move the pointer beyond the characters that were trimmed.
    size_t compare_leading_count = 0;
    while (compare_leading_count < primary_line_leading_trim) {
        if (*print_ptr == '\t') compare_leading_count += 4;
        else                    compare_leading_count += 1;
        ++print_ptr;
    }

    bool printed_underline = false;

    // Print underlines for the given locations if they
    // lay on the given line.
    for (const PointSourceLoc& loc : individual_underlines) {

        const char* line_start_ptr = line_info.line_start_ptr;

        // We can use the line length here because the line has not yet been converted
        // to remove the tabs and leading whitespace out yet.
        size_t line_length = line_info.unprocessed_text.length();
        const char* line_end_ptr = line_start_ptr + line_length + 1;

        bool is_within_line = line_start_ptr >= loc.ptr        && loc.end()    > line_start_ptr ||
                              loc.ptr        >= line_start_ptr && line_end_ptr > loc.ptr;

        if (is_within_line) {
            if (!printed_underline) {
                total_printed_characters_for_line += line_info.dots_width;
                print_line_bar(info, false);
                print(std::string(line_info.dots_width, ' '));
                print(BrightRed);
            }

            size_t print_length = 0;
            if (print_ptr < loc.ptr) {
                while (print_ptr < loc.ptr) {
                    print(' ');
                    ++print_ptr;
                    ++total_printed_characters_for_line;
                }

                print_length = loc.length;
                if (line_end_ptr < loc.end()) {
                    // Remove off the extra characters not on this line.
                    print_length -= static_cast<size_t>(loc.end() - line_end_ptr);
                }
            } else {
                // The expression is split between lines
                // so have to traverse over any leading
                // whitespace.
                while (is_whitespace(*print_ptr) && !is_newline(*print_ptr)) {
                    print(' ');
                    ++print_ptr;
                    ++total_printed_characters_for_line;
                }

                print_length = static_cast<size_t>(loc.end() - print_ptr);
                if (line_end_ptr < loc.end()) {
                    // Remove off the extra characters not on this line.
                    print_length -= static_cast<size_t>((loc.end() + 1) - line_end_ptr);
                }
            }

            printed_underline = true;
            print(std::string(print_length, '~'));
            total_printed_characters_for_line += print_length;
            print_ptr += print_length;
        }
    }

    return printed_underline;
}

void acorn::Logger::calc_alongwith_arrow_msg_info_and_print_location(const LineInfo& line_info,
                                                                     size_t line_number,
                                                                     bool& alongside_arrow_msg_on_line,
                                                                     bool& alongside_arrow_msg_after_dots) {
    const char* line_start_ptr = line_info.line_start_ptr;
    // We can use the line length here because the line has not yet been converted
    // to remove the tabs and leading whitespace out yet.
    size_t line_length = line_info.unprocessed_text.length();
    const char* line_end_ptr = line_start_ptr + line_length + 1;

    alongside_arrow_msg_on_line = arrow_msg.location.ptr >= line_start_ptr &&
                                  arrow_msg.location.ptr < line_end_ptr;
    if (!alongside_arrow_msg_on_line) {

        const char* msg_start_ptr = arrow_msg.location.ptr;
        auto [msg_line_number, _] = file.line_table.get_line_and_column_number(msg_start_ptr);

        // Print the characters around where the along with message resides.
        alongside_arrow_msg_after_dots = msg_line_number == line_number;
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

void acorn::Logger::print_alongwith_arrow_msg_on_last_line(const LineInfo& line_info,
                                                           size_t total_printed_characters_for_line) {
    const char* line_start_ptr = line_info.line_start_ptr;
    const char* msg_start_ptr = arrow_msg.location.ptr;

    std::string up_to_msg_str = std::string(line_start_ptr, msg_start_ptr - line_start_ptr);
    up_to_msg_str = tabs_to_spaces(up_to_msg_str);

    size_t remaining_spaces = up_to_msg_str.size() - total_printed_characters_for_line;
    fmt_print("%s%s%s%s", BrightRed, std::string(remaining_spaces, ' '), "^ ",
                arrow_msg.msg);
}

void acorn::Logger::print_alongwith_arrow_msg_after_dots(const LineInfo& line_info,
                                                         size_t total_printed_characters_for_line) {

    auto line_length = line_info.unprocessed_text.length();
    const char* line_start_ptr = line_info.line_start_ptr;
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

void acorn::Logger::print_line_bar(const ErrorInfo& info, bool include_new_line) {
    fmt_print(" %s %s|%s ", info.line_number_pad, BrightWhite, White);
    if (include_new_line) print("\n");
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
                                               arrow_msg.msg.empty() ? ArrowPosition::None : arrow_msg.position,
                                               individual_underlines);

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
    primary_has_period = true;
    individual_underlines.clear();

}
