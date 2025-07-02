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
#include "SystemFiles.h"

#include <iostream>

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
void acorn::AbstractLogger<L>::print(Stream stream, const SystemPath& path) {
    print(stream, path.to_utf8_string());
}

template<typename L>
void acorn::AbstractLogger<L>::print(Stream stream, Type* type) {
    print(stream, type->to_string());
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
    fmt_print("%s: ", BrightWhite), facing_length += 2;
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

    static const char* get_line_number_buffer_ptr(SourceFile& file, size_t line_number) {
        size_t line_offset = file.line_table.get_file_offset(line_number - 1);
        return file.buffer.content + line_offset;
    }

    static size_t count_digits(size_t number) {
        size_t digits = 0;
        do {
            digits++;
            number /= 10;
        } while (number != 0);
        return digits;
    }
}


void acorn::Logger::print_header(ErrCode error_code, const std::string& line_number_pad) {

    fmt_print("%serror", BrightRed), facing_length += 5;
    if (context.should_show_error_codes()) {
        fmt_print("%s[%s%s%s]", White, BrightBlue, static_cast<unsigned>(error_code), White);
        facing_length += 2 + count_digits(static_cast<unsigned>(error_code));
    }
    fmt_print("%s: ", BrightWhite), facing_length += 2;

    const auto& path = file.path;

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

void acorn::Logger::print_line_bar(const ErrorInfo& info, bool include_new_line) {
    fmt_print(" %s %s|%s ", info.line_number_pad, BrightWhite, White);
    if (include_new_line) print("\n");
}

void acorn::Logger::print_line_number_bar(const ErrorInfo& info, const ErrorLine& line) {
    size_t line_number_pad_width = count_digits(info.max_line_number) - count_digits(line.line_number);
    auto line_number_pad = std::string(line_number_pad_width, ' ');
    fmt_print(" %s%s%s %s|%s ",
              line.is_pivot_line ? BrightYellow : DarkGray,
              line.line_number,
              line_number_pad,
              BrightWhite,
              White);
}

void acorn::Logger::print_error_location(const ErrorInfo& info) {

    print_line_bar(info, true);

    auto print_dots = [this, &info]() {
        print_line_bar(info, false);
        fmt_print("%s...%s", Green, White);
        print("\n");
    };

    if (info.has_more_lines_up_cutoff) {
        print_dots();
    }

    size_t last_line_number = -1;
    for (auto& line : info.lines) {

        if (last_line_number != -1 && line.line_number - last_line_number > 1) {
            if (line.line_number - last_line_number == 2) {
                print_line_bar(info, true);
            } else {
                print_dots();
            }
        }

        last_line_number = line.line_number;

        print_line_number_bar(info, line);

        // Printing the leading cutoff dots if there is cutoff.
        if (info.has_left_cutoff) {
            fmt_print("%s...%s", Green, White);
        }

        print(line.characters);

        if (line.has_right_cutoff) {
            fmt_print("%s...%s", Green, White);
        }

        print("\n");
        print_line_bar(info, false);

        // print out the underlines and other information for the line.
        fmt_print("%s", BrightRed);
        if (info.has_left_cutoff) {
            print("   ");
        }

        bool caret_msg_overlaps = false;
        size_t caret_offset = 0;
        for (size_t i = 0; i < line.character_markers.size(); i++) {

            const char* character_offset = line.characters.c_str() + i;
            auto marker = line.character_markers[i];

            bool is_valid_utf8;
            bool is_overlong;
            size_t byte_count = get_utf8_byte_distance(character_offset, is_valid_utf8, is_overlong);

            if (is_valid_utf8 && byte_count > 1) {
                // Traverse to the end of the utf8 character so that we don't overrender the amount
                // of underscores.
                i += byte_count - 1;
            }

            if (marker == ErrorMarker::Underline) {
                size_t count = get_character_column_width(character_offset);
                print(std::string(count, '~'));
            } else if (marker == ErrorMarker::Caret) {
                caret_offset = i;

                if (caret_msg.placement == CaretPlacement::Alongside) {
                    for (size_t j = i;
                         j < line.character_markers.size() &&
                         j < i + 1 + caret_msg.msg.length()
                         ; j++) {
                        if (line.character_markers[j] != ErrorMarker::None) {
                            caret_msg_overlaps = true;
                        }
                    }

                    if (!caret_msg_overlaps) {
                        i = 1 + caret_msg.msg.length();
                    } else {
                        print("^");
                    }

                    continue;
                }

                if (caret_msg.placement == CaretPlacement::After) {
                    size_t count = get_character_column_width(character_offset);
                    print(std::string(count, ' '));
                }
                fmt_print("^ %s", caret_msg.msg);
                break;
            } else {
                size_t count = get_character_column_width(character_offset);
                print(std::string(count, ' '));
            }
        }

        if (caret_msg_overlaps) {
            print("\n");
            print_line_bar(info, false);
            fmt_print("%s%s%s", std::string(caret_offset, ' '), BrightRed, caret_msg.msg);
        }


        fmt_print("%s", BrightWhite);
        print("\n");
    }

    if (info.has_more_lines_down_cutoff) {
        print_dots();
    }

}

void acorn::Logger::end_error(ErrCode error_code) {

    if (should_silence_errors) {
        reset_state();
        return;
    }

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
        error_code_interceptor(error_code, file.path, static_cast<int>(start_line_number));
        mtx.unlock();
        return;
    }

    auto error_info = collect_error_info();

    // Placed after collecting information to reduce the time locked.
    mtx.lock();

    print_header(error_code, error_info.line_number_pad);

    if (encountered_overlong_utf8) {

        fmt_print(" %s %s%s%s ", error_info.line_number_pad, BrightWhite, '|', White);
        print(std::string(facing_length, ' '));
        print("Encountered overlong character in file so not displaying contents.\n\n");


        reset_state();
        mtx.unlock();
        return;
    }

    if (context.should_show_error_location()) {
        print_error_location(error_info);
        print("\n");
    }

    ++num_errors;
    if (context.inc_error_count()) {
        print_exceeded_errors_msg(context);
        mtx.unlock();
        exit(1);
    }
    mtx.unlock();

    reset_state();

}

void acorn::Logger::reset_state() {
    main_location = {};
    caret_msg = CaretMessage{
        .msg = "",
        .placement = CaretPlacement::None,
        .location = {}
    };
    facing_length = 0;
    main_location = {};
    primary_print_cb = {};
    line_printers.clear();
    primary_has_period = true;
    main_location_still_has_priority = false;
    individual_underlines.clear();
    encountered_overlong_utf8 = false;
}

acorn::Logger::ErrorInfo acorn::Logger::collect_error_info() {

    bool debug_print = false;

    Logger::ErrorInfo info;

    // If there are individual underlines then let the first
    // one take over the display. This is unless `main_location_still_has_priority`
    // is set to true.
    if (!individual_underlines.empty() && !main_location_still_has_priority) {
        main_location = individual_underlines[0];
    }

    // Special case: Sometimes the location given is at the end of the
    // file such as when checking for closing multi-line comments. But
    // the code expects the location to be within the bounds of the
    // buffer so that is corrected for here.
    if (*main_location.point == '\0') {
        --main_location.point;
        while (is_whitespace(*main_location.point)) {
            --main_location.point;
        }

        main_location.ptr = main_location.point;
        main_location.length = 1;
    }


    const char* pivot_point_ptr = main_location.point;
    size_t pivot_line_number = file.line_table.get_line_number(pivot_point_ptr);


    std::vector<long long> line_numbers_to_calculate;
    line_numbers_to_calculate.push_back(static_cast<long long>(pivot_line_number) - 1);
    line_numbers_to_calculate.push_back(static_cast<long long>(pivot_line_number) + 0);
    line_numbers_to_calculate.push_back(static_cast<long long>(pivot_line_number) + 1);
    line_numbers_to_calculate.push_back(static_cast<long long>(pivot_line_number) + 2);


    size_t left_pivot_distance = calculate_left_pivot_distance(pivot_line_number, pivot_point_ptr);

    size_t left_cutoff = 0;
    if (left_pivot_distance > MAX_LEADING_LENGTH) {
        left_cutoff = left_pivot_distance - MAX_LEADING_LENGTH;
    }
    if (debug_print) {
        std::cout << "left_pivot_distance: " << left_pivot_distance << "\n";
        std::cout << "initial left_cutoff: " << left_cutoff << "\n";
    }

    // Calculating the initial right cutoffs.
    size_t max_right_cutoff = 0;
    {
        for (auto line_number : line_numbers_to_calculate) {
            auto right_cutoff = calculate_right_cutoff_from_pivot(line_number, left_pivot_distance);
            if (right_cutoff != -1 && right_cutoff > max_right_cutoff) {
                max_right_cutoff = right_cutoff;
            }
        }
    }

    if (debug_print) {
        std::cout << "max_right_marker_cutoff: " << max_right_cutoff << "\n";
    }

    // Calculate the amount of spare characters that my be removed from the left without
    // discarding marked characters.
    size_t cutoff_shift = 0;
    if (max_right_cutoff > 0) {
        size_t min_spare_count = -1;
        for (auto line_number : line_numbers_to_calculate) {
            size_t spare_count = calculate_spare_left_characters(line_number, left_cutoff, left_pivot_distance);
            if (spare_count == -1) continue;

            if (spare_count < min_spare_count) {
                min_spare_count = spare_count;
            }
        }

        if (debug_print) {
            std::cout << "min_spare_count: " << min_spare_count << "\n";
        }


        cutoff_shift = std::min(max_right_cutoff, min_spare_count);

        // Shift the left of the window to the right to include more right characters.
        left_cutoff += cutoff_shift;
        acorn_assert(left_cutoff <= left_pivot_distance, "Cannot cutoff more characters than there are in the window");

        // Make sure to still display at least some characters to the left.
        if (left_pivot_distance - left_cutoff <= 6) {
            size_t cur_left_cutoff = left_cutoff;
            left_cutoff = left_pivot_distance - std::min(left_pivot_distance, size_t(6));
            cutoff_shift -= cur_left_cutoff - left_cutoff;
        }

        if (debug_print) {
            std::cout << "left_cutoff: " << left_cutoff << "\n";
        }
    }

    // Fix codepoints that are being cut in half.
    while (left_cutoff != 0) {
        bool column_cut_in_half = false;
        for (auto line_number : line_numbers_to_calculate) {
            if (does_cut_column_in_half(line_number, left_cutoff)) {
                column_cut_in_half = true;
                break;
            }
        }

        if (!column_cut_in_half) break;

        --left_cutoff;
    }

    for (auto line_number : line_numbers_to_calculate) {
        auto error_line = create_error_line(line_number, left_cutoff, left_pivot_distance, cutoff_shift);
        if (error_line.empty()) {
            continue;
        }
        error_line.is_pivot_line = line_number == pivot_line_number;
        size_t uline_number = line_number;
        if (uline_number > info.max_line_number) {
            info.max_line_number = uline_number;
        }
        if (uline_number < info.min_line_number) {
            info.min_line_number = uline_number;
        }
        info.lines.push_back(error_line);
    }

    {
        auto error_end_ptr = main_location.ptr + main_location.length;
        auto last_line_number = file.line_table.get_line_number(error_end_ptr);
        if (last_line_number > info.max_line_number) {
            info.has_more_lines_down_cutoff = true;
        }
    }

    {
        auto error_start_ptr = main_location.ptr;
        auto first_line_number = file.line_table.get_line_number(error_start_ptr);
        if (first_line_number < info.min_line_number) {
            info.has_more_lines_up_cutoff = true;
        }
    }

    size_t line_number_pad_count = count_digits(info.max_line_number);
    info.line_number_pad = std::string(line_number_pad_count, ' ');
    info.has_left_cutoff  = left_cutoff > 0;

    return info;
}

acorn::Logger::ErrorLine acorn::Logger::create_error_line(long long line_number,
                                                          size_t    left_cutoff,
                                                          size_t    left_pivot_distance,
                                                          size_t    cutoff_shift) {
    if (!is_line_within_error_location(line_number)) {
        return ErrorLine{};
    }

    size_t uline_number = static_cast<size_t>(line_number);

    const char* line_ptr = get_line_number_buffer_ptr(file, uline_number);

    // Skip past the characters outside the window on the left.
    size_t count = 0;
    const char* ptr = line_ptr;
    while (count < left_cutoff) {
        if (is_eol(ptr)) {
            break;
        }

        count += get_character_column_width(ptr);
        skip_bytes_based_on_utf8_distance(ptr);
    }

    if (count < left_cutoff) {
        // There is nothing on the line to render.
        return ErrorLine{};
    }



    const char* err_location_start = main_location.ptr;
    const char* err_location_end   = main_location.ptr + main_location.length;

    auto caret_placement = caret_msg.placement;

    bool is_leading = true;
    bool added_caret = false;

    std::string              line_characters;
    std::vector<ErrorMarker> character_markers;

    bool has_right_cutoff = false;
    while (true) {

        if (count >= left_pivot_distance + cutoff_shift + MAX_TRAILING_LENGTH) {
            // Checking if any non whitespace exists after the right cutoff in order to determine
            // if there is right cutoff.

            while (true) {
                if (is_eol(ptr)) {
                    break;
                }
                if (!is_whitespace(*ptr)) {
                    has_right_cutoff = true;
                    break;
                }
                ++ptr;
            }

            break;
        }

        char ch = *ptr;
        bool within_underline = is_within_underline(ptr);

        if (is_eol(ptr)) {
            break;
        }

        if (!is_whitespace(ch)) {
            is_leading = false;
        }

        bool is_underlined = within_underline && !is_leading;

        if (caret_placement == CaretPlacement::After ||
            caret_placement == CaretPlacement::At) {
            is_underlined = false;
        }

        bool has_caret = false;
        if (caret_placement == CaretPlacement::After ||
            caret_placement == CaretPlacement::At) {
            if (ptr == err_location_end - 1) {
                has_caret = true;
            }
        } else if (caret_placement == CaretPlacement::Alongside) {
            if (ptr == caret_msg.location.ptr) {
                has_caret = true;
            }
        }

        auto add_character = [=, &line_characters, &character_markers]
            (char ch) finline {

            line_characters += ch;
            ErrorMarker marker = is_underlined ? ErrorMarker::Underline : ErrorMarker::None;
            if (has_caret) {
                marker = ErrorMarker::Caret;
            }
            character_markers.push_back(marker);
        };


        if (ch == '\t') {
            for (int i = 0; i < 4; i++) {
                add_character(' ');
            }
            count += 4;
            ++ptr;
        } else {
            count += get_character_column_width(ptr);

            const char* char_ptr = ptr;
            size_t byte_count = skip_bytes_based_on_utf8_distance(ptr);
            for (size_t i = 0; i < byte_count; i++) {
                // Add all utf8 bytes for the given character.
                add_character(*char_ptr);
                ++char_ptr;
            }
        }
    }

    // TODO (maddie): should this go here?
    if ((caret_placement == CaretPlacement::After ||
         caret_placement == CaretPlacement::At) &&
        !added_caret &&
        !character_markers.empty()) {
        auto& last_marker = character_markers.back();
        last_marker = ErrorMarker::Caret;
    }

    return ErrorLine{
        line_characters,
        character_markers,
        uline_number,
        has_right_cutoff
    };
}

size_t acorn::Logger::calculate_left_pivot_distance(size_t pivot_line_number, const char* pivot_point_ptr) {
    if (!is_line_within_error_location(pivot_line_number)) {
        acorn_fatal("pivot line must be within the error location");
        return 0;
    }

    size_t pivot_distance = 0;

    const char* ptr = get_line_number_buffer_ptr(file, pivot_line_number);
    while (true) {
        if (is_eol(ptr)) {
            acorn_fatal("pivot point not in pivot line");
        }

        if (ptr == pivot_point_ptr) {
            break;
        }

        pivot_distance += get_character_column_width(ptr);
        skip_bytes_based_on_utf8_distance(ptr);

        if (ptr > pivot_point_ptr) {
            acorn_fatal("Somehow skipped the pivot point while traversing");
        }
    }

    return pivot_distance;
}

size_t acorn::Logger::calculate_right_cutoff_from_pivot(long long line_number, size_t left_pivot_distance) {
    if (!is_line_within_error_location(line_number)) {
        return 0;
    }

    size_t uline_number = static_cast<size_t>(line_number);
    const char* ptr = get_line_number_buffer_ptr(file, uline_number);

    // Skip characters till after the right of the window.
    size_t count = 0;
    while (true) {
        if (is_eol(ptr)) {
            // Line is too short to cutoff anything to the right.
            return 0;
        }

        count += get_character_column_width(ptr);
        skip_bytes_based_on_utf8_distance(ptr);

        if (count >= left_pivot_distance + MAX_TRAILING_LENGTH) {
            // Hit the pivot point.
            break;
        }
    }

    size_t trailing_count = 0;
    count -= left_pivot_distance + MAX_TRAILING_LENGTH;
    if (count != 0) {
        // We are at a half column cutoff of a character so we want to make
        // sure that it doesnt start dragging the window to the right just
        // because there is a character cut in half.
        trailing_count = count;
    }

    // `count` will now count how many characters
    // past the right of the window we are.
    //
    while (true) {
        if (is_eol(ptr)) {
            count -= trailing_count;
            return count;
        }

        size_t column_width = get_character_column_width(ptr);
        count += column_width;

        // Keep track of a count of the number of characters that don't get
        // so that the function doesn't think it has cutoff characters when
        // those characters are not even important.
        if (!does_character_have_marker(ptr)) {
            trailing_count += column_width;
        } else {
            trailing_count = 0;
        }

        skip_bytes_based_on_utf8_distance(ptr);

    }

    acorn_fatal("unreachable");
    return -1;
}

size_t acorn::Logger::calculate_spare_left_characters(long long line_number, size_t left_cutoff, size_t left_pivot_distance) {
    if (!is_line_within_error_location(line_number)) {
        return -1;
    }

    size_t uline_number = static_cast<size_t>(line_number);
    const char* ptr = get_line_number_buffer_ptr(file, uline_number);

    // Skip past the characters until we get to the left of the window.
    size_t total_count = 0;
    while (true) {
        if (is_eol(ptr)) {
            return total_count;
        }

        if (total_count >= left_cutoff) {
            // Hit the left of the window.
            break;
        }

        total_count += get_character_column_width(ptr);
        skip_bytes_based_on_utf8_distance(ptr);
    }

    bool is_leading_whitespace = true;
    size_t count = total_count - left_cutoff;
    while (true) {
        if (is_eol(ptr)) {
            // Hit the end of the line before hitting the pivot
            // point so we can spare as many characters as needed.
            return MAX_LEADING_LENGTH;
        } else if (total_count >= left_pivot_distance) {
            return count;
        }

        is_leading_whitespace &= is_whitespace(*ptr);

        if (does_character_have_marker(ptr) && !is_leading_whitespace) {
            return count;
        }

        count       += get_character_column_width(ptr);
        total_count += get_character_column_width(ptr);
        skip_bytes_based_on_utf8_distance(ptr);

        if (count >= MAX_LEADING_LENGTH) {
            return MAX_LEADING_LENGTH;
        }
    }

    return 0;
}

bool acorn::Logger::does_cut_column_in_half(long long line_number, size_t left_cutoff) {
    if (!is_line_within_error_location(line_number)) {
        return false;
    }

    size_t uline_number = static_cast<size_t>(line_number);
    const char* ptr = get_line_number_buffer_ptr(file, uline_number);

    // Edge case because moving the pointer forward before there is any
    // cutoff results in it passing the "cutoff".
    if (left_cutoff == 0) {
        return false;
    }

    // Skip past the characters until we get to the left of the window.
    size_t count = 0;
    while (true) {
        if (is_eol(ptr)) {
            // Hit the end of the line without running into the problem.
            return false;
        }

        size_t column_width = get_character_column_width(ptr);
        count += column_width;

        if (count >= left_cutoff) {
            // Check to make sure it is not a tab since tabs can safely be
            // cut into however many columns is needed.
            if (column_width != 4) {
                if (count - left_cutoff != 0) {
                    // Cuts the character in half.
                    return true;
                }
            }

            // Hit the left of the window.
            break;
        }

        skip_bytes_based_on_utf8_distance(ptr);
    }

    return false;
}

bool acorn::Logger::is_line_within_error_location(long long line_number) const {
    if (line_number < 0) {
        return false;
    }

    size_t uline_number = static_cast<size_t>(line_number);

    const char* min_location = main_location.ptr;
    const char* max_location = main_location.end();

    for (auto& individual_underline : individual_underlines) {
        if (individual_underline.ptr < min_location) {
            min_location = individual_underline.ptr;
        } else if (individual_underline.end() > max_location) {
            max_location = individual_underline.end();
        }
    }

    // Making sure the location underline actually resides on the line.
    size_t start_line_number = file.line_table.get_line_number(min_location);
    size_t end_line_number   = file.line_table.get_line_number(max_location);

    if (uline_number < start_line_number || uline_number > end_line_number) {
        return false;
    }

    return true;
}

bool acorn::Logger::is_eol(const char* ptr) const {
    char ch = *ptr;
    return ch == '\n' || ch == '\r' || ch == '\0';
}

bool acorn::Logger::does_character_have_marker(const char* ptr) const {
    const char* err_location_start = main_location.ptr;
    const char* err_location_end   = main_location.ptr + main_location.length;

    if (is_within_underline(ptr)) {
        return true;
    }

    if (caret_msg.placement != CaretPlacement::None) {
        if (caret_msg.placement == CaretPlacement::Alongside) {
            if (ptr == caret_msg.location.ptr) {
                return true;
            }
        } else {
            if (ptr == err_location_end - 1) {
                return true;
            }
        }
    }

    return false;
}

bool acorn::Logger::is_within_underline(const char* ptr) const {

    auto is_not_trailing = [this](const char* ptr) finline {
        while (!is_eol(ptr)) {
            if (!is_whitespace(*ptr)) {
                return true;
            }
            ++ptr;
        }
        return false;
    };

    const char* err_location_start = main_location.ptr;
    const char* err_location_end   = main_location.ptr + main_location.length;

    if (ptr >= err_location_start && ptr < err_location_end) {
        return is_not_trailing(ptr);
    }

    for (auto& individual_underline : individual_underlines) {
        err_location_start = individual_underline.ptr;
        err_location_end   = individual_underline.ptr + individual_underline.length;
        if (ptr >= err_location_start && ptr < err_location_end) {
            return is_not_trailing(ptr);
        }
    }

    return false;
}

size_t acorn::Logger::skip_bytes_based_on_utf8_distance(const char*& ptr) {
    // Quickly handle common case.
    if (static_cast<unsigned char>(*ptr) <= 127) {
        ++ptr;
        return 1;
    }

    bool is_valid_utf8;
    bool is_overlong;
    size_t num_bytes = get_utf8_byte_distance(ptr, is_valid_utf8, is_overlong);

    if (is_overlong) {
        encountered_overlong_utf8 = true;
    }

    if (is_valid_utf8) {
        ptr += num_bytes;
        return num_bytes;
    } else {
        // Invalid utf8 encoding. Just going to treat it like a distance
        // of one.
        ++ptr;
        return 1;
    }
}

/*
 *    Copyright (C) 2007 Markus Kuhn
 *
 * This is an implementation of wcwidth() and wcswidth() (defined in
 * IEEE Std 1002.1-2001) for Unicode.
 *
 * http://www.opengroup.org/onlinepubs/007904975/functions/wcwidth.html
 * http://www.opengroup.org/onlinepubs/007904975/functions/wcswidth.html
 *
 * http://www.unicode.org/unicode/reports/tr11/
 *
 * Markus Kuhn -- 2007-05-26 (Unicode 5.0)
 *
 * Permission to use, copy, modify, and distribute this software
 * for any purpose and without fee is hereby granted. The author
 * disclaims all warranties with regard to this software.
 *
 * Latest version: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 *
 *
 *
 * Modified and simplified for language's use case.
 */

size_t acorn::Logger::get_character_column_width(const char* ptr) {
    unsigned char first_byte = static_cast<unsigned char>(*ptr);
    if (first_byte == '\t') {
        return 4;
    } else if (first_byte <= 127) {
        // C0/C1 control characters still receiving a value of
        // 1 because we do not want the display algorithm to get
        // confused and not show anything.
        return 1;
    }

    // TODO (maddie): deal with zero column modifiers. This is
    // non-trivially because if a modifier is encountered without
    // an associated codepoint to modify then it the algorithm
    // returning zero here will not underline anything leading
    // to display issues.
    //
    // What we need is someway of returning zero here and then
    // having the caller perform look ahead to determine the best
    // option.

    size_t num_bytes;
    bool is_valid_utf8;
    bool is_overlong;
    uint32_t codepoint = get_utf8_codepoint(ptr, num_bytes, is_valid_utf8, is_overlong);

    if (is_overlong) {
        encountered_overlong_utf8 = true;
    }

    if (codepoint >= 0x1100) {
        if ((codepoint <= 0x115F)                                               || // Hangul Jamo init. consonants
            (codepoint == 0x2329 || codepoint == 0x232A)                        || // left/right pointing angle brackets
            (codepoint >= 0x2E80 && codepoint <= 0xA4CF && codepoint != 0x303F) || // CJK ... Yi
            (codepoint >= 0xAC00 && codepoint <= 0xD7A3)                        || // Hangul Syllables
            (codepoint >= 0xF900 && codepoint <= 0xFAFF)                        || // CJK Compatibility Ideographs
            (codepoint >= 0xFE10 && codepoint <= 0xFE19)                        || // Vertical forms
            (codepoint >= 0xFE30 && codepoint <= 0xFE6F)                        || // CJK Compatibility Forms + Small Form Variants
            (codepoint >= 0xFF00 && codepoint <= 0xFF60)                        || // Fullwidth Forms
            (codepoint >= 0xFFE0 && codepoint <= 0xFFE6)                        ||
            (codepoint >= 0x20000 && codepoint <= 0x2FFFD)                      ||
            (codepoint >= 0x30000 && codepoint <= 0x3FFFD)                      ||
            (codepoint >= 0x1F600 && codepoint <= 0x1F64F)                      || // Emoticons
            (codepoint >= 0x1F680 && codepoint <= 0x1F6FC)                      || // Transport and map symbols
            (codepoint >= 0x1F90C && codepoint <= 0x1F9FF)                      || // Supplemental Symbols and Pictographs
            (codepoint >= 0x1F300 && codepoint <= 0x1F5FF)                         // Miscellaneous Symbols and Pictographs
            ) {
            return 2;
        }
    }

    // Default to 1.
    return 1;
}
