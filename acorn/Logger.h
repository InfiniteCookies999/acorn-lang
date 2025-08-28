#ifndef LOGGER_H
#define LOGGER_H

#include <llvm/ADT/SmallVector.h>

#include <string>
#include <concepts>
#include <cstdio>
#include <functional>
#include <mutex>

#include "Source.h"
#include "Identifier.h"
#include "Util.h"
#include "Errors.h"

class LoggerTester; // Forward declare so the tester can access and test the logger functions.

namespace acorn {

    // It is fine for this to not be per thread we intend this to be
    // used to cleanup any active threads when encountering a fatal
    // error.
    extern std::function<void()> fatal_interceptor;

#define acorn_fatal(msg)          Logger::fatal_internal(__FILE__, __LINE__, (msg))
#define acorn_fatal_fmt(fmt, ...) Logger::fatal_internal(__FILE__, __LINE__, (fmt), __VA_ARGS__)
#define acorn_assert(cond, msg) if (!(cond)) { acorn_fatal(msg); }

    class Context;
    class Type;
    class SourceFile;

    class Logger;
    class GlobalLogger;

    class Path;

    class FatalException : std::exception {
    public:
        const char* what() { return "failed"; }
    };

    // The abstract logger uses CRTP which has a compiler limitation
    // that the compilers cannot determine that L derives from
    // Abstract logger requiring explicit static_cast.
    template<typename L>
    class AbstractLogger {
    public:

        AbstractLogger(Context& context, Stream stream)
            : context(context), stream(stream) {}

        L& add_line(const char* msg) {
            return add_line([msg](L&) {
                print(Stream::StdErr, msg);
            });
        }

        L& add_line(std::string msg) {
            return add_line([msg](L&) {
                print(Stream::StdErr, msg);
            });
        }

        // Adds an additional line to the error.
        template<typename... TArgs>
        L& add_line(const char* fmt, TArgs&&... args) {
            return add_line([fmt, ...fargs = std::forward<TArgs>(args)](L&) mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        L& add_line(const std::function<void(L&)>& print_cb) {
            line_printers.push_back({ true, print_cb });
            return static_cast<L&>(*this);
        }

        // Removes the period that is automatically added
        // to a line.
        L& remove_period() {
            if (!line_printers.empty()) {
                line_printers.back().add_period = false;
            } else {
                primary_has_period = false;
            }
            return static_cast<L&>(*this);
        }

        // Adds an empty error line.
        L& add_empty_line() {
            add_line("");
            remove_period();
            return static_cast<L&>(*this);
        }

        template<typename... TArgs>
        void fmt_print(const char* fmt, TArgs&&... args) {
            fmt_print(stream, fmt, std::forward<TArgs>(args)...);
        }

        template<typename T>
        void print(T&& value) {
            print(stream, std::forward<T>(value));
        }

    protected:
        Context& context;
        Stream   stream;

        struct ErrorLine {
            bool                    add_period;
            std::function<void(L&)> print_cb;
        };
        llvm::SmallVector<ErrorLine> line_printers;
        bool primary_has_period = true;

        // Print support functions
        // -----------------------------

        template<typename T, typename... TArgs>
        static void fmt_print(Stream stream, const char* fmt, T&& arg, TArgs&&... args) {
            while (*fmt) {
                if (*fmt == '%' && *(fmt + 1) == 's') {
                    print(stream, std::forward<T>(arg));
                    fmt_print(stream, fmt + 2, std::forward<TArgs>(args)...); // Continue with remaining.
                    return;
                } else {
                    print(stream, *fmt++);
                }
            }
        }

        template<typename T>
        static void fmt_print(Stream stream, const char* fmt, T&& arg) {
            while (*fmt) {
                if (*fmt == '%' && *(fmt + 1) == 's') {
                    print(stream, std::forward<T>(arg));
                    fmt += 2; // Skip '%s'.
                } else {
                    print(stream, *fmt++);
                }
            }
        }

        static void print(Stream stream, const std::string& s) {
            print(stream, s.c_str(), s.length());
        }

        static void print(Stream stream, Color color) {
            set_terminal_color(stream, color);
        }

        static void print(Stream stream, Identifier identifier) {
            print(stream, identifier.to_string());
        }

        static void print(Stream stream, llvm::StringRef s) {
            print(stream, s.data(), s.size());
        }

        static void print(Stream stream, const Path& path);

        static void print(Stream stream, Type* type);

        template<std::integral T>
        static void print(Stream stream, T v) {

            // Create buffer and write backwards into it.
            char buffer[22] = {0};
            char* p = buffer + sizeof(buffer) - 1;
            *p = '\0';

            bool neg = false;
            if constexpr (std::is_signed_v<T>) {
                if (v < 0) {
                    neg = true;
                    v = -v;
                }
            }

            do {
                *(--p) = '0' + (v % 10);
                v /= 10;
            } while (v != 0);

            if (neg) *(--p) = '-';

            print(stream, p);
        }

        template<std::floating_point T>
        static void print(Stream stream, T v) {
            char buffer[32] = {0};
            int length = snprintf(buffer, sizeof(buffer), "%f", v);
            print(stream, buffer, length);
        }

        static void print(Stream stream, void* ptr) {
            char buffer[18];
            int length = snprintf(buffer, sizeof(buffer), "%p", ptr);
            print(stream, buffer);
        }

        static void print(Stream stream, bool v) {
            print(stream, v ? "true" : "false");
        }

        static void print(Stream stream, const char* s) {
            while (*s) {
                print(stream, *s++);
            }
        }

        static void print(Stream stream, char c) {
            print(stream, &c, 1);
        }

        static void print(Stream stream, const char* s, size_t length);

        void print_exceeded_errors_msg(Context& context);

#ifdef _WIN32
        static void* get_handle(Stream stream);
#else
        static int get_handle(Stream stream);
#endif

    };

    class GlobalLogger : public AbstractLogger<GlobalLogger> {
    public:

        GlobalLogger(Context& context, const std::function<void()>& print_cb)
            : AbstractLogger(context, Stream::StdErr), print_cb(print_cb) {}

        void end_error(ErrCode error_code);

    private:
        std::function<void()> print_cb;

    };

    class Logger : public AbstractLogger<Logger> {
    public:
        enum class CaretPlacement {
            At,
            After,
            Alongside,
            None
        };

        enum class ErrorMarker {
            Underline,
            Caret,
            None,
        };

        // Data used for printing errors.
        struct ErrorLine {
            std::string              characters;
            std::vector<ErrorMarker> character_markers;
            size_t line_number;
            bool   has_right_cutoff;
            bool   is_pivot_line;

            bool empty() const {
                return characters.empty();
            }
        };

        struct ErrorInfo {
            std::vector<ErrorLine> lines;
            std::string line_number_pad;
            size_t      min_line_number             = std::numeric_limits<size_t>::max();
            size_t      max_line_number             = 0;
            bool        has_left_cutoff             = false;
            bool        has_more_lines_down_cutoff  = false;
            bool        has_more_lines_up_cutoff    = false;
        };

        Logger(Context& context, SourceFile& file)
            : AbstractLogger(context, Stream::StdErr), file(file) {}

        Logger(Logger&&) = default;

        template<typename... TArgs>
        static void fatal_internal(const char* cpp_file, int line, const char* fmt, TArgs&&... args) {
            fatal_internal(cpp_file, line, [fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }
        static void fatal_internal(const char* cpp_file, int line, const char* msg) {
            fatal_internal(cpp_file, line, [msg]() { print(Stream::StdErr, msg); });
        }
        static void fatal_internal(const char* cpp_file, int line, const std::function<void()>& print_cb);

        // Displays information related to the different steps of compilation
        // to help identify what is happening.
        static void info(const char* msg) {
            info([msg] { print(Stream::StdErr, msg); });
        }

        // Displays information related to the different steps of compilation
        // to help identify what is happening.
        template<typename... TArgs>
        static void info(const char* fmt, TArgs&&... args) {
            info([fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Displays debug information in case things go wrong to help identify
        // errors in the compilation of acorn language files.
        static void debug(const char* msg) {
            debug([msg] { print(Stream::StdErr, msg); });
        }

        // Displays debug information in case things go wrong to help identify
        // errors in the compilation of acorn language files.
        template<typename... TArgs>
        static void debug(const char* fmt, TArgs&&... args) {
            debug([fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Displays a error that is not specific to an acorn language file.
        [[nodiscard]] static GlobalLogger global_error(Context& context, const char* msg) {
            return GlobalLogger(context, [msg] { print(Stream::StdErr, msg); });
        }

        // Displays a error that is not specific to an acorn language file.
        template<typename... TArgs>
        [[nodiscard]] static GlobalLogger global_error(Context& context, const char* fmt, TArgs&&... args) {
            return GlobalLogger(context, [fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Used to tell the the logger that a new error has been encountered.
        // Must be followed up by end_error() once all information related to
        // the error has been added.
        Logger& begin_error(SourceLoc location, const char* msg) {
            return begin_error(location, [msg]() { print(Stream::StdErr, msg); });
        }

        // Used to tell the the logger that a new error has been encountered.
        // Must be followed up by end_error() once all information related to
        // the error has been added.
        template<typename... TArgs>
        Logger& begin_error(SourceLoc location, const char* fmt, TArgs&&... args) {
            return begin_error(location, [fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Adds a message to point to the error to help the user determine what they
        // need to change in their code to fix things.
        Logger& add_arrow_msg(CaretPlacement position, std::string msg) {
            if (position == CaretPlacement::Alongside) {
                acorn_fatal("Must call add_arrow_msg_alongside with ArrowLoc::Alongside");
            }
            caret_msg = { .msg = msg, .placement = position };
            return *this;
        }

        Logger& add_arrow_msg_alongside(std::string msg, SourceLoc location) {
            caret_msg = {
                .msg = msg,
                .placement = CaretPlacement::Alongside,
                .location = location
            };
            return *this;
        }

        Logger& add_individual_underline(SourceLoc underline_loc) {
            individual_underlines.push_back(underline_loc);
            return *this;
        }

        Logger& still_give_main_location_priority() {
            main_location_still_has_priority = true;
            return *this;
        }

        void silence_errors() {
            should_silence_errors = true;
        }

    private:
        friend class ::LoggerTester;

        void print_header(ErrCode error_code, const std::string& line_number_pad);

        void print_line_bar(const ErrorInfo& info, bool include_new_line);
        void print_line_number_bar(const ErrorInfo& info, const ErrorLine& line);

        void print_error_location(const ErrorInfo& info);

        ErrorInfo collect_error_info();

        ErrorLine create_error_line(long long line_number,
                                    size_t    left_cutoff,
                                    size_t    left_pivot_distance,
                                    size_t    cutoff_shift);


        // Calculate the total column width from the start of the pivot line to the pivot point.
        //
        size_t calculate_left_pivot_distance(size_t pivot_line_number, const char* pivot_point_ptr);

        // Given the distance from the start of the pivot line to the pivot point (A point of interest
        // of where in the buffer the error occured) `left_pivot_distance` and a line number calculates
        // how many characters would be cut off past the end of the right side of the error window for the
        // given line.
        //
        //                         /-----------error window-----------\
        //                         |                                  |
        // /* a comment */  a: int = 142.2 + 14124.2412 + 13232.555 + 32523.0
        //                  ~~~~~~~|~~~~~~                            |     |
        //                         |                                  |-----|
        //                         \- pivot point since the                 |- 6 characters cut off
        //                            assignment is the point                    that is what is returned.
        //                            of interest.
        //
        //
        size_t calculate_right_cutoff_from_pivot(long long line_number, size_t left_pivot_distance);

        // Calculates how many characters may be cutoff on the left side window for a given line without removing
        // a character that has a marker (underline or arrow).
        //
        size_t calculate_spare_left_characters(long long line_number, size_t left_cutoff, size_t left_pivot_distance);

        // Checks if the location of the left cutoff results in characters that have
        // 2 column width would be cut in half.
        //
        bool does_cut_column_in_half(long long line_number, size_t left_cutoff);

        // Does the error line fit within the error location.
        //
        bool is_line_within_error_location(long long line_number) const;

        // Is the pointer at the end of the line.
        //
        bool is_eol(const char* ptr) const;

        // Checks if the character found at `ptr` is marked with an underline or
        // arrow.
        //
        bool does_character_have_marker(const char* ptr) const;

        // Checks if the character found at `ptr` is marked with an underline.
        //
        bool is_within_underline(const char* ptr) const;

        // Moves the pointer forward by the number of bytes the character
        // occupies.
        //
        // Returns the number of bytes skipped.
        //
        size_t skip_bytes_based_on_utf8_distance(const char*& ptr);

        // Chooses a column width for utf8 encoded character. There is no standard
        // for choosing therefore a best guess approach is taken in hopes to display
        // the best possible output to the user.
        //
        size_t get_character_column_width(const char* ptr);

    public:
        void end_error(ErrCode error_code);

        void reset_state();

        static void set_color(Stream stream, Color color);

        Logger& begin_error(SourceLoc location, const std::function<void()>& print_cb);

        static void debug(const std::function<void()>& print_cb);

        static void info(const std::function<void()>& print_cb);

        size_t get_number_of_errors() const {
            return num_errors;
        }

        void set_error_code_interceptor(const std::function<void(ErrCode, std::string, int)>& interceptor) {
            error_code_interceptor = interceptor;
        }

    private:

        SourceFile& file;

        SourceLoc             main_location;
        size_t                facing_length = 0;
        std::function<void()> primary_print_cb;

        const size_t MAX_LEADING_LENGTH  = 20;
        const size_t MAX_TRAILING_LENGTH = 40;

        bool should_silence_errors = false;
        bool encountered_overlong_utf8 = false;

        struct CaretMessage {
            std::string    msg;
            CaretPlacement placement = CaretPlacement::None;
            SourceLoc      location;
        } caret_msg;

        bool main_location_still_has_priority = false;
        llvm::SmallVector<SourceLoc> individual_underlines;

        // This is not the total number of accumulated errors. This
        // would be the number of errors currented found within the
        // given file.
        size_t num_errors = 0;

        std::function<void(ErrCode, std::string, int)> error_code_interceptor;

};
}

#endif // LOGGER_H
