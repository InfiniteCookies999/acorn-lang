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
#include "ErrorCodes.h"

namespace acorn {

#define acorn_fatal(msg) Logger::fatal_internal(__FILE__, __LINE__, (msg))
#define acorn_assert(cond, msg) if (!(cond)) { acorn_fatal(msg); }

    class Context;
    class Type;
    struct SourceFile;

    class Logger {
        // We want to use a recursive deadlock because it is possible that
        // on the same thread we are trying to print an error message we encounter
        // a fatal error which needs to print. This would result in a recursive
        // deadlock using a normal mutex.
        using mtx_type = std::recursive_mutex;
    public:
        enum class Stream {
            StdOut,
            StdErr
        };

        enum class ArrowLoc {
            At,
            After
        };

        Logger(Context& context, SourceFile& file);

        Logger(Logger&&) = default;

        static void fatal_internal(const char* cpp_file, int line, const char* msg);

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
        static void global_error(Context& context, const char* msg) {
            global_error(context, [msg] { print(Stream::StdErr, msg); });
        }

        // Displays a error that is not specific to an acorn language file.
        template<typename... TArgs>
        static void global_error(Context& context, const char* fmt, TArgs&&... args) {
            global_error(context, [fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Used to tell the the logger that a new error has been encountered.
        // Must be followed up by end_error() once all information related to
        // the error has been added.
        Logger& begin_error(SourceLoc location, const char* msg) {
            return begin_error(fix_error_location(location), [msg]() { print(Stream::StdErr, msg); });
        }

        // Used to tell the the logger that a new error has been encountered.
        // Must be followed up by end_error() once all information related to
        // the error has been added.
        template<typename... TArgs>
        Logger& begin_error(SourceLoc location, const char* fmt, TArgs&&... args) {
            return begin_error(fix_error_location(location), [fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Used to tell the the logger that a new error has been encountered.
        // Must be followed up by end_error() once all information related to
        // the error has been added.
        Logger& begin_error(PointSourceLoc location, const char* msg) {
            return begin_error(location, [msg]() { print(Stream::StdErr, msg); });
        }

        // Used to tell the the logger that a new error has been encountered.
        // Must be followed up by end_error() once all information related to
        // the error has been added.
        template<typename... TArgs>
        Logger& begin_error(PointSourceLoc location, const char* fmt, TArgs&&... args) {
            return begin_error(location, [fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Adds a message to point to the error to help the user determine what they
        // need to change in their code to fix things.
        Logger& add_arrow_msg(ArrowLoc loc, std::string msg) {
            arrow_msg = { .msg = msg, .loc = loc };
            return *this;
        }

        // Adds an additional line to the error.
        Logger& add_line(const char* msg) {
            return add_line([msg] {
                print(Stream::StdErr, msg);
            });
        }

        // Adds an additional line to the error.
        template<typename... TArgs>
        Logger& add_line(const char* fmt, TArgs&&... args) {
            return add_line([fmt, ...fargs = std::forward<TArgs>(args)]() mutable {
                fmt_print(Stream::StdErr, fmt, std::forward<TArgs>(fargs)...);
            });
        }

        // Removes the period that is automatically added
        // to a line.
        Logger& remove_period() {
            if (!line_printers.empty()) {
                line_printers.back().add_period = false;
            }
            return *this;
        }

        // Adds an empty error line.
        Logger& add_empty_line() {
            add_line("");
            remove_period();
            return *this;
        }

        void print_header(ErrCode error_code);
        void end_error(ErrCode error_code);
        static void print_exceeded_errors_msg(Context& context);

        static void set_color(Stream stream, Color color);

        static PointSourceLoc fix_error_location(SourceLoc location) {
            return PointSourceLoc{
                location.ptr,
                location.length,
                location.ptr,
                location.length
            };
        }

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

        static void print(Stream stream, Color color) {
            set_color(stream, color);
        }

        static void print(Stream stream, Identifier identifier) {
            print(stream, identifier.reduce());
        }

        static void print(Stream stream, llvm::StringRef s) {
            print(stream, s.data(), s.size());
        }

        static void print(Stream stream, Type* type);

        template<std::integral T>
        static void print(Stream stream, T v) {

            // Create buffer and write backwards into it.
            char buffer[22];
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
            char buffer[32];
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

        static void print(Stream stream, std::wstring_view s) {
            print(stream, std::wstring{ s });
        }

        static void print(Stream stream, const std::wstring& s);

        static void print(Stream stream, const std::string& s) {
            print(stream, s.c_str(), s.length());
        }

        static void print(Stream stream, std::string_view s) {
            print(stream, s.data(), s.length());
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

        Logger& begin_error(PointSourceLoc location, const std::function<void()>& print_cb);

        Logger& add_line(const std::function<void()>& print_cb);

        static void global_error(Context& context, const std::function<void()>& print_cb);

        static void debug(const std::function<void()>& print_cb);

        static void info(const std::function<void()>& print_cb);

        size_t get_number_of_errors() const {
            return num_errors;
        }

        void set_error_code_interceptor(const std::function<void(ErrCode)>& interceptor) {
            error_code_interceptor = interceptor;
        }

    private:
        Context&    context;
        SourceFile& file;

        static mtx_type mtx;

        PointSourceLoc main_location;
        size_t         facing_length = 0;
        std::function<void()> primary_print_cb;
        struct ErrorLine {
            bool                  add_period;
            std::function<void()> print_cb;
        };
        llvm::SmallVector<ErrorLine> line_printers;

        struct ArrowMsg {
            std::string msg;
            ArrowLoc    loc;
        } arrow_msg;

        // This is not the total number of accumulated errors. This
        // would be the number of errors currented found within the
        // given file.
        size_t num_errors = 0;

        std::function<void(ErrCode)> error_code_interceptor;

        static void* get_handle(Stream stream);
    
};
}

#endif // LOGGER_H