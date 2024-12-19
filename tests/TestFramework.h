#ifndef TEST_FRAMEWORK_H
#define TEST_FRAMEWORK_H

#include <string>
#include <functional>
#include <iostream>
#include <llvm/ADT/SmallVector.h>
#include <concepts>
#include <thread>
//#include <process.h>
#include <sstream>

// Process.h

#include "Compiler.h"
#include "Logger.h"

class TestCase;

extern thread_local TestCase* current_test;
extern TestCase* single_run_case;
struct IError {
    acorn::ErrCode code;
    std::string    file;
    int            line_number;
};
extern thread_local llvm::SmallVector<IError> intercepted_error_codes;
extern thread_local int thread_id;

class TestCaseFailedException : std::exception {
public:
    const char* what() { return "failed"; }
};

class TestCase {
public:
    TestCase(const char* name, uint32_t depth, const std::function<void()>& cb);

    void run();

    void set_fail(const std::function<void()>& failed_info_cb, const char* cpp_file, int line_number) {
        has_failed = true;
        this->failed_info_cb = failed_info_cb;
        cpp_fail_file = cpp_file;
        line_fail_number = line_number;
    }

    bool failed() const { return has_failed; }

private:
    const char* name;
    uint32_t depth;
    const std::function<void()> cb;
    bool has_failed = false;
    std::function<void()> failed_info_cb;
    const char* cpp_fail_file;
    int         line_fail_number;
};

class TestSection {
public:
    TestSection(const char* name, uint32_t depth, bool run_multithreaded);

    void add_sub_section(TestSection* section);

    void add_test_case(const char* name, const std::function<void()>& test_cb);

    void run();

private:
    const char* name;
    uint32_t depth;
    llvm::SmallVector<TestSection*> sub_sections;
    llvm::SmallVector<TestCase*>    tests;
    llvm::SmallVector<std::thread>  test_threads;
    bool run_multithreaded = false;
};

void section(const char* name, const std::function<void()>& cb, bool run_multithreaded = false);
void test(const char* name, const std::function<void()>& cb, bool run_only_this = false);

template<typename T>
struct Expector {
    T given;
    std::function<std::string(T)> printer;
    const char* cpp_file;
    int         line;

    void to_be(T expected) {
        check_for_error_code();
        if (given == expected) {
            return;
        }
        Expector<T> copy = *this;
        fail([copy, expected] {
            std::cout << "Expected `" << copy.printer(expected) << "` ";
            std::cout << "but found `" << copy.printer(copy.given) << "`";
        });
    }

    void not_to_be(T expected) {
        check_for_error_code();
        if (given != expected) {
            return;
        }
        Expector<T> copy = *this;
        fail([copy, expected] {
            std::cout << "Expected `" << copy.printer(expected) << "` not to match given";
        });
    }

    void to_produce_error(acorn::ErrCode error_code) {
        if (intercepted_error_codes.empty()) {
            fail([] {
                std::cout << "Expected an error but no error occured";
            });    
            return;
        }
        if (intercepted_error_codes.size() > 1) {
            fail([this, error_code, error_codes = intercepted_error_codes]() {
                std::cout << "Expected a single error to occure but multiple occured";
                bool is_duplicate = true;
                for (auto intercepted : error_codes) {
                    if (intercepted.code != error_code) {
                        print_encountered_error_msg(intercepted);
                        is_duplicate = false;
                        break;
                    }
                }
                if (is_duplicate) {
                    std::cout << ". Duplicate of: " << static_cast<unsigned>(error_code);
                }
            });
        } else if (intercepted_error_codes[0].code != error_code) {
            fail([this, error_code, error_codes = intercepted_error_codes] {
                std::cout << "Expected error: " << static_cast<unsigned>(error_code)
                          << " (" << acorn::error_code_to_string(error_code) << ") ";
                std::cout << "but found: ";
                print_encountered_error_info(error_codes[0]);
            });
        } else {
            // Still need to clear for other tests.
            intercepted_error_codes.clear();
        }
    }

private:

    void print_encountered_error_msg(IError err) {
        std::cout << ".  Encountered Error: ";
        print_encountered_error_info(err);
    }

    void print_encountered_error_info(IError err) {
        std::cout << static_cast<unsigned>(err.code)
                  << " (" << acorn::error_code_to_string(err.code) << ") at "
                  << err.file << ":" << err.line_number;
    }

    void check_for_error_code() {
        if (!intercepted_error_codes.empty()) {
            fail([this, first = intercepted_error_codes[0]] {
                print_encountered_error_msg(first);
            });
        }
    }

    void fail(const std::function<void()>& failed_info_cb) {
        intercepted_error_codes.clear();
        current_test->set_fail(failed_info_cb, cpp_file, line);
        throw TestCaseFailedException();
    }
};

template<typename T>
std::string to_string(const T& value) {
    if constexpr (std::is_pointer_v<T>) {
        std::stringstream ss;
        ss << value;  // Insert the pointer into the stringstream
        return ss.str();  // Convert to string
    } else {
        return std::to_string(value);
    }
}

template<typename T>
Expector<T> expect_impl(T given, auto&& printer, const char* cpp_file, int line) {
    return Expector<T>{given, printer, cpp_file, line};
}

#define expect(given, printer) expect_impl(given, printer, __FILE__, __LINE__)
#define expect_none() expect_impl(0, [](int){ return ""; }, __FILE__, __LINE__)

inline void force_fail_impl(std::string fail_msg, const char* cpp_file, int line) {
    intercepted_error_codes.clear();
    current_test->set_fail([fail_msg] {
        std::cout << fail_msg << "\n";
    }, cpp_file, line);
    throw TestCaseFailedException();
}
#define force_fail(fail_msg) force_fail_impl(fail_msg, __FILE__, __LINE__)

acorn::Compiler* mock_compiler_instance(acorn::PageAllocator& allocator);
acorn::Logger&   mock_logger(acorn::Logger& logger);

void run_tests();

#endif // TEST_FRAMEWORK_H
