#ifndef TEST_FRAMEWORK_H
#define TEST_FRAMEWORK_H

#include <string>
#include <functional>
#include <iostream>
#include <llvm/ADT/SmallVector.h>
#include <concepts>
#include <sstream>

#include "Acorn.h"

class TestCase;

extern TestCase* current_test;
extern llvm::SmallVector<acorn::ErrCode> intercepted_error_codes;

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
    TestSection(const char* name, uint32_t depth);

    void add_sub_section(TestSection* section);

    void add_test_case(const char* name, const std::function<void()>& test_cb);

    void run();

private:
    const char* name;
    uint32_t depth;
    llvm::SmallVector<TestSection*> sub_sections;
    llvm::SmallVector<TestCase*>    tests;
};

void section(const char* name, const std::function<void()>& cb);
void test(const char* name, const std::function<void()>& cb);

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
            fail([error_code]() {
                std::cout << "Expected an error to occure but multiple occured";
                bool is_duplicate = true;
                for (acorn::ErrCode intercepted : intercepted_error_codes) {
                    if (intercepted != error_code) {
                        std::cout << ".  Encountered Error: " << static_cast<unsigned>(intercepted);
                        is_duplicate = false;
                        break;
                    }
                }
                if (is_duplicate) {
                    std::cout << ". Duplicate of: " << static_cast<unsigned>(error_code);
                }
            });
        } else {
            // Still need to clear for other tests.
            intercepted_error_codes.clear();
        }
    }

private:

    void check_for_error_code() {
        if (!intercepted_error_codes.empty()) {
            fail([first = intercepted_error_codes[0]] {
                std::cout << "Encountered error: " << static_cast<unsigned>(first);
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

acorn::AcornLang* mock_acorn_instance(acorn::PageAllocator& allocator);
acorn::Logger&    mock_logger(acorn::Logger& logger);

void run_tests();

#endif // TEST_FRAMEWORK_H
