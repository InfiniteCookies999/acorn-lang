#include "TestFramework.h"

#include <iomanip>
#include <atomic>
#include <mutex>

#include "Util.h"

// Forward declaring.
thread_local TestCase* current_test = nullptr;
thread_local llvm::SmallVector<IError> intercepted_error_codes;
TestCase* single_run_case = nullptr;

static thread_local TestSection*       current_section = nullptr;
static thread_local uint32_t           current_depth   = 0;
static llvm::SmallVector<TestSection*> sections;

static std::atomic<int> num_failed_tests = 0;
static std::atomic<int> num_tests_ran    = 0;

static std::mutex test_print_mutex;

thread_local int thread_id;

static void set_color(acorn::Color color) {
    acorn::set_color(acorn::Stream::StdOut, color);
}

TestCase::TestCase(const char* name, uint32_t depth, const std::function<void()>& cb)
    : name(name), depth(depth), cb(cb) {
}

void TestCase::run() {
    try {
        cb();
    } catch (TestCaseFailedException e) {
        
    }
    std::lock_guard lock(test_print_mutex);
    std::cout << std::string(4 * depth, ' ') << "(Test) '" << name << "'";
    if (!failed()) {
        set_color(acorn::Color::BrightGreen);
        std::cout << std::setw(50 - 4 * depth - strlen(name)) << "passed";
        set_color(acorn::Color::White);
        std::cout << "!";
    } else {
        set_color(acorn::Color::BrightRed);
        std::cout << std::setw(50 - 4 * depth - strlen(name)) << "failed";
        set_color(acorn::Color::White);
        std::cout << "!\n" << std::string(4 * depth + 4, ' ');
        set_color(acorn::Color::BrightRed);
        std::cout << "[!] ";
        set_color(acorn::Color::BrightWhite);
        
        std::string file = std::string(cpp_fail_file);
#ifdef _WIN32
        auto idx = file.find_last_of("\\");
#else
        auto idx = file.find_last_of("/");
#endif
        if (idx != std::string::npos) {
            file = file.substr(idx + 1);
        }
        std::cout << "(" << file << ", " << line_fail_number << ") ";
        set_color(acorn::Color::White);
        failed_info_cb();
    }
    std::cout << "\n" << std::flush;
}

TestSection::TestSection(const char* name, uint32_t depth, bool run_multithreaded)
    : name(name), depth(depth), run_multithreaded(run_multithreaded) {
}

void TestSection::add_sub_section(TestSection* section) {
    sub_sections.push_back(section);
}

void TestSection::add_test_case(const char* name, const std::function<void()>& test_cb) {
    tests.push_back(new TestCase(name, current_depth, test_cb));
}

void TestSection::run() {
    std::cout << std::string(4 * depth, ' ') << "(Testing) '" << name << "'\n";
    for (TestSection* sub_section : sub_sections) {
        sub_section->run();
    }

    if (run_multithreaded) {
        unsigned number_of_supported_threads = std::thread::hardware_concurrency();

        number_of_supported_threads = static_cast<unsigned>(number_of_supported_threads * 0.7);

        unsigned number_of_threads = 0;
        unsigned maximum_threads   = number_of_supported_threads < 1 ? 1 : number_of_supported_threads;
    
        std::atomic<int> test_idx = 0;
        std::vector<std::thread> test_threads;

        static std::atomic<bool> fatal_encountered = false;
        acorn::fatal_interceptor = [&test_threads]() {
            
            auto current_thread_id = std::this_thread::get_id();

            // Ensuring all threads are safely joined before
            // fatal finishes.
            //
            fatal_encountered = true;
            for (std::thread& thr : test_threads) {
                if (thr.joinable() && current_thread_id != thr.get_id()) {
                    thr.join();
                }
            }
        };

        for(int thr_id = 0; thr_id < maximum_threads; thr_id++) {
            test_threads.push_back(std::thread([&test_idx, &tests=this->tests, thr_id] {
                thread_id = thr_id;
            
                while (true) {
                    if (fatal_encountered) {
                        break;
                    }

                    auto idx = test_idx.fetch_add(1);
                    if (idx >= tests.size()) {
                        break;
                    }

                    TestCase* test = tests[idx];
                    current_test = test;
                    try {
                        test->run();
                    } catch (acorn::FatalException e) {
                    }
                    ++num_tests_ran;
                    if (test->failed()) {
                        ++num_failed_tests;
                    }
                }
            }));
        }

        for (std::thread& thread : test_threads) {
            if (!fatal_encountered) {
                thread.join();
            }
        }
    } else {
        for (TestCase* test : tests) {
            current_test = test;
            test->run();
            ++num_tests_ran;
            if (test->failed()) {
                ++num_failed_tests;
            }
        }
    }
}

void section(const char* name, const std::function<void()>& cb, bool run_multithreaded) {
    TestSection* prev_section = current_section;
    TestSection* new_section = new TestSection(name, current_depth, run_multithreaded);
    if (prev_section) {
        prev_section->add_sub_section(new_section);
    }
    current_section = new_section;
    ++current_depth;
    cb();
    if (!prev_section) {
        sections.push_back(current_section);
    }
    --current_depth;
    current_section = prev_section;
}

void test(const char* name, const std::function<void()>& cb, bool only_run_this) {
    current_section->add_test_case(name, cb);
    if (only_run_this) {
        single_run_case = new TestCase(name, 0, cb);
    }
}

static void error_interceptor(acorn::ErrCode error_code, std::string file, int line_number) {
    intercepted_error_codes.push_back({ error_code, std::move(file), line_number });
}

acorn::Compiler* mock_compiler_instance(acorn::PageAllocator& allocator) {
    // DO NOT clear the identifier cache here.
    auto compiler_instance = new acorn::Compiler(allocator);
    compiler_instance->set_error_code_interceptor(error_interceptor);
    return compiler_instance;
}

acorn::Logger& mock_logger(acorn::Logger& logger) {
    logger.set_error_code_interceptor(error_interceptor);
    return logger;
}

void run_tests() {
    if (single_run_case) {
        current_test = single_run_case;
        single_run_case->run();
        return;
    }
    
    for (TestSection* section : sections) {
        section->run();
    }
    if (num_failed_tests == 0) {
        set_color(acorn::Color::BrightGreen);
        std::cout << "\n\n>>  ";
        set_color(acorn::Color::White);
        std::cout << "Passed all tests!\n\n";
    } else {
        set_color(acorn::Color::BrightRed);
        std::cout << "\n\n>>  ";
        set_color(acorn::Color::White);
        std::cout << "Failed tests!\n\n";
    }
    std::cout << "    (" << (num_tests_ran - num_failed_tests) << "/" << num_tests_ran << ") tests passed.\n";
}
