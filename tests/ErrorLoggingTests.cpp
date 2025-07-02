#include "ErrorLoggingTests.h"

// Include parser first because it has a function named expect.
#include "Parser.h"

#include "Context.h"
#include "PageAllocator.h"
#include "Module.h"
#include "SourceFile.h"

#include "TestFramework.h"

#include "Logger.h"

using namespace acorn;

static llvm::LLVMContext ll_context;
static llvm::Module* ll_test_model = new llvm::Module("Test Module", ll_context);

static PageAllocator allocator(4096);

static Context* context;

class LoggerTester {
public:

    static const char* get_start_of_text(const char* program, const char* text) {
        return std::strstr(program, text);
    }

    static const char* get_end_of_text(const char* program, const char* text) {
        const char* ptr = get_start_of_text(program, text);
        ptr += strlen(text);
        return ptr - 1;
    }

    static SourceLoc create_location(const char* program, const char* sub_text) {
        const char* start_ptr = get_start_of_text(program, sub_text);
        const char* end_ptr   = get_end_of_text(program, sub_text);
        return SourceLoc::from_ptrs(start_ptr, end_ptr + 1);
    }

    static SourceFile* create_mock_file(const char* program) {
        Buffer buffer = {
            .content = const_cast<char*>(program),
            .length  = strlen(program)
        };
        acorn::Identifier::clear_cache();
        Module* mock_modl = new Module();
        SourceFile* mock_file = new SourceFile(*context, "", "", buffer, *mock_modl);
        set_logger_mock_interpreter(mock_file->logger);
        return mock_file;
    }

    void run_test() {

        context = allocator.alloc_type<Context>();
        new (context) Context(ll_context, *ll_test_model, allocator);
        context->set_max_error_count(999999);
        context->set_stand_alone();

        acorn::initialize_float_parsing(allocator);

        section("Error Logging", []() {
            section("Logger::calculate_left_pivot_distance", []() {
                test("Calculates basic pivot distance", []() {
                    const char* program = "        a: int = 4124124;";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "a: int = 4124124");
                    file->logger.begin_error(location, "");

                    size_t pivot_distance = file->logger.calculate_left_pivot_distance(1, location.ptr);
                    expect(pivot_distance, to_string<size_t>).to_be(8);
                });
                test("Calculate pivot distance containg tabs and unicode", []() {
                    const char* program = "🚀\t /**/ 😀\ta: int = 4124124;";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "a: int = 4124124");
                    file->logger.begin_error(location, "");

                    size_t pivot_distance = file->logger.calculate_left_pivot_distance(1, location.ptr);
                    expect(pivot_distance, to_string<size_t>).to_be(18);
                });
                test("Calculate pivot distance of zero", []() {
                    const char* program = "a = 4124124;";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "a = 4124124");
                    file->logger.begin_error(location, "");

                    size_t pivot_distance = file->logger.calculate_left_pivot_distance(1, location.ptr);
                    expect(pivot_distance, to_string<size_t>).to_be(0);
                });
            });
            section("Logger::calculate_right_cutoff_from_pivot", []() {
                test("Line too short to cutoff to the right", []() {
                    const char* program = "a: int = 1412412.3223;";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 1412412.3223");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 6);
                    expect(offset, to_string<size_t>).to_be(0);
                });
                test("Line error too short to cutoff to the right", []() {
                    const char* program = "a: int = 1412412.3223; // this is not part of the cutoff even though it is a lot of characters";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 1412412.3223");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 6);
                    expect(offset, to_string<size_t>).to_be(0);
                });
                test("Calculates the amount of cutoff characters past the right of window", []() {
                    const char* program = "a: int = 412134'u64 + 23521421'u64 + 78012809212'u64 + 563421746'u64 + 25434124'u64; // hello!";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 412134'u64 + 23521421'u64 + 78012809212'u64 + 563421746'u64 + 25434124'u64");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 7);
                    expect(offset, to_string<size_t>).to_be(36);
                });
                test("Calculates the amount of cutoff characters past the right of window with tabs and unicode", []() {
                    const char* program = "a: int = 412134'u64 \t+ 23521421'u64 +🚀\t + 78012809212'u64 \t + 563421746'u64 + 🚀🚀 + 25434124'u64; // hello!";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 412134'u64 \t+ 23521421'u64 +🚀\t + 78012809212'u64 \t + 563421746'u64 + 🚀🚀 + 25434124'u64");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 7);
                    expect(offset, to_string<size_t>).to_be(60);
                });
                test("Calculates zero for the amount of characters past the right of window because right on boundry", []() {
                    const char* program = "a: int = 412134'u64 + 23521421'u64 + 780180'u64; // These are not cutoff! >_<";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 412134'u64 + 23521421'u64 + 780180'u64");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 7);
                    expect(offset, to_string<size_t>).to_be(0);
                });
                test("Calculates one for the amount of characters past the right of window because right after boundry", []() {
                    const char* program = "a: int = 412134'u64 + 23521421'u64 + 7802180'u64; // These are not cutoff! >_<";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 412134'u64 + 23521421'u64 + 7802180'u64");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 7);
                    expect(offset, to_string<size_t>).to_be(1);
                });
                test("Line error too short to cutoff to the right for error starting at beginning of line", []() {
                    const char* program = "a = 1412412.3223; // this is not part of the cutoff even though it is a lot of characters";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "a = 1412412.3223");
                    file->logger.begin_error(location, "");

                    size_t offset = file->logger.calculate_right_cutoff_from_pivot(1, 2);
                    expect(offset, to_string<size_t>).to_be(0);
                });
            });
            section("Logger::calculate_spare_left_characters", []() {
                test("Calculates spare characters to be the pivot distance since there is no left cutoff", []() {
                    const char* program = "a: int = 1412412.3223; // this is not part of the cutoff even though it is a lot of characters";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 1412412.3223");
                    file->logger.begin_error(location, "");

                    size_t spare_count = file->logger.calculate_spare_left_characters(1, 0, 7);

                    expect(spare_count, to_string<size_t>).to_be(7);

                });
                test("Calculates spare characters to be the distance between the cutoff and the pivot distance", []() {
                    const char* program = "a: int = 1412412.3223; // this is not part of the cutoff even though it is a lot of characters";

                    auto file = create_mock_file(program);

                    auto location = create_location(program, "= 1412412.3223");
                    file->logger.begin_error(location, "");

                    size_t spare_count = file->logger.calculate_spare_left_characters(1, 4, 7);

                    expect(spare_count, to_string<size_t>).to_be(3);

                });
            });
        });
    }
};

void test_error_logging() {
    LoggerTester tester;
    tester.run_test();
}