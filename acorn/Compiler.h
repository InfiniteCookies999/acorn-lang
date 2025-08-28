#ifndef ACORN_H
#define ACORN_H

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <filesystem>

#include "Context.h"
#include "Module.h"
#include "Timer.h"
#include "Errors.h"
#include "Util.h"
#include "SystemFiles.h"

namespace llvm {
    class TargetMachine;
    class Module;
}

namespace acorn {

    class DebugInfoEmitter;

    struct Source {
        Path            path;
        llvm::StringRef mod_name;
    };

    class Compiler {
    public:
        using SourceVector = llvm::SmallVector<Source>;

        ~Compiler();

        Compiler(PageAllocator& allocator);

        bool pre_initialize_target_machine();
        int run(SourceVector& sources);

        void set_released_build()                   { release_build = true;                       }
        void set_should_show_times()                { should_show_times = true;                   }
        void set_should_show_llvm_ir()              { should_show_llvm_ir = true;                 }
        void set_should_show_error_codes()          { context.set_should_show_error_codes();      }
        void set_dont_show_wrote_to_msg()           { dont_show_wrote_to_msg = true;              }
        void set_stand_alone()                      { context.set_stand_alone();                  }
        void set_run_program()                      { should_run_program = true;                  }
        void set_run_program_seperate_window()      { should_run_seperate_window = true;          }
        void set_show_linker_command()              { show_linker_command = true;                 }
        void set_max_error_count(int max_errors)    { context.set_max_error_count(max_errors);    }
        void set_max_call_err_funcs(int max_errors) { context.set_max_call_err_funcs(max_errors); }
        void set_should_emit_debug_info()           { context.set_should_emit_debug_info();       }
        void set_dont_show_colors()                 { disable_terminal_colors = true;             }
        void set_dont_show_error_location()         { context.set_dont_show_error_location();     }
        void set_dont_show_spell_checking()         { context.set_dont_show_spell_checking();     }

        void set_output_name(std::string output_name);

        void set_output_directory(Path output_directory) {
            this->output_directory = std::move(output_directory);
        }

        void add_library_path(std::string library_path) {
            library_paths.push_back(std::move(library_path));
        }

        void add_library(std::string library) {
            libraries.push_back(std::move(library));
        }

        void set_standard_library_path(std::string std_lib_path) {
            this->std_lib_path = std_lib_path;
        }

        Context* get_context() const { return &context; }

        bool has_errors() const { return context.has_errors(); }

        void set_error_code_interceptor(const std::function<void(ErrCode, std::string, int)>& interceptor) {
            error_code_interceptor = interceptor;
        }

    private:
        llvm::LLVMContext ll_context;
        llvm::Module*     ll_module;
        // Set to static to share between tests.
        static llvm::TargetMachine* ll_target_machine;

        std::string output_name = "program";
        std::string exe_name;
        std::string obj_name;

        std::optional<Path> output_directory;
        Path absolute_output_directory;
        Path absolute_exe_path;
        Path absolute_obj_path;
        std::string std_lib_path;

        llvm::SmallVector<std::string> library_paths;
        llvm::SmallVector<std::string> libraries;

        bool release_build              = false;
        bool should_show_times          = false;
        bool should_show_llvm_ir        = false;
        bool dont_show_wrote_to_msg     = false;
        bool should_run_program         = false;
        bool should_run_seperate_window = false;
        bool show_linker_command        = false;
        std::function<void(ErrCode, std::string, int)> error_code_interceptor;

        // Timers to keep track of how different
        // stages take.
        Timer parse_timer;
        Timer sema_timer;
        Timer ir_timer;
        Timer codegen_timer;
        Timer link_timer;
        Timer total_timer;

        PageAllocator& allocator;
        Context&       context;

        uint64_t total_lines_parsed      = 0;
        uint64_t whitespace_lines_parsed = 0;

        // Initializes LLVM code generation.
        void initialize_codegen();
        bool initialize_target_machine();

        // Parses the files/directories specified by the user.
        void parse_sources(SourceVector& sources);

        // Checks to make sure the source files exist.
        bool validate_sources(const SourceVector& sources);

        void parse_directory(Module& modl, const Path& dir_path, const Path& proj_path);

        Buffer read_file_to_buffer(const Path& path);
        // Parses the file into an AST.
        //
        // The `proj_path` is the directory path to parent directory of the user
        // specified source path. This path should be absolute.
        //
        // Examples:
        //
        // 1. If the user adds the source file "foo.ac" which is within the
        //    directory "bar" then it will be the absolute path to directory
        //    "bar".
        //     ex.
        //      - "C:\xyz\bar\foo.ac"   ->  "C:\xyz\bar"
        //      - "/mk/ijk/bar/foo.ac"  ->  "/mk/ijk/bar"
        //
        // 2. If the user adds the source directory "abc" with ac files in
        //    it then the `proj_path` is the parent directory of "abc"
        //    ex.
        //     - "C:\xyz\bar\abc\foo.ac"   ->  "C:\xyz\bar"
        //     - "/mk/ijk/bar/abc/foo.ac"  ->  "/mk/ijk/bar"
        //
        // This allows us to show source file locations relative to the provided
        // source when reporting errors.
        //
        // so in the first example above it would just show the location as "foo.ac"
        // but in the second example it would show it as "abc/foo.ac" when reporting
        // the path during error reporting.
        //
        void parse_file(Module& modl, const Path& path, const Path& proj_path);

        // Runs semantic analysis over parsed files and generates
        // LLVM IR.
        void sema_and_irgen();

        // Runs LLVM code generation.
        void codegen();

        // Runs the linker stage to create an executable.
        void link();

        void show_time_table();

        // Runs the program once compiled.
        int run_program();

        std::string get_std_lib_path() const;

    };
}

#endif // ACORN_H
