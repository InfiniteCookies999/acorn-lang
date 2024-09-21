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

namespace llvm {
    class TargetMachine;
    class Module;
}

namespace acorn {
    
    struct Source {
        std::wstring    path;
        llvm::StringRef mod_name;
    };

    class AcornLang {
    public:
        using SourceVector = llvm::SmallVector<Source>;

        ~AcornLang();

        AcornLang(PageAllocator& allocator);

        void run(SourceVector& sources);

        void set_released_build()          { release_build = true;                  }
        void set_should_show_times()       { should_show_times = true;              }
        void set_should_show_llvm_ir()     { should_show_llvm_ir = true;            }
        void set_should_show_error_codes() { context.set_should_show_error_codes(); }
        void set_dont_show_wrote_to_msg()  { dont_show_wrote_to_msg = true;         }
        void set_stand_alone()             { stand_alone = true;                    }

        void set_output_name(std::wstring output_name);
        void set_output_directory(std::wstring output_directory);

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

        std::wstring output_name = L"program";
        std::wstring output_directory;
        std::wstring exe_name;
        std::wstring obj_name;
        std::wstring absolute_exe_path;
        std::wstring absolute_obj_path;

        bool release_build          = false;
        bool should_show_times      = false;
        bool should_show_llvm_ir    = false;
        bool dont_show_wrote_to_msg = false;
        bool stand_alone            = false;
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

        void show_time_table();

        void initialize_codegen();

        void sema_and_irgen();

        void codegen();

        void link();

        bool validate_sources(const SourceVector& sources);

        void parse_files(SourceVector& sources);

        void parse_directory(Module& modl, const std::filesystem::path& path);

        Buffer read_file_to_buffer(const std::filesystem::path& path);
        void parse_file(Module& modl, const std::filesystem::path& path);

    };
}

#endif // ACORN_H