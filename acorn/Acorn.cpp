#include "Acorn.h"

#include <llvm/IR/Verifier.h>
#include <codecvt>

#include <fstream>
#include <new>
#include <iostream>
#include <ranges>
#include <chrono>

#include "Logger.h"
#include "Parser.h"
#include "Sema.h"
#include "Util.h"
#include "ir/IRGen.h"
#include "CodeGen.h"
#include "link/Linking.h"
#include "Process.h"
#include "SourceFile.h"

namespace fs = std::filesystem;

static const char* StdLibEnvironmentVariable = "acorn_std_lib";

llvm::TargetMachine* acorn::AcornLang::ll_target_machine = nullptr;

static const char* get_std_lib_path() {
    return std::getenv(StdLibEnvironmentVariable);
}

acorn::AcornLang::~AcornLang() {
    delete ll_module;
}

namespace acorn {
    static Context* new_context(llvm::LLVMContext& ll_context, llvm::Module& ll_module, PageAllocator& allocator) {
        Context* context = allocator.alloc_type<Context>();
        new (context) acorn::Context(ll_context, ll_module, allocator);
        return context;
    }
}

acorn::AcornLang::AcornLang(PageAllocator& allocator)
    : allocator(allocator),
    // NOTE: cannot use the allocator because it needs it's memory deleted.
    ll_module(new llvm::Module("AcornModule", ll_context)),
    context(*new_context(ll_context, *ll_module, allocator)) {
    set_output_name(L"program");
}

void acorn::AcornLang::run(SourceVector& sources) {
#define go(f) { f; if (context.has_errors()) return; }

    total_timer.start();

    go(initialize_codegen());

    go(parse_files(sources));

    go(sema_and_irgen());

    go(codegen());

    go(link());

    total_timer.stop();

    show_time_table();

#undef go
}

void acorn::AcornLang::set_output_name(std::wstring output_name) {
#if WIN_OS
    exe_name = output_name.ends_with(L".exe") ? output_name : output_name + L".exe";
#elif UNIX_OS
    exe_name = output_name;
#endif
    obj_name = output_name.ends_with(L".exe") ? output_name.substr(0, output_name.length() - 4) : output_name;
    obj_name += L".o";
    this->output_name = std::move(output_name);
}

void acorn::AcornLang::set_output_directory(std::wstring output_directory) {
    this->output_directory = output_directory;
}

static std::ostream& operator<<(std::ostream& os, acorn::Timer& timer) {
    return os << std::fixed << std::setprecision(3) << timer.took_ms() / 1e9 << " ms. ";
}

struct ShowTime {
    double time;

    ShowTime(const acorn::Timer& timer)
        : time(timer.took_ms()) {
    }

    ShowTime(double time)
        : time(time) {
    }
};

static std::ostream& operator<<(std::ostream& os, const ShowTime& show_time) {
    return os << std::fixed << std::setprecision(3) << show_time.time << " ms. ";
}

static int count_digits(int number) {
    int digits = 0;
    do {
        digits++;
        number /= 10;
    } while (number != 0);
    return digits;
}

void acorn::AcornLang::show_time_table() {
    if (!should_show_times) return;

    std::cout << "\n\n";

    auto get_ms_pad_width = [](const auto&... timers) -> int {
        using namespace std::ranges;
        std::vector<int> v{ static_cast<int>(timers.took_ms())... };
        return max(v | views::transform(count_digits));
    };
    auto get_misc_time = [this](const auto&... timers) -> double {
         return total_timer.took_ms() - (timers.took_ms() + ...);
    };
    auto get_pad1 = [](double time, int pad_width) {
        return std::string(pad_width - count_digits(static_cast<int>(time)), ' ');
    };
    auto get_pad = [get_pad1](const Timer& timer, int pad_width) {
        return get_pad1(timer.took_ms(), pad_width);
    };

    int pad_width = get_ms_pad_width(parse_timer, sema_timer, ir_timer, codegen_timer, link_timer, total_timer);
    double misc_time = get_misc_time(parse_timer, sema_timer, ir_timer, codegen_timer, link_timer);

    std::cout << "Parsing Time   " << get_pad(parse_timer, pad_width)
                                   << ShowTime(parse_timer) << "\n";
    std::cout << "Sema    Time   " << get_pad(sema_timer, pad_width)
                                   << ShowTime(sema_timer) << "\n";
    std::cout << "IRgen   Time   " << get_pad(ir_timer, pad_width)
                                   << ShowTime(ir_timer) << "\n";
    std::cout << "Codegen Time   " << get_pad(codegen_timer, pad_width)
                                   << ShowTime(codegen_timer) << "\n";
    std::cout << "Link    Time   " << get_pad(link_timer, pad_width)
                                   << ShowTime(link_timer) << "\n";
    std::cout << "Misc    Time   " << get_pad1(misc_time, pad_width)
                                   << ShowTime(misc_time) << "\n";
    std::cout << std::string(15 + pad_width + 8, '-') << "\n";
    std::cout << "total   Time   " << get_pad(total_timer, pad_width)
                                   << ShowTime(total_timer) << "\n";

}

void acorn::AcornLang::initialize_codegen() {
    codegen_timer.start();
    
    std::error_code ec;
    auto output_directory_path = output_directory.empty() ? fs::current_path()
                                                          : fs::path(output_directory);
    
    if (ec) {
        Logger::global_error(context, "Failed to find the current path. Error %s", ec.message())
            .end_error(ErrCode::GlobalFailedToFindCurrentPath);
        return;
    }

    std::filesystem::create_directories(output_directory_path, ec);
    if (ec) {
        Logger::global_error(context, "Failed to create output directory: '%s'. Error: %s",
                             output_directory, ec.message())
            .end_error(ErrCode::GlobalFailedToCreateOutputDirectory);
        return;
    }

    auto absolute_output_directory =
        fs::absolute(output_directory_path).generic_wstring();

    absolute_obj_path = absolute_output_directory + L"/" + obj_name;
    absolute_exe_path = absolute_output_directory + L"/" + exe_name;

    if (!init_llvm_native_target()) {
        Logger::global_error(context, "Failed to initialize LLVM native target")
            .end_error(ErrCode::GlobalFailedToInitializeLLVMNativeTarget);
        return;
    }

    if (!ll_target_machine) {
        ll_target_machine = create_llvm_target_machine(context, release_build);
    }
    if (!ll_target_machine) {
        return;
    }

    set_llvm_module_target(*ll_module, ll_target_machine);
    
    codegen_timer.stop();
}

void acorn::AcornLang::sema_and_irgen() {

    for (auto& entry : context.get_modules()) {
        Sema::check_nodes_wrong_scopes(*entry.second);
    }
    if (context.has_errors()) {
        return;
    }

    for (auto& entry : context.get_modules()) {
        Sema::resolve_global_comptime(context, *entry.second);
    }
    if (context.has_errors()) {
        return;
    }

    for (auto& entry : context.get_modules()) {
        Sema::resolve_imports(context, *entry.second);
    }

    Sema::find_main_function(context);

    if (!context.get_main_function()) {
        Logger::global_error(context, "Could not find 'main' (entry point) function")
            .add_line("Expected declaration of: 'void main()'")
            .end_error(ErrCode::GlobalCouldNotFindEntryPointFunc);
        return;
    }
    context.queue_gen(context.get_main_function());

    for (auto& entry : context.get_modules()) {
        Sema::check_for_duplicate_functions(*entry.second);
        Sema::check_for_duplicate_variables(*entry.second);
    }

    if (context.has_errors()) {
        return;
    }

    auto check_decl = [this](Decl* decl) finline {
        sema_timer.start();
        
        Sema sema(context, decl->file, decl->get_logger());
        if (decl->is(NodeKind::Func)) {    
            sema.check_function(as<Func*>(decl));
        } else if (decl->is(NodeKind::Var)) {
            sema.check_variable(as<Var*>(decl));
        } else {
            acorn_fatal("Unreachable: Missing check case");
        }
        sema_timer.stop();
    };

    while (!context.decl_queue_empty()) {
        Decl* decl = context.decl_queue_next();
        
        // Semantic analysis.
        check_decl(decl);

        // Code generation.
        ir_timer.start();
        if (context.has_errors()) continue;
            
        IRGenerator generator(context);
        if (decl->is(NodeKind::Func)) {
            generator.gen_function(as<Func*>(decl));
        } else if (decl->is(NodeKind::Var)) {
            generator.gen_global_variable(as<Var*>(decl));
        } else {
            acorn_fatal("Unreachable: Missing generation case");
        }
        ir_timer.stop();

    }

    IRGenerator generator(context);
    generator.finish_incomplete_global_variables();

    // Checking any declarations that were not checked.
    for (Decl* decl : context.get_unchecked()) {
        check_decl(decl);
    }

    if (context.has_errors()) {
        return;
    }
    
    llvm::verifyModule(*ll_module, &llvm::errs());
    
    if (should_show_llvm_ir) {
        // Do not use ll_module->dump() because if DLLVM_ENABLE_DUMP flag is turned
        // off then it will cause a linker issue.
        ll_module->print(llvm::outs(), nullptr);
        llvm::outs() << "\n";
    }
}

void acorn::AcornLang::codegen() {
    codegen_timer.start();
    write_obj_file(context, "__acorn_tmp_object.o", *ll_module, ll_target_machine);
    std::error_code ec;
    fs::rename("__acorn_tmp_object.o", absolute_obj_path, ec);
    if (ec) {
        Logger::global_error(context,
                             "Failed to move temporary object file to output directory. Error: %s", ec.message())
            .end_error(ErrCode::GlobalFailedToMoveTempObjFile);
    }
    codegen_timer.stop();
}

void acorn::AcornLang::link() {
    link_timer.start();

#ifdef _WIN32
    std::wstring msvc_bin_path, msvc_lib_path;
    if (!get_msvc_install_paths(context, allocator, true, msvc_bin_path, msvc_lib_path)) {
        if (!context.has_errors()) {
            Logger::global_error(context, "Failed to find msvc paths for linking")
                .end_error(ErrCode::GlobalFailedToFindMSVCPathsForLinking);
        }
        return;
    }
    std::wstring winkit_lib_um_path, winkit_lib_ucrt_path;
    if (!get_windows_kits_install_paths(context, allocator, true, winkit_lib_um_path, winkit_lib_ucrt_path)) {
        return;
    }

    //   /DEBUG      for building debug information
    //   /NOLOGO     for removing useless message
    //   /DLL        builds a dll
    //   /LIBPATH    specifies a path for libraries
    //   /PDB        creates a PDB file
    //   /SUBSYSTEM  tells which subsystem to use
    //   /VERBOSE    prints state info
    //   
    llvm::SmallVector<const wchar_t*, 16> library_paths;
    library_paths.push_back(msvc_lib_path.c_str());
    library_paths.push_back(winkit_lib_um_path.c_str());
    library_paths.push_back(winkit_lib_ucrt_path.c_str());

    llvm::SmallVector<const wchar_t*, 16> libraries;
    
    auto get_lib_paths = [&library_paths]() {
        std::wstring lib_paths;
        for (const wchar_t* lib_path : library_paths) {
            lib_paths += std::format(L"/LIBPATH:\"{}\" ", lib_path);
        }
        return lib_paths;
    };

    std::wstring cmd = std::format(L"\"{}\" /NOLOGO /OUT:{} {} {} {}",
                                   msvc_bin_path + L"\\link.exe",
                                   absolute_exe_path,
                                   absolute_obj_path,
                                   get_lib_paths(),
                                   L"libcmt.lib msvcrt.lib kernel32.lib");

    
#elif UNIX_OS
    
    // TODO: Fix this so it doesn't change assume we are using clang.
    std::wstring cmd = std::format(L"clang {} -o {}",
                                   absolute_obj_path,
                                   absolute_exe_path);

#endif

    int exit_code;
    exe_process(cmd.data(), nullptr, false, exit_code);

    std::error_code ec;
    fs::remove(absolute_obj_path, ec);
    if (ec) {
        Logger::global_error(context, "Failed to delete object file '%s'. Error %s",
                             absolute_obj_path, ec.message())
            .end_error(ErrCode::GlobalFailedToDeleteObjFile);
        return;
    }
    
    if (exit_code == 0 && !dont_show_wrote_to_msg) {
        bool is_wide = std::ranges::any_of(absolute_exe_path, [](wchar_t c) {
            return c > 0x7F;
        });
        if (!is_wide) {
            std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
            std::cout << "Wrote program to: " << wconverter.to_bytes(absolute_exe_path) << "\n";
        } else {
            std::wcout << "Wrote program to: " << absolute_exe_path << "\n";
        }
    }

    link_timer.stop();
}

bool acorn::AcornLang::validate_sources(const SourceVector& sources) {
    
    bool failed_to_find_source = false;
    for (const auto& source : sources) {
        fs::path path = fs::path(source.path);
        std::error_code ec;
        if (!fs::exists(path, ec) || ec) {
            if (ec) {
                Logger::global_error(context,
                    "Could not check if source \"%s\" exists. Please check permissions. Error: '%s'",
                    source.path, ec.message())
                    .end_error(ErrCode::GlobalCouldNotCheckIfSourceExists);
            } else {
                Logger::global_error(context, "Source \"%s\" does not exist", source.path)
                    .end_error(ErrCode::GlobalSourceDoesNotExists);
            }
            failed_to_find_source = true;
        }
    }

    return !failed_to_find_source;
}

void acorn::AcornLang::parse_files(SourceVector& sources) {
    parse_timer.start();

    if (sources.empty()) {
        Logger::global_error(context, "No sources provided")
            .end_error(ErrCode::GlobalNoSourcesProvided);
        return;
    }

    // Trying to find the standard library.
    if (!stand_alone) {
        if (const char* lib_path = get_std_lib_path()) {
            std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
            auto wpath = converter.from_bytes(lib_path);
            sources.push_back(Source{
                              .path     = std::move(wpath),
                              .mod_name = "std"
                              });
        } else {
            Logger::global_error(context, "Missing standard library environment variable")
                .add_line("Make sure you set environment variable '%s'", StdLibEnvironmentVariable)
                .end_error(ErrCode::GlobalNoStdLibrary);
            return;
        }
    }

    if (!validate_sources(sources)) {
        return; // Validation failed, so exit early.
    }

    for (const auto& source : sources) {
        fs::path path = fs::path(source.path);
        auto modl = context.get_or_create_modl(source.mod_name);

        std::error_code ec;
        if (fs::is_directory(path, ec) && !ec) {
            // The user specified a path to a directory (parsing all acorn
            // files under the directory).
            parse_directory(*modl, path);
        } else if (ec) {
            Logger::global_error(context,
                "Failed to check if source \"%s\" is a directory. "
                "Make sure not to modify files while compiling. Error: '%s'",
                source.path, ec.message())
                .end_error(ErrCode::GlobalFailedToCheckSourceIsDir);
        } else {
            if (path.extension() != ".ac") {
                Logger::global_error(context, "Expected source file with extension type "
                                              ".ac for file \"%s\"", source.path)
                    .end_error(ErrCode::GlobalWrongExtensionTypeForFile);
                continue; // Skip this file.
            }
            
            // The user specified a path to a file.
            parse_file(*modl, path, path.root_directory());
        }
    }

    parse_timer.stop();

}

void acorn::AcornLang::parse_directory(Module& modl, const fs::path& dir_path) {
    auto root_dir_path = dir_path.parent_path();
    for (const auto& entry : fs::recursive_directory_iterator(dir_path)) {
        if (entry.is_regular_file()) {
            const auto& path = entry.path().generic_wstring();
            parse_file(modl, path, root_dir_path);
        }
    }
}

acorn::Buffer acorn::AcornLang::read_file_to_buffer(const std::filesystem::path& path) {
    
    Buffer buffer;
    if (!read_entire_file(path, buffer.content, buffer.length, allocator)) {
        Logger::global_error(context, "Failed to read file \"%s\". "
                                      "Make sure not to modifty files while compiling", path)
            .end_error(ErrCode::GlobalFailedToReadSourceFile);
        return { .length = 0 };
    }
    
    return buffer;
}

void acorn::AcornLang::parse_file(Module& modl, 
                                  const fs::path& path, 
                                  const fs::path& root_path) {
    auto buffer = read_file_to_buffer(path);

    size_t root_path_size = root_path.generic_wstring().size();
    if (root_path_size) {
        // If it exists we need to add one to get rid of the '/'.
        ++root_path_size;
    }
    std::wstring wpath = path.generic_wstring().substr(root_path_size);

    SourceFile* file = allocator.alloc_type<SourceFile>();
    new (file) SourceFile(context, std::move(wpath), buffer, modl);
    if (error_code_interceptor) {
        file->logger.set_error_code_interceptor(error_code_interceptor);
    }
    modl.add_source_file(file);

    if (!file->buffer.length) {
        return; // Exit early because the file failed to read or because
                // the file is empty.
    }

    Parser parser(context, modl, file);
    parser.parse();

}
