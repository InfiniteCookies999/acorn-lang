#include "Compiler.h"

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
#include "ir/IRGen.h"
#include "ir/DebugGen.h"
#include "CodeGen.h"
#include "link/Linking.h"
#include "ProcessExec.h"
#include "SourceFile.h"
#include "FloatParsing.h"

namespace fs = std::filesystem;

static const char* StdLibEnvironmentVariable = "acorn_std_lib";

llvm::TargetMachine* acorn::Compiler::ll_target_machine = nullptr;

acorn::Compiler::~Compiler() {
    delete ll_module;
}

namespace acorn {
    static Context* new_context(llvm::LLVMContext& ll_context, llvm::Module& ll_module, PageAllocator& allocator) {
        Context* context = allocator.alloc_type<Context>();
        new (context) acorn::Context(ll_context, ll_module, allocator);
        return context;
    }
}

acorn::Compiler::Compiler(PageAllocator& allocator)
    : allocator(allocator),
    // NOTE: cannot use the allocator because it needs it's memory deleted.
    ll_module(new llvm::Module("AcornModule", ll_context)),
    context(*new_context(ll_context, *ll_module, allocator)) {
    set_output_name("program");
    // Start tracking time before run is called to include timing for
    // command line processing.
    total_timer.start();
}

bool acorn::Compiler::pre_initialize_target_machine() {
    return initialize_target_machine();
}

int acorn::Compiler::run(SourceVector& sources) {
#define go(f) { f; if (context.has_errors()) return 1; }

    go(initialize_codegen());

    go(parse_files(sources));

    go(sema_and_irgen());

    go(codegen());

    go(link());

    total_timer.stop();

    show_time_table();

    return run_program();

#undef go
}

void acorn::Compiler::set_output_name(std::string output_name) {
#if WIN_OS
    exe_name = output_name.ends_with(".exe") ? output_name : output_name + ".exe";
#elif UNIX_OS
    exe_name = output_name;
#endif
    obj_name = output_name.ends_with(".exe") ? output_name.substr(0, output_name.length() - 4) : output_name;
    obj_name += ".o";
    this->output_name = std::move(output_name);
}

bool acorn::Compiler::initialize_target_machine() {
    if (!init_llvm_native_target()) {
        Logger::global_error(context, "Failed to initialize LLVM native target")
            .end_error(ErrCode::GlobalFailedToInitializeLLVMNativeTarget);
        return false;
    }

    if (!ll_target_machine) {
        ll_target_machine = create_llvm_target_machine(context, release_build);
    }

    return ll_target_machine != nullptr;
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

void acorn::Compiler::show_time_table() {
    if (!should_show_times || context.has_errors()) return;

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

    std::cout << "Parsed " << (total_lines_parsed - whitespace_lines_parsed) << " lines"
              << " (" << "Including whitespace: " << total_lines_parsed << ")\n";
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

int acorn::Compiler::run_program() {
    if (!(should_run_program || should_run_seperate_window)) return 0;

    if (!dont_show_wrote_to_msg && !should_run_seperate_window) {
        std::cout << "\n";
    }

    int exit_code;
    auto process_path = absolute_exe_path.to_utf8_string();
    auto process_dir  = absolute_output_directory.to_utf8_string();
    exe_process(process_path.data(),
                process_dir.data(),
                should_run_seperate_window,
                exit_code);
    return exit_code;
}

void acorn::Compiler::initialize_codegen() {
    codegen_timer.start();

    std::string err;
    auto output_directory_path = !output_directory.has_value() ? get_current_directory_path(err)
                                                               : output_directory.value();

    if (!err.empty()) {
        Logger::global_error(context, "Failed to find the current path. Error %s", err)
            .end_error(ErrCode::GlobalFailedToFindCurrentPath);
        return;
    }

    make_directory(output_directory_path, err, false);
    if (!err.empty()) {
        Logger::global_error(context, "Failed to create output directory: '%s'. Error: %s",
                             output_directory_path, err)
            .end_error(ErrCode::GlobalFailedToCreateOutputDirectory);
        return;
    }

    absolute_output_directory = get_absolute_path(output_directory_path, err);
    if (!err.empty()) {
        Logger::global_error(context, "Failed to get absolute path of output directory: '%s'. Error: %s",
                             output_directory_path, err)
            .end_error(ErrCode::GlobalFailedToCreateOutputDirectory);
        return;
    }

    absolute_obj_path = absolute_output_directory / obj_name;
    absolute_exe_path = absolute_output_directory / exe_name;

    if (!initialize_target_machine()) {
        return;
    }

    set_llvm_module_target(*ll_module, ll_target_machine);

    codegen_timer.stop();
}

void acorn::Compiler::sema_and_irgen() {

    sema_timer.start();
    // Check for nodes being placed in the wrong scope first such as if
    // statements at global scope.
    for (auto& entry : context.get_modules()) {
        Sema::report_nodes_wrong_scopes(*entry.second);
    }

    if (context.has_errors()) {
        return;
    }

    if (!context.should_stand_alone()) {
        find_std_lib_declarations();

        if (context.has_errors()) {
            return;
        }
    }

    // Resolve all imports for all files.
    //
    for (auto& entry : context.get_modules()) {
        for (auto source_file : entry.second->get_source_files()) {
            Sema::resolve_imports(context, source_file);
        }
    }

    // Ensure that the main entry point function exists.
    //
    Sema::find_main_function(context);
    if (!context.get_main_function()) {
        Logger::global_error(context, "Could not find 'main' (entry point) function")
            .add_line("Expected declaration of: 'fn main()'")
            .end_error(ErrCode::GlobalCouldNotFindEntryPointFunc);
        return;
    }
    context.queue_gen(context.get_main_function(), nullptr);

    // Check to make sure that there are no duplicate declarations within
    // any namespaces.
    //
    for (auto& entry : context.get_modules()) {
        for (auto source_file : entry.second->get_source_files()) {
            // This must go before checking if the nspace the file belongs to
            // has been checked because otherwise it will not check private
            // functions that are not common to the namespace.
            Sema::check_for_duplicate_functions(source_file, context);

            auto nspace = source_file->get_namespace();
            if (nspace->have_duplicates_been_checked()) {
                continue;
            }
            Sema::check_for_duplicate_functions(nspace, context);
        }
        Sema::check_all_other_duplicates(*entry.second, context);
    }

    sema_timer.stop();

    if (context.has_errors()) {
        return;
    }

    // Allocate debug emitters for each file if this is being ran within debug mode.
    //
    ir_timer.start();
    if (context.should_emit_debug_info()) {
        // The debug builder requires one builder per file so we actually
        // have to create an emitter per file.
        for (auto& [_, modl] : context.get_modules()) {
            for (auto file : modl->get_source_files()) {
                file->di_emitter = allocator.alloc_type<DebugInfoEmitter>();
                new (file->di_emitter) DebugInfoEmitter(context, file);
            }
        }
    }
    ir_timer.stop();

    auto check_decl = [this](Decl* decl) finline {
        sema_timer.start();

        Sema sema(context, decl->file, decl->get_logger());
        if (decl->is(NodeKind::Func)) {
            sema.check_function(static_cast<Func*>(decl));
        } else if (decl->is(NodeKind::Var)) {
            auto var = static_cast<Var*>(decl);
            if (!var->has_been_checked) {
                sema.check_variable(var);
            }
        } else if (decl->is(NodeKind::Struct)) {
            auto structn = static_cast<Struct*>(decl);
            if (!structn->has_been_checked) {
                sema.check_struct(structn);
            }
        } else if (decl->is(NodeKind::Enum)) {
            auto enumn = static_cast<Enum*>(decl);
            if (!enumn->has_been_checked) {
                sema.check_enum(enumn);
            }
        } else if (decl->is(NodeKind::Interface)) {
            auto interfacen = static_cast<Interface*>(decl);
            if (!interfacen->has_been_checked) {
                sema.check_interface(interfacen);
            }
        } else {
            acorn_fatal("Unreachable: Missing check case");
        }

        sema_timer.stop();
    };

    while (!context.decl_queue_empty()) {
        DeclGen decl_gen      = context.decl_queue_next();
        Node* decl            = decl_gen.decl;
        auto generic_instance = decl_gen.generic_instance;

        if (generic_instance) {
            if (decl->is(NodeKind::Func)) {
                auto func = static_cast<Func*>(decl);
                auto func_generic_instance = static_cast<GenericFuncInstance*>(generic_instance);
                func->bind_generic_instance(func_generic_instance);
            } else {
                acorn_fatal("Unknown node kind to bind generics to");
            }
        }

        // Semantic analysis.
        if (decl->is_not(NodeKind::ImplicitFunc)) {
            check_decl(static_cast<Decl*>(decl));
        }

        // Code generation.
        ir_timer.start();
        if (context.has_errors()) continue;

        IRGenerator generator(context);
        if (decl->is(NodeKind::Func)) {
            generator.gen_function(static_cast<Func*>(decl), static_cast<GenericFuncInstance*>(generic_instance));
        } else if (decl->is(NodeKind::Var)) {
            generator.gen_global_variable(static_cast<Var*>(decl));
        } else if (decl->is(NodeKind::ImplicitFunc)) {
            generator.gen_implicit_function(static_cast<ImplicitFunc*>(decl));
        } else {
            acorn_fatal("Unreachable: Missing generation case");
        }
        ir_timer.stop();

    }

    ir_timer.start();
    IRGenerator generator(context);
    generator.add_return_to_global_init_function();
    generator.gen_global_cleanup_function();
    ir_timer.stop();

    // Checking any declarations that were not checked.
    //
    for (Decl* decl : context.get_unchecked()) {
        check_decl(decl);
    }

    if (context.has_errors()) {
        return;
    }

    // Finalizing the Debug information for each file.
    //
    if (context.should_emit_debug_info()) {
        ir_timer.start();
        auto& ll_module  = context.get_ll_module();
        auto& ll_context = context.get_ll_context();

#ifdef _WIN32
        ll_module.addModuleFlag(llvm::Module::Warning, "CodeView", 1);
#endif
        ll_module.addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
        llvm::NamedMDNode* ll_ident_md = ll_module.getOrInsertNamedMetadata("llvm.ident");
        ll_ident_md->addOperand(llvm::MDNode::get(ll_context, {llvm::MDString::get(ll_context, "acorn compiler")}));

        for (auto& [_, modl] : context.get_modules()) {
            for (auto file : modl->get_source_files()) {
                file->di_emitter->finalize();
            }
        }
        ir_timer.stop();
    }

    // This should be matching the PIC level specified when creating the target machine.
    ll_module->addModuleFlag(llvm::Module::Min, "PIC Level", 2);

    if (should_show_llvm_ir) {
        // Do not use ll_module->dump() because if DLLVM_ENABLE_DUMP flag is turned
        // off then it will cause a linker issue.
        ll_module->print(llvm::outs(), nullptr);
        llvm::outs() << "\n";
    }

    if (llvm::verifyModule(*ll_module, &llvm::errs())) {
        context.inc_error_count();
        return;
    }
}

void acorn::Compiler::codegen() {
    codegen_timer.start();
    auto obj_path = absolute_obj_path.to_utf8_string();
    write_obj_file(context, obj_path.c_str(), *ll_module, ll_target_machine);
    codegen_timer.stop();
}

void acorn::Compiler::link() {
    link_timer.start();

#if WIN_OS
    std::string msvc_bin_path, msvc_lib_path;
    if (!get_msvc_install_paths(context, allocator, true, msvc_bin_path, msvc_lib_path)) {
        if (!context.has_errors()) {
            Logger::global_error(context, "Failed to find msvc paths for linking")
                .end_error(ErrCode::GlobalFailedToFindMSVCPathsForLinking);
        }
        return;
    }
    std::string winkit_lib_um_path, winkit_lib_ucrt_path;
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
    library_paths.push_back(msvc_lib_path);
    library_paths.push_back(winkit_lib_um_path);
    library_paths.push_back(winkit_lib_ucrt_path);

    auto get_lib_paths = [this] {
        std::string lib_paths;
        for (const std::string& lib_path : library_paths) {
            lib_paths += std::format("/LIBPATH:\"{}\" ", lib_path);
        }
        return lib_paths;
    };

    auto get_libs = [this] {
        // ucrt.lib     -- dynamic standard C lib.
        // libucrt.lib  -- static standard C lib
        // libcmt.lib  -- static CRT.
        // msvcrt.lib  -- dynamic CRT.
        std::string libs = "msvcrt.lib ucrt.lib kernel32.lib user32.lib shell32.lib gdi32.lib Advapi32.lib ";
        for (const std::string& lib : libraries) {
            libs += lib.ends_with(".lib") ? lib : lib + ".lib ";
        }
        return libs;
    };

    std::string cmd = std::format("\"{}\" /NOLOGO {} /OUT:{} {} {} {}",
                                  msvc_bin_path + "\\link.exe",
                                  context.should_emit_debug_info() ? "/DEBUG" : "",
                                  absolute_exe_path.to_utf8_string(),
                                  absolute_obj_path.to_utf8_string(),
                                  get_lib_paths(),
                                  get_libs());


#elif UNIX_OS

    auto get_lib_paths = [this] {
        std::string lib_paths;
        for (const std::string& lib_path : library_paths) {
            lib_paths += std::format("-L\"{}\" ", lib_path);
        }
        return lib_paths;
    };

    auto get_libs = [this] {
        std::string libs = "";
        for (const std::string& lib : libraries) {
            libs += std::format("-l{} ", lib);
        }
        return libs;
    };

    // TODO: Fix this so it doesn't change assume we are using clang.
    std::string cmd = std::format("clang {} {} {} {} -o {}",
                                  context.should_emit_debug_info() ? "-g" : "",
                                  get_lib_paths(),
                                  get_libs(),
                                  absolute_obj_path.to_utf8_string(),
                                  absolute_exe_path.to_utf8_string());

#endif

    if (show_linker_command) {
        Logger::info("Executing linker cmd: %s", cmd);
        std::cout << "\n";
    }

    int exit_code;
    if (!exe_process(cmd.data(), nullptr, false, exit_code)) {
        const char* error_msg_line1 = "Failed to link with %s linker";
        const char* error_msg_line2 = "Make sure you have %s linker installed on your machine";

#if WIN_OS
        const char* linker_tried = "msvc";
#elif UNIX_OS
        const char* linker_tried = "clang";
#endif

        Logger::global_error(context, error_msg_line1, linker_tried)
            .add_line(error_msg_line2, linker_tried)
            .end_error(ErrCode::GlobalFailedToFindLinker);
    }

    std::string err;
    remove_file(absolute_obj_path, err);
    if (!err.empty()) {
        Logger::global_error(context, "Failed to delete object file '%s'. Error %s",
                             absolute_obj_path, err)
            .end_error(ErrCode::GlobalFailedToDeleteObjFile);
        return;
    }

    if (exit_code != 0) {
        context.inc_error_count();
    }

    if (exit_code == 0 && !dont_show_wrote_to_msg) {
        std::cout << "Wrote program to: " << absolute_exe_path.to_utf8_string() << "\n";
    }

    link_timer.stop();
}

bool acorn::Compiler::validate_sources(const SourceVector& sources) {

    bool failed_to_find_source = false;
    for (const auto& source : sources) {
        std::string err;
        if (!path_exists(source.path, err)) {
            if (!err.empty()) {
                Logger::global_error(context,
                    "Could not check if source \"%s\" exists. Please check permissions. Error: '%s'",
                    source.path, err)
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

void acorn::Compiler::parse_files(SourceVector& sources) {
    parse_timer.start();

    acorn::initialize_float_parsing(allocator);

    if (sources.empty()) {
        Logger::global_error(context, "No sources provided")
            .end_error(ErrCode::GlobalNoSourcesProvided);
        return;
    }

    // Trying to find the standard library.
    if (!context.should_stand_alone()) {
        auto lib_path = get_std_lib_path();
        if (!lib_path.empty()) {
            sources.push_back(Source{
                                  .path     = SystemPath(std::move(lib_path)),
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
        auto modl = context.get_or_create_modl(source.mod_name);

        auto& path = source.path;

        std::string err;
        if (is_directory(path, err)) {
            // The user specified a path to a directory (parsing all acorn
            // files under the directory).
            parse_directory(*modl, path);
        } else if (!err.empty()) {
            Logger::global_error(context,
                "Failed to check if source \"%s\" is a directory. "
                "Make sure not to modify files while compiling. Error: '%s'",
                path, err)
                .end_error(ErrCode::GlobalFailedToCheckSourceIsDir);
        } else {
            if (path.utf8_extension() != ".ac") {
                Logger::global_error(context, "Expected source file with extension type "
                                              ".ac for file \"%s\"", path)
                    .end_error(ErrCode::GlobalWrongExtensionTypeForFile);
                continue; // Skip this file.
            }

            // The user specified a path to a file.
            parse_file(*modl, path, path.parent_directory(err));
        }
    }

    parse_timer.stop();

}

void acorn::Compiler::parse_directory(Module& modl, const SystemPath& dir_path) {
    std::string err;
    auto base_path = dir_path.parent_directory(err);

    recursively_iterate_directory(dir_path, err, [this, &modl, &base_path](SystemPath path, PathKind kind) {
        if (kind == PathKind::REGULAR && path.utf8_extension() == ".ac") {
            parse_file(modl, path, base_path);
        }
    });

    if (!err.empty()) {
        Logger::global_error(context, "Failed to iterate over directory %s. Error: %s",
                             dir_path, err)
            .end_error(ErrCode::GlobalFailedToIterateDirectory);
        return;
    }
}

acorn::Buffer acorn::Compiler::read_file_to_buffer(const SystemPath& path) {

    Buffer buffer;
    if (!read_entire_file(path, buffer.content, buffer.length, allocator)) {
        Logger::global_error(context, "Failed to read file \"%s\". "
                                      "Make sure not to modifty files while compiling", path)
            .end_error(ErrCode::GlobalFailedToReadSourceFile);
        return { .length = 0 };
    }

    return buffer;
}

void acorn::Compiler::parse_file(Module& modl, const SystemPath& path, const SystemPath& base_path) {
    auto buffer = read_file_to_buffer(path);

    size_t base_path_length = base_path.to_utf8_string().length();
    if (base_path_length) {
        // If it exists we need to add one to get rid of the '/'.
        ++base_path_length;
    }

    std::string err;
    auto abs_path = get_absolute_path(path, err);
    if (!err.empty()) {
        Logger::global_error(context, "Failed to get absolute path of file: \"%s\". Error: %s", path, err)
            .end_error(ErrCode::GlobalFailedToGetAbsolutePathOfFile);
        return;
    }

    auto full_path  = abs_path.to_utf8_string();
    auto short_path = abs_path.to_utf8_string().substr(base_path_length);

    SourceFile* file = allocator.alloc_type<SourceFile>();
    new (file) SourceFile(context, std::move(short_path), std::move(full_path), buffer, modl);
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
    total_lines_parsed += static_cast<uint64_t>(parser.get_total_lines_parsed());
    whitespace_lines_parsed += static_cast<uint64_t>(parser.get_whitespace_lines_parsed());

}

void acorn::Compiler::find_std_lib_declarations() {

    auto find_composite_of_kind = [&, this]<typename T>
        (T*, Namespace* nspace, Identifier decl_name) finline -> T* {

        NodeKind decl_kind;
        const char* decl_type_str;
        if constexpr (std::is_same_v<Struct, T>) {
            decl_kind = NodeKind::Struct;
            decl_type_str = "struct";
        } else if constexpr (std::is_same_v<Enum, T>) {
            decl_kind = NodeKind::Enum;
            decl_type_str = "enum";
        } else if constexpr (std::is_same_v<Interface, T>) {
            decl_kind = NodeKind::Interface;
            decl_type_str = "interface";
        } else {
            acorn_fatal("unknown composite type");
        }

        if (Decl* composite = nspace->find_composite(decl_name)) {
            if (composite->is(decl_kind)) {
                return static_cast<T*>(composite);
            } else {
                Logger::global_error(context, "Standard library '%s' struct not a %s",
                                     decl_name, decl_type_str)
                    .end_error(ErrCode::GlobalFailedToFindStdLibDecl);
            }
        } else {
            Logger::global_error(context, "Failed to find standard library '%s' %s",
                                 decl_name, decl_type_str)
                .end_error(ErrCode::GlobalFailedToFindStdLibDecl);
        }
        return nullptr;
    };

    auto& type_table = context.type_table;
    auto modl = context.find_module(Identifier::get("std"));
    if (Struct* structn = find_composite_of_kind((Struct*)0, modl, context.string_struct_identifier)) {
        auto& struct_import = context.std_string_struct_import;

        struct_import = allocator.alloc_type<ImportStmt>();
        new (struct_import) ImportStmt();
        struct_import->key.push_back({ context.string_struct_identifier });
        struct_import->set_imported_composite(structn);
    }

    if (Interface* interfacen = find_composite_of_kind((Interface*)0, modl, context.error_interface_identifier)) {
        context.std_error_interface = interfacen;
        auto& funcs = interfacen->functions;
        for (Func* func : funcs) {
            if (func->name == context.get_name_function_identifier) {
                context.std_error_get_name_func = func;
                break;
            }
        }
    }

    bool found_abort_func = false;
    if (FuncList* abort_funcs = modl->find_functions(Identifier::get("abort"))) {
        auto error_interface_type = context.std_error_interface->interface_type;

        for (Func* func : *abort_funcs) {
            if (func->params.size() != 1) continue;
            Var* param = func->params[0];

            Type* parsed_type = param->parsed_type;
            if (!parsed_type->is_pointer()) continue;

            auto ptr_type = static_cast<PointerType*>(parsed_type);
            Type* elm_type = ptr_type->get_elm_type();

            if (elm_type->get_kind() != TypeKind::UnresolvedComposite) continue;

            auto composite_type = static_cast<UnresolvedCompositeType*>(elm_type);
            if (composite_type->get_composite_name() == context.error_interface_identifier) {
                found_abort_func = true;
                context.std_abort_function = func;
                break;
            }
        }
    }

    if (!found_abort_func) {
        Logger::global_error(context, "Failed to find standard library 'abort' function")
            .end_error(ErrCode::GlobalFailedToFindStdLibDecl);
    }

    if (Namespace* nspace = modl->find_namespace(context.reflect_identifier)) {
        if (Enum* enumn = find_composite_of_kind((Enum*)0, nspace, context.type_id_enum_identifier)) {
            context.std_type_id_enum = enumn;
        }
        if (Struct* structn = find_composite_of_kind((Struct*)0, nspace, context.type_struct_identifier)) {
            context.std_type_struct = structn;
            context.const_std_type_ptr = type_table.get_ptr_type(type_table.get_const_type(structn->struct_type));
        }
        if (Struct* structn = find_composite_of_kind((Struct*)0, nspace, context.struct_type_info_struct_identifier)) {
            context.std_struct_type_info_struct = structn;
        }
        if (Struct* structn = find_composite_of_kind((Struct*)0, nspace, context.field_type_info_struct_identifier)) {
            context.std_field_type_info_struct = structn;
        }
        if (Struct* structn = find_composite_of_kind((Struct*)0, nspace, context.enum_type_info_struct_identifier)) {
            context.std_enum_type_info_struct = structn;
        }
        if (Struct* structn = find_composite_of_kind((Struct*)0, nspace, context.any_struct_identifier)) {
            context.std_any_struct = structn;
            context.std_any_struct_type = structn->struct_type;
        }
    } else {
        Logger::global_error(context, "Failed to find standard library namespace 'reflect'")
            .end_error(ErrCode::GlobalFailedToFindStdLibDecl);
    }
}

std::string acorn::Compiler::get_std_lib_path() const {
    if (std_lib_path.empty()) {
        auto lib_path = std::getenv(StdLibEnvironmentVariable);
        if (!lib_path) {
            return "";
        }
        return lib_path;
    }
    return std_lib_path;
}
