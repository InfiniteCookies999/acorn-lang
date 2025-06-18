#include "CodeGen.h"

// LLVM Target
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/IR/Module.h>

// Needed for writing .o files
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/LegacyPassManager.h>
#include <codecvt>

#include "Logger.h"
#include "Context.h"
#include "Util.h"

#if WIN_OS
#include <Windows.h>
#include <fcntl.h>     // For _O_WRONLY
#include <io.h>        // For _open_osfhandle
#undef min
#undef max
#endif

bool acorn::init_llvm_native_target() {
    if (llvm::InitializeNativeTarget())           return false;
    if (llvm::InitializeNativeTargetAsmParser())  return false;
    if (llvm::InitializeNativeTargetAsmPrinter()) return false;
    return true;
}

llvm::TargetMachine* acorn::create_llvm_target_machine(Context& context, bool use_optimization) {

    auto ll_target_triple = llvm::sys::getDefaultTargetTriple();

    std::string target_error;
    auto target = llvm::TargetRegistry::lookupTarget(ll_target_triple, target_error);
    if (!target) {
        Logger::global_error(context, "Failed to find LLVM Target. Error: %s", target_error)
            .end_error(ErrCode::GlobalFailedToFindLLVMTarget);
        return nullptr;
    }

    auto cpuv = "generic";
    auto features = "";
    llvm::TargetOptions options;
    auto ll_target_machine = target->createTargetMachine(ll_target_triple,
                                                         cpuv,
                                                         features,
                                                         options,
                                                         llvm::Reloc::PIC_,
                                                         std::nullopt,
                                                         use_optimization ? llvm::CodeGenOptLevel::Default
                                                                          : llvm::CodeGenOptLevel::None);

    return ll_target_machine;
}

void acorn::set_llvm_module_target(llvm::Module& ll_module, llvm::TargetMachine* ll_target_machine) {
    auto target_triple = llvm::sys::getDefaultTargetTriple();
    ll_module.setTargetTriple(target_triple);
    ll_module.setDataLayout(ll_target_machine->createDataLayout());
}

static std::mutex write_mtx;
void acorn::write_obj_file(Context& context, const char* file_path,
                           llvm::Module& ll_module, llvm::TargetMachine* ll_target_machine) {

    auto report_error_could_not_open_object_file = [&context](const char* error_msg) {
        Logger::global_error(context, "Could not open object file to write. Error %s", error_msg)
            .end_error(ErrCode::GlobalFailedToWriteObjFile);
    };

#if WIN_OS
    // Have to do extra work when dealing with wide paths because raw_fd_ostream doesn't directly
    // deal with them.

    std::wstring wfile_path = acorn::utf8_to_wide(file_path);

    HANDLE handle = CreateFileW(wfile_path.c_str(),
                                GENERIC_WRITE,
                                0,
                                nullptr,
                                CREATE_ALWAYS,
                                FILE_ATTRIBUTE_NORMAL,
                                nullptr);

    if (handle == INVALID_HANDLE_VALUE) {
        std::string formated_error = std::format("code: %lu", GetLastError());;
        report_error_could_not_open_object_file(formated_error.c_str());
        return;
    }

    // Creates a C runtime file descriptor for the windows HANDLE.
    int fd = _open_osfhandle(reinterpret_cast<intptr_t>(handle), _O_WRONLY);
    if (fd == -1) {
        Logger::global_error(context, "Failed to convert windows file handle to C runtime file descriptor for writing object file")
            .end_error(ErrCode::GlobalFailedToWriteObjFile);
        return;
    }

    llvm::raw_fd_ostream stream(fd, true);

#else
    std::error_code err_code;
    llvm::raw_fd_ostream stream(file_path, err_code, llvm::sys::fs::OF_None);

    if (err_code) {
        report_error_could_not_open_object_file(err_code.message().c_str());
        return;
    }

#endif

    llvm::legacy::PassManager pass;
    if (ll_target_machine->addPassesToEmitFile(pass, stream, nullptr, llvm::CodeGenFileType::ObjectFile)) {
        Logger::global_error(context, "The llvm::TargetMachine can't emit a file of this type")
            .end_error(ErrCode::GlobalFailedToEmitObjFileType);
        return;
    }

    // LLVM does not seem to have proper multithreading support for writing files.
    std::lock_guard lock(write_mtx);
    pass.run(ll_module);
    stream.flush();

    if (stream.has_error()) {
        Logger::global_error(context, "Stream failed to write object file: %s", stream.error().message())
            .end_error(ErrCode::GlobalFailedToWriteObjFile);
        return;
    }
}
