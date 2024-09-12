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

void acorn::write_obj_file(Context& context, const char* file_path,
                           llvm::Module& ll_module, llvm::TargetMachine* ll_target_machine) {
    
    std::error_code err_code;
    llvm::raw_fd_ostream stream(file_path, err_code, llvm::sys::fs::OF_None);

    if (err_code) {
        Logger::global_error(context, "Could not open object file to write. Error %s", err_code.message())
            .end_error(ErrCode::GlobalFailedToWriteObjFile);
        return;
    }

    llvm::legacy::PassManager pass;
    if (ll_target_machine->addPassesToEmitFile(pass, stream, nullptr, llvm::CodeGenFileType::ObjectFile)) {
        Logger::global_error(context, "The llvm::TargetMachine can't emit a file of this type")
            .end_error(ErrCode::GlobalFailedToEmitObjFileType);
        return;
    }

    pass.run(ll_module);
    stream.flush();

}

