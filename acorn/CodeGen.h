#ifndef CODEGEN_H
#define CODEGEN_H

namespace llvm {
    class TargetMachine;
    class Module;
    class TargetMachine;
}

namespace acorn {
 
    class Context;

    bool init_llvm_native_target();

    llvm::TargetMachine* create_llvm_target_machine(Context& context, bool use_optimization);

    void set_llvm_module_target(llvm::Module& ll_module, llvm::TargetMachine* ll_target_machine);

    void write_obj_file(Context& context, const char* file_path,
                        llvm::Module& ll_module, llvm::TargetMachine* ll_target_machine);

}

#endif // CODEGEN_H