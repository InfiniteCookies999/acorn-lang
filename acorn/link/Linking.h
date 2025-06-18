#ifndef LINKING_H
#define LINKING_H

#include <string>

#include "../PageAllocator.h"

namespace acorn {

    class Context;

    bool get_windows_kits_install_paths(Context& context, PageAllocator& allocator, bool is_64bit_target,
                                        std::string& winkit_lib_um_path, std::string& winkit_lib_ucrt_path);

    bool get_msvc_install_paths(Context& context, PageAllocator& allocator, bool is_64bit_target,
                                std::string& bin_path, std::string& lib_path);
}

#endif // LINKING_H