#ifndef LINKING_H
#define LINKING_H

#include <string>

#include "../PageAllocator.h"

namespace acorn {

    class Context;

    bool get_windows_kits_install_paths(Context& context, PageAllocator& allocator, bool is_64bit_target,
                                        std::wstring& winkit_lib_um_path, std::wstring& winkit_lib_ucrt_path);

    bool get_msvc_install_paths(Context& context, PageAllocator& allocator, bool is_64bit_target,
                                std::wstring& bin_path, std::wstring& lib_path);
}

#endif // LINKING_H