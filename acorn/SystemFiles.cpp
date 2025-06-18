#include "SystemFiles.h"

#if WIN_OS
#include <Windows.h>
#undef min
#undef max
#elif MAC_OS
#include <mach-o/dyld.h>
#endif


acorn::SystemPath::SystemPath()
    : storage(Storage::NONE) {
}

acorn::SystemPath::SystemPath(std::string path)
    : utf8(std::move(path)), storage(Storage::UTF8) {
    normalize_empty();
}

acorn::SystemPath::SystemPath(std::wstring path)
    : wide(std::move(path)), storage(Storage::WIDE) {
    normalize_empty();
}

acorn::SystemPath::SystemPath(const char* path)
    : utf8(path), storage(Storage::UTF8) {
    normalize_empty();
}

acorn::SystemPath::SystemPath(const char* path, size_t length)
    : utf8(path, length), storage(Storage::UTF8) {
    normalize_empty();
}

acorn::SystemPath::SystemPath(const wchar_t* path)
    : wide(path), storage(Storage::WIDE) {
    normalize_empty();
}

acorn::SystemPath::SystemPath(const wchar_t* path, size_t length)
    : wide(path, length), storage(Storage::WIDE) {
    normalize_empty();
}

acorn::SystemPath acorn::SystemPath::parent_directory(std::string& err) const {

    auto abs_path = get_absolute_path(*this, err);
    if (!err.empty()) {
        return SystemPath();
    }

    SystemPath new_path;

    if (abs_path.storage == Storage::UTF8) {
#if _WIN32
        size_t idx = abs_path.utf8.find_last_of("/\\");
#elif
        size_t idx = abs_path.utf8.find_last_of('/');
#endif
        if (idx != std::string::npos) {
            new_path = SystemPath(abs_path.utf8.substr(0, idx));
        } else {
            new_path = abs_path; // The path must be the root path.
        }
    } else {
        // Assume the path is a windows path.
        size_t idx = abs_path.wide.find_last_of(L"/\\");
        if (idx != std::wstring::npos) {
            new_path = SystemPath(abs_path.wide.substr(0, idx));
        } else {
            new_path = abs_path; // The path must be the root path.
        }
    }

    return new_path;
}

std::string acorn::SystemPath::to_utf8_string() const {
    std::string conv = storage == Storage::UTF8 ? utf8
                                                : acorn::wide_to_utf8(wide.c_str(), wide.length());
    // Normalize the path for the display.
#if WIN_OS
    std::ranges::replace(conv, '\\', '/');
#endif
    return conv;
}

std::wstring acorn::SystemPath::to_wide_string() const {
    if (storage == Storage::WIDE) {
        return wide;
    }
    return acorn::utf8_to_wide(utf8.c_str(), utf8.length());
}

std::string acorn::SystemPath::utf8_extension() const {
    if (storage == Storage::UTF8) {
        size_t idx = utf8.find_last_of('.');
        return utf8.substr(idx);
    } else {
        size_t idx = wide.find_last_of('.');
        auto wide_extension = wide.substr(idx);
        return acorn::wide_to_utf8(wide_extension.c_str(), wide_extension.length());
    }
}

acorn::SystemPath acorn::SystemPath::append_path(const std::string& path) {

    if (storage == Storage::UTF8) {
        // Don't append a slash if it already has a slash.
#if _WIN32
        if (utf8.ends_with('/') || utf8.ends_with('\\')) {
            return SystemPath(utf8 + path);
        }
#elif
        if (utf8.ends_with('/')) {
            return SystemPath(utf8 + path);
        }
#endif

        return SystemPath(utf8 + "/" + path);
    } else {
        auto wpath = acorn::utf8_to_wide(path.c_str(), path.length());

        // Don't append a slash if it already has a slash.
        // Assume windows path.
        if (wide.ends_with('/') || wide.ends_with('\\')) {
            return SystemPath(wide + wpath);
        }

        return SystemPath(wide + L"/" + wpath);
    }
}

void acorn::SystemPath::normalize_empty() {
    if (storage == Storage::UTF8 && utf8.empty()) {
        utf8 = "./";
    } else if (wide.empty()) {
        wide = L"./";
    }
}


#if WIN_OS
static std::string win32_error_code_to_string(DWORD ec) {

    wchar_t* buffer = nullptr;
    DWORD length = FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr,
        ec,
        0, // Using default system language. TODO (maddie): is this what we want?
        (LPWSTR)&buffer,
        0,
        nullptr
    );

    if (length == 0) {
        return "Failed to format Win32 Error";
    }

    return acorn::wide_to_utf8(buffer, length);
}
#endif

acorn::SystemPath acorn::get_current_directory_path(std::string& err) {
    auto path = get_executable_path(err);
    if (!err.empty()) {
        return acorn::SystemPath();
    }

    // Remove off the executable name.
    return path.parent_directory(err);
}

acorn::SystemPath acorn::get_executable_path(std::string& err) {
#if WIN_OS
    // TODO (maddie): because windows there is a thing called "long file name"
    // which can extend above MAX_PATH length. Apparently this happens when there
    // is \\?\ prepended to the path.
    wchar_t buffer[MAX_PATH];
    DWORD length = GetModuleFileNameW(nullptr, buffer, MAX_PATH);
    if (length != 0) {
        return SystemPath(buffer, length);
    }
    err = win32_error_code_to_string(GetLastError());
    return SystemPath();
#elif MAC_OS
    char buffer[PATH_MAX + 1];
    uint32_t length = PATH_MAX;
    if (!_NSGetExecutablePath(buf, &length)) {
        return SystemPath(buffer, length);
    }
    acorn_fatal("Not implemented");
    // TODO (maddie): error handling!
    return SystemPath();
#elif UNIX_OS
    char buffer[PATH_MAX + 1];
    ssize_t length = readlink("/proc/self/exe", buffer, PATH_MAX);
    if (length != -1) {
        return SystemPath(buffer, length);
    }
    acorn_fatal("Not implemented");
    // TODO (maddie): error handling!
    return SystemPath();
#endif
}

acorn::SystemPath acorn::get_absolute_path(const SystemPath& path, std::string& err) {
#if WIN_OS
    std::wstring wpath = path.to_wide_string();
    // TODO (maddie): because windows there is a thing called "long file name"
    // which can extend above MAX_PATH length. Apparently this happens when there
    // is \\?\ prepended to the path.
    wchar_t buffer[MAX_PATH];
    DWORD length = GetFullPathNameW(wpath.c_str(), MAX_PATH, buffer, nullptr);
    if (length != 0) {
        return SystemPath(buffer, length);
    }
    err = win32_error_code_to_string(GetLastError());
    return SystemPath();
#elif
    acorn_fatal("Not implemented");
    return SystemPath();
#endif
}

void acorn::make_directory(const SystemPath& path, std::string& err, bool error_if_exists) {
#if WIN_OS

    std::wstring wpath = path.to_wide_string();

    if (!CreateDirectoryW(wpath.c_str(), nullptr)) {
        DWORD ec = GetLastError();
        if (ec == ERROR_ALREADY_EXISTS) {
            if (!error_if_exists) {
                return;
            }
            err = "Already exists";
        } else if (ec == ERROR_PATH_NOT_FOUND) {
            err = "Path not found";
        } else {
            err = win32_error_code_to_string(ec);
        }
    }
#else
    acorn_fatal("Not implemented");
#endif
}

#if WIN_OS
static void win32_get_file_attribs_error(DWORD ec, std::string& err) {
    if (ec == ERROR_FILE_NOT_FOUND || ec == ERROR_PATH_NOT_FOUND) {
        err = "Path not found";
    } else if (ec == ERROR_ACCESS_DENIED) {
        err = "Access denied";
    } else {
        err = win32_error_code_to_string(ec);
    }
}
#endif

void acorn::remove_file(const SystemPath& path, std::string& err) {
#if WIN_OS
    std::wstring wpath = path.to_wide_string();

    DWORD attribs = GetFileAttributesW(wpath.c_str());
    if (attribs == INVALID_FILE_ATTRIBUTES) {
        DWORD ec = GetLastError();
        win32_get_file_attribs_error(ec, err);
        return;
    }

    if ((attribs & FILE_ATTRIBUTE_DIRECTORY) != 0) {
        err = "Path is directory";
        return;
    }

    if (DeleteFileW(wpath.c_str()) == 0) {
        err = win32_error_code_to_string(GetLastError());
        return;
    }
#else
    acorn_fatal("Not implemented");
#endif
}

bool acorn::path_exists(const SystemPath& path, std::string& err) {
#if WIN_OS
    std::wstring wpath = path.to_wide_string();

    DWORD attribs = GetFileAttributesW(wpath.c_str());
    if (attribs == INVALID_FILE_ATTRIBUTES) {
        DWORD ec = GetLastError();
        if (ec == ERROR_FILE_NOT_FOUND || ec == ERROR_PATH_NOT_FOUND) {
            return false;
        }

        win32_get_file_attribs_error(ec, err);
        return false;
    }

    return true;
#else
    acorn_fatal("Not implemented");
    return false;
#endif
}

bool acorn::is_directory(const SystemPath& path, std::string& err) {
#if WIN_OS
    std::wstring wpath = path.to_wide_string();

    DWORD attribs = GetFileAttributesW(wpath.c_str());
    if (attribs == INVALID_FILE_ATTRIBUTES) {
        DWORD ec = GetLastError();
        win32_get_file_attribs_error(ec, err);
        return false;
    }

    return (attribs & FILE_ATTRIBUTE_DIRECTORY) != 0;
#else
    acorn_fatal("Not implemented");
    return false;
#endif
}

void acorn::recursively_iterate_directory(const SystemPath& dir_path,
                                          std::string& err,
                                          std::function<void(SystemPath, PathKind)> callback) {
#if WIN_OS
    if (!is_directory(dir_path, err)) {
        if (!err.empty()) {
            return;
        }
        err = "Not a directory";
        return;
    }

    std::wstring wpath = dir_path.to_wide_string();
    std::wstring glob_path = wpath + L"\\*";

    WIN32_FIND_DATAW find_data;
    HANDLE handle = FindFirstFileW(glob_path.c_str(), &find_data);

    if (handle == INVALID_HANDLE_VALUE) {
        DWORD ec = GetLastError();
        if (ec == ERROR_NO_MORE_FILES) {
            return;
        }
        err = win32_error_code_to_string(ec);
        return;
    }

    do {

        const wchar_t* name = find_data.cFileName;

        // Skip "." and ".."
        if (wcscmp(name, L".") == 0 || wcscmp(name, L"..") == 0) {
            continue;
        }

        bool is_directory = find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;

        auto entry_path = wpath + L"/" + name;
        if (!is_directory) {
            PathKind kind;
            if (find_data.dwFileAttributes & FILE_ATTRIBUTE_NORMAL   ||
                find_data.dwFileAttributes & FILE_ATTRIBUTE_ARCHIVE  ||
                find_data.dwFileAttributes & FILE_ATTRIBUTE_READONLY ||
                find_data.dwFileAttributes& FILE_ATTRIBUTE_HIDDEN) {
                kind = PathKind::REGULAR;
            } else {
                kind = PathKind::OTHER;
            }

            callback(SystemPath(entry_path), kind);
        } else {
            recursively_iterate_directory(SystemPath(entry_path), err, callback);
            if (!err.empty()) {
                return;
            }
        }

    } while (FindNextFileW(handle, &find_data));

    DWORD ec = GetLastError();
    if (ec == ERROR_NO_MORE_FILES) {
        FindClose(handle);
        return;
    }

    err = win32_error_code_to_string(ec);

#endif
}
