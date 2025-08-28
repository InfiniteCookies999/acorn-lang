#include "SystemFiles.h"

#if WIN_OS
#include <Windows.h>
#undef min
#undef max
#elif MAC_OS
#include <mach-o/dyld.h>
#endif

#if !WIN_OS
#include <sys/stat.h>
#include <dirent.h>
#endif

#include "Logger.h"

acorn::Path::Path()
    : storage(Storage::NONE) {}

acorn::Path::Path(std::string path)
    : utf8(std::move(path)), storage(Storage::UTF8) {}

acorn::Path::Path(std::wstring path)
    : utf16(std::move(path)), storage(Storage::UTF16) {}

acorn::Path::Path(const char* path)
    : utf8(path), storage(Storage::UTF8) {}

acorn::Path::Path(const char* path, size_t length)
    : utf8(path, length), storage(Storage::UTF8) {}

acorn::Path::Path(const wchar_t* path)
    : utf16(path), storage(Storage::UTF16) {}

acorn::Path::Path(const wchar_t* path, size_t length)
    : utf16(path, length), storage(Storage::UTF16) {}

std::string acorn::Path::to_normalized_utf8_string() const {
    std::string normalized_path = to_utf8_string();

    // convert any backslashes to forward slashes.
    for (auto& ch : normalized_path) {
        if (ch == '\\') {
            ch = '/';
        }
    }

    return normalized_path;
}

std::string acorn::Path::to_utf8_string() const {
    if (storage == Storage::UTF8) {
        return utf8;
    }
    return acorn::utf16_to_utf8(utf16.c_str(), utf16.length());
}

std::wstring acorn::Path::to_utf16_string() const {
    if (storage == Storage::UTF16) {
        return utf16;
    }
    return acorn::utf8_to_utf16(utf8.c_str(), utf8.length());
}

std::string acorn::Path::get_extension() const {
    if (storage == Storage::UTF8) {
        size_t idx = utf8.find_last_of('.');
        if (idx == std::string::npos) {
            return "";
        }
        return utf8.substr(idx);
    } else {
        size_t idx = utf16.find_last_of('.');
        if (idx == std::wstring::npos) {
            return "";
        }

        auto utf16_extension = utf16.substr(idx);
        return acorn::utf16_to_utf8(utf16_extension.c_str(), utf16_extension.length());
    }
}

acorn::Path acorn::Path::append_path(const std::string& path) {

    if (storage == Storage::UTF8) {
        // Don't append a slash if it already has a slash.
#if _WIN32
        if (utf8.ends_with('/') || utf8.ends_with('\\')) {
            return Path(utf8 + path);
        }
#else
        if (utf8.ends_with('/')) {
            return SystemPath(utf8 + path);
        }
#endif

        return Path(utf8 + "/" + path);
    } else {
        auto wpath = acorn::utf8_to_utf16(path.c_str(), path.length());

        // Don't append a slash if it already has a slash.
        // Assume windows path.
        if (utf16.ends_with('/') || utf16.ends_with('\\')) {
            return Path(utf16 + wpath);
        }

        return Path(utf16 + L"/" + wpath);
    }
}




#if WIN_OS
static void win32_error_code_to_string(DWORD ec, std::string& err) {

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
        err = "Failed to format Win32 Error";
    }

    err = acorn::utf16_to_utf8(buffer, length);
}

static void win32_get_file_attribs_error(DWORD ec, std::string& err) {
    if (ec == ERROR_FILE_NOT_FOUND || ec == ERROR_PATH_NOT_FOUND) {
        err = "Path not found";
    } else if (ec == ERROR_ACCESS_DENIED) {
        err = "Access denied";
    } else {
        win32_error_code_to_string(ec, err);
    }
}
#else
static void unix_get_file_stat_error(std::string& err) {
    if (errno == ENOENT || errno == ENOTDIR) {
        err = "Path not found";
    } else if (errno == EACCES) {
        err = "Access denied";
    } else {
        err = strerror(errno);
    }
}
#endif

bool acorn::path_exists(const Path& path, std::string& err) {
#if WIN_OS
    auto utf16_path = path.to_utf16_string();

    DWORD attribs = GetFileAttributesW(utf16_path.c_str());
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
    struct stat path_stat;

    auto utf8_path = path.to_utf8_string();
    if (stat(utf8_path.c_str(), &path_stat) != 0) {
        if (errno == ENOENT || errno == ENOTDIR) {
            return false;
        }
        unix_get_file_stat_error(err);
        return false;
    }

    return true;
#endif
}

bool acorn::is_directory(const Path& path, std::string& err) {
#if WIN_OS
    auto utf16_path = path.to_utf16_string();

    DWORD attribs = GetFileAttributesW(utf16_path.c_str());
    if (attribs == INVALID_FILE_ATTRIBUTES) {
        DWORD ec = GetLastError();
        win32_get_file_attribs_error(ec, err);
        return false;
    }

    return (attribs & FILE_ATTRIBUTE_DIRECTORY) != 0;
#else
    struct stat path_stat;

    auto utf8_path = path.to_utf8_string();
    if (stat(utf8_path.c_str(), &path_stat) != 0) {
        unix_get_file_stat_error(err);
        return false;
    }

    return S_ISDIR(path_stat.st_mode);
#endif
}

acorn::Path acorn::get_absolute_path(const Path& path, std::string& err) {
#if WIN_OS
    auto utf16_path = path.to_utf16_string();
    // TODO (maddie): because windows there is a thing called "long file name"
    // which can extend above `MAX_PATH` length we need to handle larger sizes.
    // Apparently this happens when there is \\?\ prepended to the path.
    wchar_t buffer[MAX_PATH];
    DWORD length = GetFullPathNameW(utf16_path.c_str(), MAX_PATH, buffer, nullptr);
    if (length != 0) {
        return Path(buffer, length);
    }
    win32_error_code_to_string(GetLastError(), err);
    return Path();
#else
    auto utf8_path = path.to_utf8_string();
    char buffer[PATH_MAX];
    if (realpath(utf8_path.c_str(), buffer)) {
        return SystemPath(buffer);
    }
    err = strerror(errno);
    return SystemPath();
#endif
}

acorn::Path acorn::get_parent_directory(const Path& path, std::string& err) {

    auto parent_directory = get_absolute_path(path, err);
    if (!err.empty()) {
        return Path{};
    }

#if WIN_OS
    auto utf16_path = parent_directory.to_utf16_string();
    size_t idx = utf16_path.find_last_of('\\');

    // check to see if it is the system root by checking if it specifies
    // a drive like "C:\"
    if (idx == 2 && utf16_path[1] == ':') {
        return Path(utf16_path.substr(0, 3));
    }

    return Path(utf16_path.substr(0, idx + 1));
#else
    auto utf8_path = parent_directory.to_utf8_string();
    if (utf8_path == '/') {
        return SystemPath(utf8_path);
    }

    size_t idx = utf8_path.find_last_of('/');
    return SystemPath(utf8_path.substr(0, idx + 1));
#endif
}

acorn::Path acorn::get_current_directory_path(std::string& err) {
    auto path = get_executable_path(err);
    if (!err.empty()) {
        return acorn::Path();
    }

    return get_parent_directory(path, err);
}

acorn::Path acorn::get_executable_path(std::string& err) {
#if WIN_OS
    // TODO (maddie): because windows there is a thing called "long file name"
    // which can extend above MAX_PATH length. Apparently this happens when there
    // is \\?\ prepended to the path.
    wchar_t buffer[MAX_PATH];
    DWORD length = GetModuleFileNameW(nullptr, buffer, MAX_PATH);
    if (length != 0) {
        return Path(buffer, length);
    }
    win32_error_code_to_string(GetLastError(), err);
    return Path();
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
    err = strerror(errno);
    return SystemPath();
#endif
}

void acorn::make_directory(const Path& path, std::string& err, bool error_if_exists) {

    auto check_already_exists = [&path, &err, error_if_exists]() finline {
        if (error_if_exists) {
            err = "Already exists";
            return;
        }

        if (!is_directory(path, err)) {
            if (!err.empty()) {
                return;
            }
            err = "Already exists as a but not as directory";
        }
    };

#if WIN_OS

    auto utf16_path = path.to_utf16_string();

    if (!CreateDirectoryW(utf16_path.c_str(), nullptr)) {
        DWORD ec = GetLastError();
        if (ec == ERROR_ALREADY_EXISTS) {
            check_already_exists();
        } else if (ec == ERROR_PATH_NOT_FOUND) {
            err = "Path not found";
        } else {
            win32_error_code_to_string(ec, err);
        }
    }
#else
    auto utf8_path = path.to_utf8_string();
    if (mkdir(utf8_path.c_str(), 0775) != 0) {
        if (errno == EEXIST) {
            check_already_exists();
        } else if (errno == ENOENT) {
            err = "Path not found";
        } else {
            err = strerror(errno);
        }
    }
#endif
}

void acorn::remove_file(const Path& path, std::string& err) {
#if WIN_OS
    auto utf16_path = path.to_utf16_string();

    DWORD attribs = GetFileAttributesW(utf16_path.c_str());
    if (attribs == INVALID_FILE_ATTRIBUTES) {
        DWORD ec = GetLastError();
        win32_get_file_attribs_error(ec, err);
        return;
    }

    if ((attribs & FILE_ATTRIBUTE_DIRECTORY) != 0) {
        err = "Path is directory";
        return;
    }

    if (DeleteFileW(utf16_path.c_str()) == 0) {
        win32_error_code_to_string(GetLastError(), err);
        return;
    }
#else

    struct stat path_stat;

    std::string utf8_path = path.to_utf8_string();
    if (stat(utf8_path.c_str(), &path_stat) != 0) {
        unix_get_file_stat_error(err);
        return;
    }

    if (S_ISDIR(path_stat.st_mode)) {
        err = "Path is directory";
        return;
    }

    if (unlink(utf8_path.c_str()) != 0) {
        err = strerror(errno);
    }
#endif
}

void acorn::recursively_iterate_directory(const Path& dir_path,
                                          std::string& err,
                                          std::function<void(Path, PathKind)> callback) {

    if (!is_directory(dir_path, err)) {
        if (!err.empty()) {
            return;
        }
        err = "Not a directory";
        return;
    }

#if WIN_OS
    std::wstring utf16_path = dir_path.to_utf16_string();
    std::wstring glob_path = utf16_path + L"\\*";

    WIN32_FIND_DATAW find_data;
    HANDLE handle = FindFirstFileW(glob_path.c_str(), &find_data);

    if (handle == INVALID_HANDLE_VALUE) {
        DWORD ec = GetLastError();
        if (ec == ERROR_NO_MORE_FILES) {
            return;
        }
        win32_error_code_to_string(ec, err);
        return;
    }

    do {

        const wchar_t* name = find_data.cFileName;

        // Skip "." and ".."
        if (wcscmp(name, L".") == 0 || wcscmp(name, L"..") == 0) {
            continue;
        }

        bool is_directory = find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;

        auto entry_path = utf16_path + L"/" + name;
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

            callback(Path(entry_path), kind);
        } else {
            recursively_iterate_directory(Path(entry_path), err, callback);
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

    win32_error_code_to_string(ec, err);
#else

    std::string utf8_path = dir_path.to_utf8_string();

    DIR* dir = opendir(utf8_path.c_str());
    if (!dir) {
        err = strerror(errno);
        return;
    }

    struct dirent* entry;
    while (true) {
        // We have to set the errror number to zero because it is possible
        // an error happened somewhere else in the code and it is important
        // not to confuse errors happening outside this function for errors
        // happening because of it.
        errno = 0;
        entry = readdir(dir);
        if (!entry) {
            break;
        }

        const char* name = entry->d_name;

        // Skip "." and ".."
        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) {
            continue;
        }

        std::string full_path = utf8_path + "/" + name;

        struct stat entry_stat;
        if (stat(full_path.c_str(), &entry_stat) != 0) {
            unix_get_file_stat_error(err);
            closedir(dir);
            return;
        }

        if (S_ISDIR(entry_stat.st_mode)) {
            recursively_iterate_directory(SystemPath(full_path), err, callback);
            if (!err.empty()) {
                closedir(dir);
                return;
            }
        } else if (S_ISREG(entry_stat.st_mode)) {
            callback(SystemPath(full_path), PathKind::REGULAR);
        } else {
            callback(SystemPath(full_path), PathKind::OTHER);
        }
    }

    if (errno != 0) {
        err = strerror(errno);
        return;
    }

    closedir(dir);

#endif
}

bool acorn::read_file_to_buffer(const Path&    file_path,
                                std::string&   err,
                                char*&         buffer,
                                size_t&        length,
                                uint64_t       max_file_size,
                                PageAllocator& allocator) {
#if WIN_OS

    auto utf16_path = file_path.to_utf16_string();

    auto handle = CreateFileW(utf16_path.c_str(),
                              GENERIC_READ,
                              FILE_SHARE_READ,   // allows for multiple handles to open the file at once.
                              nullptr,           // security attributes
                              OPEN_EXISTING,     // must already exist.
                              FILE_ATTRIBUTE_NORMAL,
                              nullptr
    );
    if (handle == INVALID_HANDLE_VALUE) {
        win32_get_file_attribs_error(GetLastError(), err);
        return false;
    }

    LARGE_INTEGER win32_file_size;
    if (!GetFileSizeEx(handle, &win32_file_size)) {
        win32_get_file_attribs_error(GetLastError(), err);
        CloseHandle(handle);
        return false;
    }

    if (static_cast<uint64_t>(win32_file_size.QuadPart) > max_file_size) {
        err = "file size too large";
        err += ". maximum file size: " + std::to_string((size_t)(max_file_size / 1e6)) + "MB";
        CloseHandle(handle);
        return false;
    }

    length = static_cast<size_t>(win32_file_size.QuadPart);
    buffer = (char*)allocator.allocate(length + 1); // +1 for null terminator.

    // keep reading bytes as long as we are not at the maximum bytes.
    DWORD total_bytes_read = 0;
    while (total_bytes_read < length) {
        DWORD bytes_read;
        if (!ReadFile(handle, buffer, (DWORD)(length) - total_bytes_read, &bytes_read, nullptr)) {
            win32_get_file_attribs_error(GetLastError(), err);
            return false;
        }

        total_bytes_read += bytes_read;
    }

    // Null terminating.
    buffer[length] = '\0';

    CloseHandle(handle);
    return true;
#else

#endif
}
