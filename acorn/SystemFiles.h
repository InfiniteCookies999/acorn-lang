#ifndef SYSTEM_FILES_H
#define SYSTEM_FILES_H

#include <string>

#include "Util.h"

namespace acorn {

    class Path {
    public:

        Path();

        explicit Path(std::string path);

        explicit Path(std::wstring path);

        explicit Path(const char* path);
        explicit Path(const char* path, size_t length);

        explicit Path(const wchar_t* path);
        explicit Path(const wchar_t* path, size_t length);

        // converts to a utf8 string and normalizes any backslashes
        // as forward slashes.
        std::string to_normalized_utf8_string() const;

        // To string conversions.
        std::string  to_utf8_string() const;
        std::wstring to_utf16_string() const;

        // gets the extension as a utf8 string.
        std::string get_extension() const;

        Path operator/(const char* path) {
            return append_path(std::string(path, strlen(path)));
        }
        Path operator/(const std::string& path) {
            return append_path(path);
        }

    private:
        enum class Storage {
            UTF8,
            UTF16,
            NONE
        } storage;

        std::string  utf8;
        std::wstring utf16;

        Path append_path(const std::string& path);

    };

    enum class PathKind {
        REGULAR,
        DIRECTORY,
        OTHER
    };

    // Checks if the given path exists on the system. returns false if it
    // does not or if it failed to check in the first place in which case
    // an error is placed into `err`.
    bool path_exists(const Path& path, std::string& err);

    // Checks if the given path is a directory on the system. returns false
    // if it is not or if it failed to check in the first place in which case
    // an error is placed into `err`.
    bool is_directory(const Path& path, std::string& err);

    // Guarantees that the path ends with a slash.
    Path get_absolute_path(const Path& path, std::string& err);

    // Gets the parent directory or if it is the system root directory returns the
    // system root directory path.
    //
    // This function guarantees that the returns path will be absolute. that is when
    // `Path::to_normalized_utf8_string()` is called it will return an absolute path.
    //
    // it will also guarantee that the path ends with a slash.
    Path get_parent_directory(const Path& path, std::string& err);

    // Gets the directory path where the executable is location.
    Path get_current_directory_path(std::string& err);

    Path get_executable_path(std::string& err);

    void make_directory(const Path& path, std::string& err, bool error_if_exists = true);

    void remove_file(const Path& path, std::string& err);

    void recursively_iterate_directory(const Path& dir_path,
                                       std::string& err,
                                       std::function<void(Path, PathKind)> callback);

    bool read_file_to_buffer(const Path&    file_path,
                             std::string&   err,
                             char*&         buffer,
                             size_t&        length,
                             uint64_t       max_file_size,
                             PageAllocator& allocator);

}

#endif
