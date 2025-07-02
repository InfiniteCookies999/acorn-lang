#ifndef SYSTEM_FILES_H
#define SYSTEM_FILES_H

#include <string>

#include "Util.h"

namespace acorn {

    class SystemPath {
    public:

        SystemPath();

        explicit SystemPath(std::string path);

        explicit SystemPath(std::wstring path);

        explicit SystemPath(const char* path);
        explicit SystemPath(const char* path, size_t length);

        explicit SystemPath(const wchar_t* path);
        explicit SystemPath(const wchar_t* path, size_t length);

        SystemPath parent_directory(std::string& err) const;

        SystemPath operator/(const char* path) {
            return append_path(std::string(path, strlen(path)));
        }
        SystemPath operator/(const std::string& path) {
            return append_path(path);
        }

        std::string  to_utf8_string() const;
        std::wstring to_wide_string() const;

        std::string utf8_extension() const;

    private:
        enum class Storage {
            UTF8,
            WIDE,
            NONE
        } storage;

        std::string  utf8;
        std::wstring wide;

        SystemPath append_path(const std::string& path);

        void normalize_empty();

    };

    enum class PathKind {
        REGULAR,
        DIRECTORY,
        OTHER
    };

    SystemPath get_current_directory_path(std::string& err);

    SystemPath get_executable_path(std::string& err);

    SystemPath get_absolute_path(const SystemPath& path, std::string& err);

    void make_directory(const SystemPath& path, std::string& err, bool error_if_exists = true);

    void remove_file(const SystemPath& path, std::string& err);

    bool path_exists(const SystemPath& path, std::string& err);

    bool is_directory(const SystemPath& path, std::string& err);

    void recursively_iterate_directory(const SystemPath& dir_path,
                                       std::string& err,
                                       std::function<void(SystemPath, PathKind)> callback);

}

#endif