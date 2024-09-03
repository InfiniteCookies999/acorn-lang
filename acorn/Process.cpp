#include "Process.h"

#ifdef _WIN32
#include <Windows.h>
#undef min
#undef max
#endif

#include <codecvt>

#include "Logger.h"

#if defined(_UNICODE) || defined(UNICODE)
#define wide_funcs 1
#else
#define wide_funcs 0
#endif

#if wide_funcs
#define create_proc CreateProcessW
#else
#define create_proc CreateProcessA
#endif

#define min(a, b) (a) > (b) ? (a) : (b)

void acorn::exe_hidden_process(wchar_t* process, std::string& std_out, int& exit_code) {
#ifdef _WIN32
    HANDLE write_handle_in, write_handle;

    SECURITY_ATTRIBUTES security_attributes = {
        .nLength              = static_cast<DWORD>(sizeof(SECURITY_ATTRIBUTES)),
        .lpSecurityDescriptor = nullptr,
        .bInheritHandle       = TRUE
    };

    if (!CreatePipe(&write_handle_in, &write_handle, &security_attributes, 0)) {
        acorn_fatal("Failed to create pipe for process");
    }

    PROCESS_INFORMATION process_info = {0};
#if wide_funcs
    STARTUPINFOW startup_info = {0};
    startup_info.cb = sizeof(STARTUPINFOW);
#else
    STARTUPINFOA startup_info = {0};
    startup_info.cb = sizeof(STARTUPINFOA);
    std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
    auto char_process = wconverter.to_bytes(process);
#endif

    startup_info.dwFlags     = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
    startup_info.hStdOutput  = write_handle;
    startup_info.hStdError   = write_handle;
    startup_info.wShowWindow = SW_HIDE; // Don't show the newly created window.

    if (!create_proc(nullptr,
#if wide_funcs
                     process,
#else
                     char_process.data(),
#endif
                     nullptr, nullptr, TRUE,
                     CREATE_NEW_CONSOLE,
                     nullptr,
                     nullptr,
                     &startup_info,
                     &process_info)) {
        CloseHandle(write_handle);
        CloseHandle(write_handle_in);
        acorn_fatal("Failed to create process");
    }

    bool is_running = true;
    while (is_running) {
        
        is_running = !(WaitForSingleObject(process_info.hProcess, 50) == WAIT_OBJECT_0);

        while (true) {
            
            char buffer[1024];
            DWORD amount_read = 0, avail = 0;
            
            if (!PeekNamedPipe(write_handle_in, nullptr, 0, nullptr, &avail, nullptr)) {
                break;
            }

            if (!avail) {
                break;
            }

            if (!ReadFile(write_handle_in, buffer, min(sizeof(buffer) - 1, avail), &amount_read, nullptr)) {
                break;
            }

            buffer[amount_read] = 0; // Null terminating.
            std_out += buffer;
        }

    }

    unsigned long vexit_code;
    GetExitCodeProcess(process_info.hProcess, &vexit_code);
    exit_code = static_cast<int>(vexit_code);

    CloseHandle(write_handle);
    CloseHandle(write_handle_in);
    CloseHandle(process_info.hProcess);
    CloseHandle(process_info.hThread);

#endif
}

void acorn::exe_process(wchar_t* process, wchar_t* process_dir, bool seperate_window, int& exit_code) {
#ifdef _WIN32
    // No need to use inherited handles since this does not capture
    // the program's output.

    PROCESS_INFORMATION process_info = {0};
#if wide_funcs
    STARTUPINFOW startup_info = {0};
    startup_info.cb = sizeof(STARTUPINFOW);
#else
    STARTUPINFOA startup_info = {0};
    startup_info.cb = sizeof(STARTUPINFOA);
    std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
    auto char_process = wconverter.to_bytes(process);
    const char* char_process_dir = process_dir ? wconverter.to_bytes(process_dir).c_str() : nullptr;
#endif

    if (!create_proc(nullptr,
#if wide_funcs
                     process,
#else
                     char_process.data(),
#endif
                     nullptr, nullptr, FALSE,
                     seperate_window ? CREATE_NEW_CONSOLE : 0,
                     nullptr,
#if wide_funcs
                     process_dir,
#else
                     char_process_dir,
#endif
                     &startup_info,
                     &process_info)) {
        acorn_fatal("Failed to create process");
    }
    WaitForSingleObject(process_info.hProcess, INFINITE);

    unsigned long vexit_code;
    GetExitCodeProcess(process_info.hProcess, &vexit_code);
    exit_code = static_cast<int>(vexit_code);

    CloseHandle(process_info.hProcess);
    CloseHandle(process_info.hThread);

#endif
}
