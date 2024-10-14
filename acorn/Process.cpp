#include "Process.h"


#include "Logger.h"

#if WIN_OS
#include <Windows.h>
#undef min
#undef max

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

#elif UNIX_OS
#include <sys/wait.h>
#include <poll.h>
#endif 



#include <codecvt>

#define min(a, b) (a) > (b) ? (a) : (b)

#if UNIX_OS
namespace acorn {
    const char** get_cmd_and_args(wchar_t* process) {
        std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
        auto char_process = converter.to_bytes(process);

        auto cmd_and_args = split_by_whitespace(char_process);
        const char** argv = new const char*[cmd_and_args.size() + 1]; // +1 because requires nullptr at end.
        for (size_t i = 0; i < cmd_and_args.size(); ++i) {
            argv[i] = strdup(cmd_and_args[i].c_str());
        }
        argv[cmd_and_args.size()] = nullptr;

        return argv;
    }

    void add_process_directory(wchar_t* process_dir) {
        if (!process_dir) return;

        std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
        auto char_process_dir = converter.to_bytes(process_dir);
        if (chdir(char_process_dir.c_str()) != 0) {
            acorn_fatal("Failed to change directory for new process");
        }
    }
}
#endif

bool acorn::exe_hidden_process(wchar_t* process, wchar_t* process_dir, std::string& std_out, int& exit_code) {
#if WIN_OS
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
        return false;
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
#elif UNIX_OS

    // pipes[0] - read
    // pipes[1] - write
    int pipes[2];
    if (pipe(pipes) == -1) {
        acorn_fatal("Failed to create pipe for process");
    }

    // Note: While fork() is said to make a clone of the parent process it does
    //       not actually create a copy of all the memory. There is a concept called
    //       Copy-on-Write in which the OS can wait until the process tries to write
    //       to memory of a page then it will make a copy.
    //
    //       This is good for us because otherwise we would have to worry about taking
    //       up too much memory.
    pid_t pid = fork();
    if (pid == -1) {
        acorn_fatal("Failed to fork process");
    } else if (pid == 0) {
        // Child process
    
        add_process_directory(process_dir);

        close(pipes[0]); // Closing read end of pipe

        // Redirect stdout and stderr to the pipe.
        if (dup2(pipes[1], STDOUT_FILENO) == -1) {
            acorn_fatal("Failed to redirect output");
        }
        if (dup2(pipes[1], STDERR_FILENO) == -1) {
            acorn_fatal("Failed to redirect output");
        }
        close(pipes[1]); // Close the write end of the pipe.

        const char** cmd_and_args = get_cmd_and_args(process);

        // Calling execvp will give over control of the this child process
        // to the command.
        int status = execvp(cmd_and_args[0], const_cast<char* const*>(cmd_and_args));
        
        // Check to see if it could not find the specified command.
        if (errno == ENOENT) {
            _exit(127);
        }
        
        // If we get here then failed.
        _exit(EXIT_FAILURE);

    } else {
        // Parent process
        close(pipes[1]); // Close write end of the pipe

        struct pollfd poll_set;
        poll_set.fd = pipes[0];
        poll_set.events = POLLIN;

        bool is_running = true;
        while (is_running) {
            // 1  is the number in the set
            // -1 is for infinite timeout.
            int ret = poll(&poll_set, 1, -1);
            if (ret == -1) {
                acorn_fatal("Failed process poll");
            }

            if (poll_set.revents & POLLHUP) {
                break;
            }

            if ((poll_set.revents & POLLIN)) {
                char buffer[1024];
                ssize_t count = read(pipes[0], buffer, sizeof(buffer) - 1);
                if (count > 0) {
                    buffer[count] = '\0';
                    std_out += buffer;
                } else if (count == 0) {
                    is_running = false;
                } else {
                    acorn_fatal("Failed to read process");
                }
            }
        }

        close(pipes[0]); // Close read end of the pipe

        int status;
        // Wait on child process to finish.
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            exit_code = WEXITSTATUS(status);
        } else {
            exit_code = -1;
        }

        if (exit_code == 127) {
            return false;
        }
    }

#endif

    return true;
}

bool acorn::exe_process(wchar_t* process, wchar_t* process_dir, bool seperate_window, int& exit_code) {
#if WIN_OS
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
    const char* char_process_dir = nullptr;
    if (process_dir) {
        std::string converted = wconverter.to_bytes(process_dir);
        char_process_dir = converted.c_str();
    }
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
        return false;
    }
    WaitForSingleObject(process_info.hProcess, INFINITE);

    unsigned long vexit_code;
    GetExitCodeProcess(process_info.hProcess, &vexit_code);
    exit_code = static_cast<int>(vexit_code);

    CloseHandle(process_info.hProcess);
    CloseHandle(process_info.hThread);
#elif UNIX_OS

    // Read explaination under exe_hidden_process about memory concerns.
    pid_t pid = fork();
    if (pid == -1) {
        acorn_fatal("Failed to fork process");
    } else if (pid == 0) {
        // Child process

        add_process_directory(process_dir);

        // TODO: If we want to create a new window this means we would have to
        //       determine the supported terminal to even be able to create
        //       one such as xterm.

        const char** cmd_and_args = get_cmd_and_args(process);
        
        // Calling execvp will give over control of the this child process
        // to the command.
        int status = execvp(cmd_and_args[0], const_cast<char* const*>(cmd_and_args));

        // Check to see if it could not find the specified command.
        if (errno == ENOENT) {
            _exit(127);
        }

        // If we get here then failed.
        _exit(EXIT_FAILURE);

    } else {
        // Parent process
        int status;
        // Wait on child process to finish.
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            exit_code = WEXITSTATUS(status);
        } else {
            exit_code = -1;
        }

        if (exit_code == 127) {
            return false;
        }
    }

#endif

    return true;
}
