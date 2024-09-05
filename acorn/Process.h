#ifndef PROCESS_H
#define PROCESS_H

#include <string>

namespace acorn {
    
    // Creates a new process in a hidden windows. The resulting standard
    // output of the process is then returned via a string.
    // 
    void exe_hidden_process(wchar_t* process, wchar_t* process_dir, std::string& std_out, int& exit_code);

    void exe_process(wchar_t* process, wchar_t* process_dir, bool seperate_window, int& exit_code);
}

#endif // PROCESS_H