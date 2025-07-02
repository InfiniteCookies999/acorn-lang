#ifndef PROCESS_EXEC_H
#define PROCESS_EXEC_H

#include <string>

namespace acorn {

    // Creates a new process in a hidden windows. The resulting standard
    // output of the process is then returned via a string.
    //
    bool exe_hidden_process(char* process, char* process_dir, std::string& std_out, int& exit_code);

    bool exe_process(char* process, char* process_dir, bool seperate_window, int& exit_code);
}

#endif // PROCESS_EXEC_H