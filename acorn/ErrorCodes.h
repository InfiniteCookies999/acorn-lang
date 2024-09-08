#ifndef ERROR_H
#define ERROR_H

#include <unordered_map>
#include <string>

namespace acorn {

    enum class ErrCode {
#define X(name, val) name = val,
#include "ErrorCodesDef.inc"
#undef X
    };

    inline std::unordered_map<ErrCode, std::string> ErrCodeStrs = {
#define X(name, ...) { ErrCode :: name , # name },
#include "ErrorCodesDef.inc"
#undef X
    };

    inline std::string error_code_to_string(ErrCode code) {
        return ErrCodeStrs [code];
    }
}

#endif // ERROR_H