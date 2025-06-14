#ifndef RAISED_ERROR_H
#define RAISED_ERROR_H

#include "Identifier.h"
#include "Token.h"

namespace acorn {
    struct Struct;

    struct RaisedError {
        Identifier name;
        SourceLoc  error_loc;
        Struct* structn;
    };
}

#endif