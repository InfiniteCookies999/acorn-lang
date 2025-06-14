#ifndef SOURCE_EXPANSION_H
#define SOURCE_EXPANSION_H

#include "AST.h"

namespace acorn {

    // Keeps traversing until it encounters the close taking into
    // account balancing of opens/closes.
    void go_until(const char*& e, char open, char close);

    // Takes the error location of a node, decends
    // its tree, and creates a new error location
    // that includes all of the error locations of
    // the children.
    PointSourceLoc expand(Node* node);
}

#endif // SOURCE_EXPANSION_H