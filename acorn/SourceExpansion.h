#ifndef SOURCE_EXPANSION_H
#define SOURCE_EXPANSION_H

#include "AST.h"

namespace acorn {
    // Takes the error location of a node, decends
    // its tree, and creates a new error location
    // that includes all of the error locations of
    // the children.
    PointSourceLoc expand(Node* node);
}

#endif // SOURCE_EXPANSION_H