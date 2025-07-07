#ifndef DEEP_COPY_AST_H
#define DEEP_COPY_AST_H

#include "AST.h"

namespace acorn {

    Expr* deep_copy_expr(PageAllocator& allocator, Expr* expr);

    GenericStructInstance* deep_copy_struct(PageAllocator& allocator, UnboundGenericStruct* structn);

}

#endif // DEEP_COPY_AST_H
