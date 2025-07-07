#include "DeepCopyAST.h"
#include "PageAllocator.h"
#include "Logger.h"

namespace acorn {

    template<typename T>
    static T* raw_copy(PageAllocator& allocator, T* original) {
        T* new_node = allocator.alloc_type<T>();
        new (new_node) T();
        *new_node = *original;
        return new_node;
    }

    static Node* deep_copy_process(PageAllocator& allocator, Node* node);

    template<typename T>
    static T* deep_copy(PageAllocator& allocator, T* node) {
        Node* new_node = deep_copy_process(allocator, node);
        return static_cast<T*>(new_node);
    }

    static Node* deep_copy_process(PageAllocator& allocator, Node* node) {
        switch (node->kind) {
        case NodeKind::Func: {
            acorn_fatal("Functions cannot be deep copied");
            return nullptr;
        }
        case NodeKind::ImplicitFunc: {
            acorn_fatal("Implicit functions cannot be part of the original AST");
            return nullptr;
        }
        case NodeKind::Var: {
            auto var = raw_copy(allocator, static_cast<Var*>(node));
            if (var->assignment) {
                var->assignment = deep_copy(allocator, var->assignment);
            }
            return var;
        }
        case NodeKind::VarList: {
            acorn_fatal("Variable lists are resolved during parsing into just variables");
            return nullptr;
        }
        case NodeKind::Struct: {
            acorn_fatal("Structs cannot be deep copied only generic structs");
            return nullptr;
        }
        case NodeKind::Enum: {
            auto enumn = raw_copy(allocator, static_cast<Enum*>(node));
            for (auto& value : enumn->values) {
                value.assignment = deep_copy(allocator, value.assignment);
            }
            return enumn;
        }
        case NodeKind::Interface: {
            return raw_copy(allocator, static_cast<Interface*>(node));
        }
        case NodeKind::Generic: {
            acorn_fatal("cannot deep copy generics");
            return nullptr;
        }
        case NodeKind::IfStmt:
        case NodeKind::ReturnStmt:
        case NodeKind::ScopeStmt:
        case NodeKind::ImportStmt:
        case NodeKind::PredicateLoopStmt:
        case NodeKind::RangeLoopStmt:
        case NodeKind::IteratorLoopStmt:
        case NodeKind::ContinueStmt:
        case NodeKind::BreakStmt:
        case NodeKind::SwitchStmt:
        case NodeKind::RaiseStmt:
        case NodeKind::RecoverStmt: {
            acorn_fatal("cannot deep copy statements");
            return nullptr;
        }
        case NodeKind::ExprStart: {
            acorn_fatal("lol");
            return nullptr;
        }
        case NodeKind::InvalidExpr: {
            acorn_fatal("cannot deep copy invalid expressions");
            return nullptr;
        }
        case NodeKind::BinOp: {
            auto bin_op = raw_copy(allocator, static_cast<BinOp*>(node));
            bin_op->lhs = deep_copy(allocator, bin_op->lhs);
            bin_op->rhs = deep_copy(allocator, bin_op->rhs);
            return bin_op;
        }
        case NodeKind::UnaryOp: {
            auto unary_op = raw_copy(allocator, static_cast<UnaryOp*>(node));
            unary_op->expr = deep_copy(allocator, unary_op->expr);
            return unary_op;
        }
        // TODO (maddie): for some of these trivial types can we get away with not copying them?
        // At the moment there is a cast type that would cause problems but maybe there is a way
        // to check if casts happen first?
        case NodeKind::Number: {
            return raw_copy(allocator, static_cast<Number*>(node));
        }
        case NodeKind::Bool: {
            return raw_copy(allocator, static_cast<Bool*>(node));
        }
        case NodeKind::IdentRef: {
            return raw_copy(allocator, static_cast<IdentRef*>(node));
        }
        case NodeKind::DotOperator: {
            auto dot = raw_copy(allocator, static_cast<DotOperator*>(node));
            dot->site = deep_copy(allocator, dot->site);
            return dot;
        }
        case NodeKind::MemoryAccess: {
            auto mem_access = raw_copy(allocator, static_cast<MemoryAccess*>(node));
            mem_access->site = deep_copy(allocator, mem_access->site);
            mem_access->index = deep_copy(allocator, mem_access->index);
            return mem_access;
        }
        case NodeKind::NamedValue: {
            auto named_value = raw_copy(allocator, static_cast<NamedValue*>(node));
            named_value->assignment = deep_copy(allocator, named_value);
            return named_value;
        }
        case NodeKind::FuncCall: {
            auto call = raw_copy(allocator, static_cast<FuncCall*>(node));
            call->site = deep_copy(allocator, call->site);

            llvm::SmallVector<Expr*> new_args;
            new_args.resize(call->args.size());
            for (Expr* arg : call->args) {
                new_args.push_back(deep_copy(allocator, arg));
            }
            call->args = std::move(new_args);
            return call;
        }
        case NodeKind::StructInitializer: {
            auto initializer = raw_copy(allocator, static_cast<StructInitializer*>(node));
            initializer->site = deep_copy(allocator, initializer->site);

            llvm::SmallVector<Expr*> new_values;
            new_values.resize(initializer->values.size());
            for (Expr* value : initializer->values) {
                new_values.push_back(deep_copy(allocator, value));
            }
            initializer->values = std::move(new_values);
            return initializer;
        }
        case NodeKind::String: {
            return raw_copy(allocator, static_cast<String*>(node));
        }
        case NodeKind::Null: {
            return raw_copy(allocator, static_cast<Null*>(node));
        }
        case NodeKind::Cast: {
            auto cast = raw_copy(allocator, static_cast<Cast*>(node));
            cast->value = deep_copy(allocator, cast->value);
            return cast;
        }
        case NodeKind::Array: {
            auto arr = raw_copy(allocator, static_cast<Array*>(node));

            llvm::SmallVector<Expr*> new_elms;
            new_elms.resize(arr->elms.size());
            for (Expr* elm : arr->elms) {
                new_elms.push_back(deep_copy(allocator, elm));
            }
            arr->elms = std::move(new_elms);
            return arr;
        }
        case NodeKind::This: {
            return raw_copy(allocator, static_cast<This*>(node));
        }
        case NodeKind::SizeOf: {
            auto sizeofn = raw_copy(allocator, static_cast<SizeOf*>(node));
            sizeofn->value = deep_copy(allocator, sizeofn->value);
            return sizeofn;
        }
        case NodeKind::Ternary: {
            auto ternary = raw_copy(allocator, static_cast<Ternary*>(node));
            ternary->cond = deep_copy(allocator, ternary->cond);
            ternary->lhs = deep_copy(allocator, ternary->lhs);
            ternary->rhs = deep_copy(allocator, ternary->rhs);
            return ternary;
        }
        case NodeKind::MoveObj: {
            auto moveobj = raw_copy(allocator, static_cast<MoveObj*>(node));
            moveobj->value = deep_copy(allocator, moveobj->value);
            return moveobj;
        }
        case NodeKind::TypeExpr: {
            return raw_copy(allocator, static_cast<TypeExpr*>(node));
        }
        case NodeKind::Reflect: {
            auto reflect = raw_copy(allocator, static_cast<Reflect*>(node));
            if (reflect->expr) {
                reflect->expr = deep_copy(allocator, reflect->expr);
            }
            return reflect;
        }
        case NodeKind::Try: {
            acorn_fatal("cannot copy try statement");
            return nullptr;
        }
        case NodeKind::ExprEnd: {
            acorn_fatal("lol");
            return nullptr;
        }
        }
        return nullptr;
    }
}

acorn::Expr* acorn::deep_copy_expr(PageAllocator& allocator, Expr* expr) {
    return deep_copy(allocator, expr);
}

acorn::GenericStructInstance* acorn::deep_copy_struct(PageAllocator& allocator, UnboundGenericStruct* structn) {

    auto new_struct = allocator.alloc_type<GenericStructInstance>();
    new (new_struct) GenericStructInstance();
    // Copy over only the information we care about.
    {
        auto new_partial_struct = static_cast<Struct*>(new_struct);
        auto parital_struct     = static_cast<Struct*>(structn);
        *new_partial_struct = *parital_struct;
    }

    llvm::SmallVector<Var*> new_fields;
    for (Var* field : new_struct->fields) {
        Var* new_field = deep_copy(allocator, field);
        new_field->structn = new_struct;
        new_fields.push_back(new_field);
    }
    new_struct->fields = std::move(new_fields);

    return new_struct;
}
