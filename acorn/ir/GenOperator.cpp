#include "IRGen.h"

#include "../Logger.h"
#include "../Util.h"

llvm::Value* acorn::IRGenerator::gen_binary_op(BinOp* bin_op) {
    Expr* lhs = bin_op->lhs, *rhs = bin_op->rhs;
    
    auto apply_op_eq = [this, bin_op, lhs, rhs](tokkind op) finline {
        auto ll_address = gen_node(lhs);
        auto ll_value = gen_binary_numeric_op(op,
                                              bin_op,
                                              builder.CreateLoad(gen_type(lhs->type), ll_address),
                                              gen_rvalue(rhs));
        builder.CreateStore(ll_value, ll_address);
        return nullptr;
    };

    switch (bin_op->op) {
    case '=': {
        auto ll_address = gen_node(lhs);
        gen_assignment(ll_address, rhs);
        return ll_address;
    }
    case Token::AddEq:   return apply_op_eq('+');
    case Token::SubEq:   return apply_op_eq('-');
    case Token::MulEq:   return apply_op_eq('*');
    case Token::DivEq:   return apply_op_eq('/');
    case Token::ModEq:   return apply_op_eq('%');
    case Token::AndEq:   return apply_op_eq('&');
    case Token::OrEq:    return apply_op_eq('|');
    case Token::CaretEq: return apply_op_eq('^');
    case Token::TildeEq: return apply_op_eq('~');
    case Token::LtLtEq:  return apply_op_eq(Token::LtLt);
    case Token::GtGtEq:  return apply_op_eq(Token::GtGt);
    default:
        return gen_binary_numeric_op(bin_op->op, bin_op,
                                     gen_rvalue(lhs), gen_rvalue(rhs));
    }
}

// TODO: At the moment we assume there will be no signed overflow
//       for signed calculation but not for unsigned. This is because
//       we want stuff like 'uint32 a = -1' to be defined behavior.
// 
//       We may want to be more strict and throw an exception if it
//       happens. That or just always wrap around.
//
//       In C++ it is defined behavior for unsigned but not signed.

// We use CreateInBoundsGEP because accessing memory beyond the bounds of
// valid memory is considered undefined behavior and it allows LLVM to
// perform better optimizations.

llvm::Value* acorn::IRGenerator::gen_binary_numeric_op(tokkind op, BinOp* bin_op,
                                                       llvm::Value* ll_lhs, llvm::Value* ll_rhs) {
    
    switch (op) {
    // Arithmetic Operators
    // -------------------------------------------
    case '+': {
        if (bin_op->type->is_pointer()) {
            // Pointer arithmetic
            auto ll_ptr = bin_op->lhs->type->is_pointer() ? ll_lhs : ll_rhs;
            auto ll_off = bin_op->lhs->type->is_pointer() ? ll_rhs : ll_lhs;
            auto ptr_type = bin_op->lhs->type->is_pointer() ? bin_op->lhs->type : bin_op->rhs->type;
            auto elm_type = as<PointerType*>(ptr_type)->get_elm_type();

            ll_off = builder.CreateIntCast(ll_off, gen_ptrsize_int_type(), true);
            return builder.CreateInBoundsGEP(gen_type(elm_type), ll_ptr, ll_off, "ptr.add");
        } else {
            if (bin_op->type->is_signed())
                return builder.CreateNSWAdd(ll_lhs, ll_rhs, "add");
            else return builder.CreateAdd(ll_lhs, ll_rhs, "add");
        }
    }
    case '-': {
        if (bin_op->rhs->type->is_pointer()) {
            // Subtracting a pointer from a pointer
            auto ptr_type = as<PointerType*>(bin_op->rhs->type);
            auto ll_elm_type = gen_type(ptr_type->get_elm_type());
            return builder.CreatePtrDiff(ll_elm_type, ll_lhs, ll_rhs, "ptrs.sub");
        } else if (bin_op->type->is_pointer()) {
            // Subtracting an integer from a pointer.

            auto ll_ptr = bin_op->lhs->type->is_pointer() ? ll_lhs : ll_rhs;
            auto ll_off = bin_op->lhs->type->is_pointer() ? ll_rhs : ll_lhs;
            auto ptr_type = bin_op->lhs->type->is_pointer() ? bin_op->lhs->type : bin_op->rhs->type;
            auto elm_type = as<PointerType*>(ptr_type)->get_elm_type();
            auto val_type = bin_op->lhs->type->is_pointer() ? bin_op->rhs->type : bin_op->lhs->type;

            auto ll_zero = gen_zero(val_type);
            auto ll_neg  = builder.CreateSub(ll_zero, ll_off, "neg");
            ll_neg = builder.CreateIntCast(ll_neg, gen_ptrsize_int_type(), true);
            return builder.CreateInBoundsGEP(gen_type(elm_type), ll_ptr, ll_neg, "ptr.sub");
        } else {
            if (bin_op->type->is_signed())
                return builder.CreateNSWSub(ll_lhs, ll_rhs, "sub");
            else return builder.CreateSub(ll_lhs, ll_rhs, "sub");
        }
    }
    case '*':
        return builder.CreateMul(ll_lhs, ll_rhs, "mul");
    case '/':
        return builder.CreateSDiv(ll_lhs, ll_rhs, "div");
    case '%':
        return builder.CreateSRem(ll_lhs, ll_rhs, "rem");
    // Bitwise Operators
    // -------------------------------------------
    case '|':
        return builder.CreateOr(ll_lhs, ll_rhs, "or");
    case '&':
        return builder.CreateAnd(ll_lhs, ll_rhs, "and");
    case '^':
        return builder.CreateXor(ll_lhs, ll_rhs, "xor");
    case Token::GtGt: // >>
        if (bin_op->lhs->type->is_signed())
            return builder.CreateAShr(ll_lhs, ll_rhs, "shr");
        else return builder.CreateLShr(ll_lhs, ll_rhs, "shr");
    case Token::LtLt: // <<
        return builder.CreateShl(ll_lhs, ll_rhs, "shl");
    // Comparisons Operators
    // -------------------------------------------
    case '>':
        if (bin_op->lhs->type->is_signed() || bin_op->rhs->type->is_signed())
            return builder.CreateICmpSGT(ll_lhs, ll_rhs, "gt");
        else return builder.CreateICmpUGT(ll_lhs, ll_rhs, "gt");
    case '<':
        if (bin_op->lhs->type->is_signed() || bin_op->rhs->type->is_signed())
            return builder.CreateICmpSLT(ll_lhs, ll_rhs, "lt");
        else return builder.CreateICmpULT(ll_lhs, ll_rhs, "lt");
    case Token::GtEq:
        if (bin_op->lhs->type->is_signed() || bin_op->rhs->type->is_signed())
            return builder.CreateICmpSGE(ll_lhs, ll_rhs, "gte");
        else return builder.CreateICmpUGE(ll_lhs, ll_rhs, "gte");
    case Token::LtEq:
        if (bin_op->lhs->type->is_signed() || bin_op->rhs->type->is_signed())
            return builder.CreateICmpSLE(ll_lhs, ll_rhs, "lte");
        else return builder.CreateICmpULE(ll_lhs, ll_rhs, "lte");
    case Token::EqEq:
        return builder.CreateICmpEQ(ll_lhs, ll_rhs, "eq");
    case Token::ExEq:
        return builder.CreateICmpNE(ll_lhs, ll_rhs, "neq");
    default:
        acorn_fatal("gen_binary_op(): failed to implement case");
        return nullptr;
    }
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_unary_op(UnaryOp* unary_op) {
    Expr* expr = unary_op->expr;

    auto gen_inc_or_dec = [this, unary_op](bool add, bool is_post) finline {
        auto type = unary_op->expr->type;
        llvm::Value* ll_addr  = gen_node(unary_op->expr);
        llvm::Value* ll_value = builder.CreateLoad(gen_type(unary_op->expr->type), ll_addr);
        llvm::Value* ll_org   = ll_value;
        if (add) {
            if (type->is_pointer()) {
                // Pointer arithmetic
                auto elm_type = as<PointerType*>(type)->get_elm_type();
                ll_value = builder.CreateInBoundsGEP(gen_type(elm_type), ll_value, gen_isize(1), "ptr.inc");
            } else {
                if (type->is_signed())
                    ll_value = builder.CreateNSWSub(ll_value, gen_one(type), "inc");
                else ll_value = builder.CreateAdd(ll_value, gen_one(type), "inc");
            }
        } else {
            if (type->is_pointer()) {
                // Pointer arithmetic
                auto elm_type = as<PointerType*>(type)->get_elm_type();
                ll_value = builder.CreateInBoundsGEP(gen_type(elm_type), ll_value, gen_isize(-1), "ptr.dec");
            } else {
                if (type->is_signed())
                    ll_value = builder.CreateNSWSub(ll_value, gen_one(type), "dec");
                else ll_value = builder.CreateSub(ll_value, gen_one(type), "dec");
            }
        }
        builder.CreateStore(ll_value, ll_addr);
        return is_post ? ll_org : ll_value;
    };

    switch (unary_op->op) {
    case '+': // Purely semantic and doesn't effect anything.
        return gen_rvalue(expr);
    case '-': {
        auto ll_value = gen_rvalue(expr);
        auto ll_zero = gen_zero(unary_op->type);

        if (unary_op->type->is_signed()) {
            return builder.CreateNSWSub(ll_zero, ll_value, "neg");
        } else {
            return builder.CreateSub(ll_zero, ll_value, "neg");
        }
    }
    case '~':
        return builder.CreateXor(gen_rvalue(expr), -1, "neg");
    case '!': {
        if (expr->type->is_pointer()) {
            auto ll_null = llvm::Constant::getNullValue(llvm::PointerType::get(ll_context, 0));
            return builder.CreateICmpEQ(gen_rvalue(expr), ll_null);
        }
        return builder.CreateXor(gen_rvalue(expr), 1, "not");
    }    
    case '&':
        // To get the address of the lvalue all we have to do is
        // get the value from gen_value since if we do not call
        // gen_rvalue it generates the address.
        return gen_node(expr);
    case '*': {
        // TODO: can we just call gen_rvalue here instead?
        auto ll_ptr = gen_node(expr);

        if (expr->is(NodeKind::BinOp)) {
            // If it is a binary operator then it is pointer arithmetic but
            // pointer arithmetic does not return the address of the pointer
            // but instead returns the pointer value itself (or in llvm terms
            // it is the eqv. of returning i32* (the pointer) rather than i32**
            // the address of the pointer).
            return ll_ptr;
        } else if (expr->is(NodeKind::UnaryOp)) {
            auto unary_op = as<UnaryOp*>(expr);
            auto op = unary_op->op;
            if (op == Token::AddAdd || op == Token::SubSub ||
                op == Token::PostAddAdd || op == Token::PostSubSub) {
                // Read comment for binary op. It is pointer arithmetic so
                // don't load.
                return ll_ptr;
            }
        } else if (expr->is(NodeKind::FuncCall)) {
            // If it is a function since functions dont return addresses what
            // we recieve is just the pointer value itself so there is nothing
            // to dereference.
            return ll_ptr;
        }

        return builder.CreateLoad(gen_type(expr->type), ll_ptr);
    }

    case Token::AddAdd:
        return gen_inc_or_dec(true, false);
    case Token::SubSub:
        return gen_inc_or_dec(false, false);
    case Token::PostAddAdd:
        return gen_inc_or_dec(true, true);
    case Token::PostSubSub:
        return gen_inc_or_dec(false, true);
    default:
        acorn_fatal("gen_unary_op(): failed to implement case");
        return nullptr;
    }
}
