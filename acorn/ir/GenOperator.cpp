#include "IRGen.h"

#include "../Logger.h"
#include "../Util.h"
#include "DebugGen.h"
#include "Context.h"

llvm::Value* acorn::IRGenerator::gen_binary_op(BinOp* bin_op) {
    Expr* lhs = bin_op->lhs;
    Expr *rhs = bin_op->rhs;

    switch (bin_op->op) {
    case '=':
        return gen_assignment_op(bin_op->lhs, bin_op->rhs);
    case Token::AddEq:   return gen_apply_and_assign_op('+', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::SubEq:   return gen_apply_and_assign_op('-', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::MulEq:   return gen_apply_and_assign_op('*', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::DivEq:   return gen_apply_and_assign_op('/', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::ModEq:   return gen_apply_and_assign_op('%', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::AndEq:   return gen_apply_and_assign_op('&', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::OrEq:    return gen_apply_and_assign_op('|', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::CaretEq: return gen_apply_and_assign_op('^', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::TildeEq: return gen_apply_and_assign_op('~', bin_op->loc, bin_op->type, lhs, rhs);
    case Token::LtLtEq:  return gen_apply_and_assign_op(Token::LtLt, bin_op->loc, bin_op->type, lhs, rhs);
    case Token::GtGtEq:  return gen_apply_and_assign_op(Token::GtGt, bin_op->loc, bin_op->type, lhs, rhs);
    case Token::AndAnd: {
        if (bin_op->is_foldable) {
            auto ll_lhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->lhs));
            auto ll_rhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->rhs));
            if (ll_lhs->isOne() && ll_rhs->isOne()) {
                return llvm::ConstantInt::getTrue(ll_context);
            } else {
                return llvm::ConstantInt::getFalse(ll_context);
            }
        }

        // See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CGExprScalar.cpp
        // VisitBinLAnd

        auto ll_end_bb      = gen_bblock("and.end");
        auto ll_lhs_true_bb = gen_bblock("and.lhs.true");

        // Generate children
        gen_branch_on_condition(bin_op->lhs, ll_lhs_true_bb, ll_end_bb);

        auto ll_result = llvm::PHINode::Create(
            llvm::Type::getInt1Ty(ll_context), // Incoming values are booleans.
            2,                                 // At least 2 blocks but could be more.
            "cond.res",
            ll_end_bb                          // Insert the phi node into the block after the condition blocks.
        );
        // Generating false for the phi node if the block comes from
        // a false block which is any block other than ll_lhs_true_bb.
        for (auto itr = llvm::pred_begin(ll_end_bb), end = llvm::pred_end(ll_end_bb);
             itr != end;
             ++itr) {
            ll_result->addIncoming(llvm::ConstantInt::getFalse(ll_context), *itr);
        }

        // Have to deal with the final block.
        insert_bblock_at_end(ll_lhs_true_bb);
        builder.SetInsertPoint(ll_lhs_true_bb);
        auto ll_cond = gen_condition(bin_op->rhs);
        // Add the final condition that will determine if the entire
        // chain was true.
        ll_result->addIncoming(ll_cond, ll_lhs_true_bb);

        // Continuing generating code in the end block after all the blocks.
        insert_bblock_at_end(ll_end_bb);
        builder.CreateBr(ll_end_bb);
        builder.SetInsertPoint(ll_end_bb);

        return ll_result;
    }
    case Token::OrOr: {
        if (bin_op->is_foldable) {
            auto ll_lhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->lhs));
            auto ll_rhs = llvm::cast<llvm::ConstantInt>(gen_condition(bin_op->rhs));
            if (ll_lhs->isOne() || ll_rhs->isOne()) {
                return llvm::ConstantInt::getTrue(ll_context);
            } else {
                return llvm::ConstantInt::getFalse(ll_context);
            }
        }

        auto ll_end_bb = gen_bblock("or.end");
        auto ll_lhs_false_bb = gen_bblock("or.lhs.false");

        // Generate children
        gen_branch_on_condition(bin_op->lhs, ll_end_bb, ll_lhs_false_bb);

        auto ll_result = llvm::PHINode::Create(
            llvm::Type::getInt1Ty(ll_context), // Incoming values are booleans.
            2,                                 // At least 2 blocks but could be more.
            "cond.res",
            ll_end_bb                          // Insert the phi node into the block after the condition blocks.
        );
        // Generating true for the phi node if the block comes from
        // a true block which is any block other than ll_lhs_false_bb.
        for (auto itr = llvm::pred_begin(ll_end_bb), end = llvm::pred_end(ll_end_bb);
             itr != end;
             ++itr) {
            ll_result->addIncoming(llvm::ConstantInt::getTrue(ll_context), *itr);
        }

        // Have to deal with the final block.
        insert_bblock_at_end(ll_lhs_false_bb);
        builder.SetInsertPoint(ll_lhs_false_bb);
        auto ll_cond = gen_condition(bin_op->rhs);
        // Add the final condition as a last chance at the expression
        // being true.
        ll_result->addIncoming(ll_cond, ll_lhs_false_bb);

        // Continuing generating code in the end block after all the blocks.
        insert_bblock_at_end(ll_end_bb);
        builder.CreateBr(ll_end_bb);
        builder.SetInsertPoint(ll_end_bb);

        return ll_result;
    }
    default:
        return gen_numeric_binary_op(bin_op->op, bin_op->type,
                                     lhs, rhs,
                                     gen_rvalue(lhs), gen_rvalue(rhs));
    }
}

llvm::Value* acorn::IRGenerator::gen_assignment_op(Expr* lhs, Expr* rhs) {
    emit_dbg(di_emitter->set_store_node(lhs));
    Try* prev_try = nullptr;
    if (rhs->tryn) {
        gen_try(rhs->tryn, prev_try);
    }
    auto ll_address = gen_node(lhs);
    gen_assignment(ll_address, lhs->type, rhs, lhs->loc, lhs, true);
    if (rhs->tryn) {
        finish_try(prev_try);
    }
    emit_dbg(di_emitter->clear_store_node());
    return ll_address;
}

llvm::Value* acorn::IRGenerator::gen_apply_and_assign_op(tokkind op, SourceLoc loc, Type* rtype, Expr* lhs, Expr* rhs) {

    emit_dbg(di_emitter->set_store_node(lhs));

    Try* prev_try = nullptr;
    if (rhs->tryn) {
        gen_try(rhs->tryn, prev_try);
    }
    auto ll_address = gen_node(lhs);
    auto ll_value = gen_numeric_binary_op(op, rtype,
                                          lhs, rhs,
                                          builder.CreateLoad(gen_type(lhs->type), ll_address),
                                          gen_rvalue(rhs));
    builder.CreateStore(ll_value, ll_address);
    emit_dbg(di_emitter->emit_location(builder, loc));
    if (rhs->tryn) {
        finish_try(prev_try);
    }

    emit_dbg(di_emitter->clear_store_node());
    return nullptr;
}

// We use CreateInBoundsGEP because accessing memory beyond the bounds of
// valid memory is considered undefined behavior and it allows LLVM to
// perform better optimizations.
//
// Many of the operations here differ from how clang emits arithmetic since
// we avoid the use of NSW operations. This is because acorn preferences
// defined behavior for arithmetic. NSW does not guarantee defined behavior
// but it does provide better performance which is the trade-off.
//
llvm::Value* acorn::IRGenerator::gen_numeric_binary_op(tokkind op, Type* rtype,
                                                       Expr* lhs, Expr* rhs,
                                                       llvm::Value* ll_lhs, llvm::Value* ll_rhs) {

    auto is_ptr_or_arr = [](Type* type) finline {
        return type->is_pointer() || type->is_array();
    };

    switch (op) {
    // Arithmetic Operators
    // -------------------------------------------
    case '+': {
        if (rtype->is_pointer()) {
            // Pointer arithmetic
            auto lhs_mem = is_ptr_or_arr(lhs->type);
            auto ll_mem = lhs_mem ? ll_lhs : ll_rhs;
            auto ll_off = lhs_mem ? ll_rhs : ll_lhs;
            auto mem_type = lhs_mem ? lhs->type : rhs->type;

            ll_off = builder.CreateIntCast(ll_off, gen_ptrsize_int_type(), true);
            if (mem_type->is_pointer()) {
                auto ptr_type = static_cast<PointerType*>(mem_type);
                auto elm_type = ptr_type->get_elm_type();
                auto ll_elm_type = elm_type->get_kind() != TypeKind::Void ? gen_type(elm_type)
                                                                          : builder.getInt8Ty();
                return builder.CreateInBoundsGEP(ll_elm_type, ll_mem, ll_off, "ptr.add");
            }

            return builder.CreateInBoundsGEP(gen_type(mem_type), ll_mem, { gen_isize(0), ll_off }, "arr.add");
        } else {
            if (rtype->is_integer())
                return builder.CreateAdd(ll_lhs, ll_rhs, "add");
            return builder.CreateFAdd(ll_lhs, ll_rhs, "add");
        }
    }
    case '-': {
        if (rhs->type->is_pointer()) {
            // Subtracting a pointer from a pointer
            auto ptr_type = static_cast<PointerType*>(rhs->type);
            auto ll_elm_type = gen_type(ptr_type->get_elm_type());
            return builder.CreatePtrDiff(ll_elm_type, ll_lhs, ll_rhs, "ptrs.sub");
        } else if (rtype->is_pointer()) {
            // Subtracting an integer from a pointer.

            auto lhs_mem = is_ptr_or_arr(lhs->type);
            auto ll_mem = lhs_mem ? ll_lhs : ll_rhs;
            auto ll_off = lhs_mem ? ll_rhs : ll_lhs;
            auto mem_type = lhs_mem ? lhs->type : rhs->type;
            auto val_type = lhs_mem ? rhs->type : lhs->type;

            auto ll_zero = gen_zero(val_type);
            auto ll_neg  = builder.CreateSub(ll_zero, ll_off, "neg");
            ll_neg = builder.CreateIntCast(ll_neg, gen_ptrsize_int_type(), true);
            if (mem_type->is_pointer()) {
                auto ptr_type = static_cast<PointerType*>(mem_type);
                auto elm_type = ptr_type->get_elm_type();
                return builder.CreateInBoundsGEP(gen_type(elm_type), ll_mem, ll_neg, "ptr.sub");
            }

            return builder.CreateInBoundsGEP(gen_type(mem_type), ll_mem, { gen_isize(0), ll_neg }, "arr.sub");
        } else {
            if (rtype->is_integer())
                return builder.CreateSub(ll_lhs, ll_rhs, "sub");
            return builder.CreateFSub(ll_lhs, ll_rhs, "sub");
        }
    }
    case '*':
        if (rtype->is_integer())
            return builder.CreateMul(ll_lhs, ll_rhs, "mul");
        else return builder.CreateFMul(ll_lhs, ll_rhs, "mul");
    case '/':
        if (rtype->is_integer())
            if (rtype->is_signed())
                return builder.CreateSDiv(ll_lhs, ll_rhs, "div");
            else return builder.CreateUDiv(ll_lhs, ll_rhs, "div");
        else return builder.CreateFDiv(ll_lhs, ll_rhs, "div");
    case '%': {
        if (rtype->is_signed())
            return builder.CreateSRem(ll_lhs, ll_rhs, "rem");
        else return builder.CreateURem(ll_lhs, ll_rhs, "rem");
    }
    // Bitwise Operators
    // -------------------------------------------
    case '|':
        return builder.CreateOr(ll_lhs, ll_rhs, "or");
    case '&':
        return builder.CreateAnd(ll_lhs, ll_rhs, "and");
    case '^':
        return builder.CreateXor(ll_lhs, ll_rhs, "xor");
    case Token::GtGt: // >>
        if (lhs->type->is_signed())
            return builder.CreateAShr(ll_lhs, ll_rhs, "shr");
        else return builder.CreateLShr(ll_lhs, ll_rhs, "shr");
    case Token::LtLt: // <<
        return builder.CreateShl(ll_lhs, ll_rhs, "shl");
    // Comparisons Operators
    // -------------------------------------------
    case '>':
        if (ll_lhs->getType()->isFloatTy() || ll_rhs->getType()->isFloatTy())
            return builder.CreateFCmpOGT(ll_lhs, ll_rhs, "gt");
        if (lhs->type->is_signed() || rhs->type->is_signed())
            return builder.CreateICmpSGT(ll_lhs, ll_rhs, "gt");
        else return builder.CreateICmpUGT(ll_lhs, ll_rhs, "gt");
    case '<':
        if (ll_lhs->getType()->isFloatTy() || ll_rhs->getType()->isFloatTy())
            return builder.CreateFCmpOLT(ll_lhs, ll_rhs, "gt");
        if (lhs->type->is_signed() || rhs->type->is_signed())
            return builder.CreateICmpSLT(ll_lhs, ll_rhs, "lt");
        else return builder.CreateICmpULT(ll_lhs, ll_rhs, "lt");
    case Token::GtEq:
        if (ll_lhs->getType()->isFloatTy() || ll_rhs->getType()->isFloatTy())
            return builder.CreateFCmpOGE(ll_lhs, ll_rhs, "gt");
        if (lhs->type->is_signed() || rhs->type->is_signed())
            return builder.CreateICmpSGE(ll_lhs, ll_rhs, "gte");
        else return builder.CreateICmpUGE(ll_lhs, ll_rhs, "gte");
    case Token::LtEq:
        if (ll_lhs->getType()->isFloatTy() || ll_rhs->getType()->isFloatTy())
            return builder.CreateFCmpOLE(ll_lhs, ll_rhs, "gt");
        if (lhs->type->is_signed() || rhs->type->is_signed())
            return builder.CreateICmpSLE(ll_lhs, ll_rhs, "lte");
        else return builder.CreateICmpULE(ll_lhs, ll_rhs, "lte");
    case Token::EqEq:
        return gen_equal(ll_lhs, ll_rhs);
    case Token::ExEq:
        if (ll_lhs->getType()->isFloatTy() || ll_rhs->getType()->isFloatTy())
            return builder.CreateFCmpUNE(ll_lhs, ll_rhs, "neq");
        else return builder.CreateICmpNE(ll_lhs, ll_rhs, "neq");
    default:
        acorn_fatal("gen_binary_op(): failed to implement case");
        return nullptr;
    }
    return nullptr;
}

llvm::Value* acorn::IRGenerator::gen_equal(llvm::Value* ll_lhs, llvm::Value* ll_rhs) {
    if (ll_lhs->getType()->isFloatTy() || ll_rhs->getType()->isFloatTy())
        return builder.CreateFCmpOEQ(ll_lhs, ll_rhs, "eq");
    return builder.CreateICmpEQ(ll_lhs, ll_rhs, "eq");
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
                auto ptr_type = static_cast<PointerType*>(type);
                auto elm_type = ptr_type->get_elm_type();
                ll_value = builder.CreateInBoundsGEP(gen_type(elm_type), ll_value, gen_isize(1), "ptr.inc");
            } else {
                ll_value = builder.CreateAdd(ll_value, gen_one(type), "inc");
            }
        } else {
            if (type->is_pointer()) {
                // Pointer arithmetic
                auto ptr_type = static_cast<PointerType*>(type);
                auto elm_type = ptr_type->get_elm_type();
                ll_value = builder.CreateInBoundsGEP(gen_type(elm_type), ll_value, gen_isize(-1), "ptr.dec");
            } else {
                ll_value = builder.CreateSub(ll_value, gen_one(type), "dec");
            }
        }
        builder.CreateStore(ll_value, ll_addr);
        emit_dbg(di_emitter->emit_location(builder, unary_op->loc));
        return is_post ? ll_org : ll_value;
    };

    switch (unary_op->op) {
    case '+': // Purely semantic and doesn't effect anything.
        return gen_rvalue(expr);
    case '-': {
        auto ll_value = gen_rvalue(expr);
        if (unary_op->type->is_float())
            return builder.CreateFNeg(ll_value, "neg");

        auto ll_zero = gen_zero(unary_op->type);
        return builder.CreateSub(ll_zero, ll_value, "neg");
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
    case '&': {
        if (expr->is_foldable) {
            // The expression is foldable so it does not have an address. To fix this
            // issue a global variable will be created and then the address of that will
            // be taken instead.
            return gen_foldable_global_variable(static_cast<IdentRef*>(expr));
        } else {
            // To get the address of the lvalue all we have to do is
            // get the value from gen_value since if we do not call
            // gen_rvalue it generates the address.
            return gen_node(expr);
        }
    }
    case '*': {
        auto ll_ptr = gen_node(expr);

        if (expr->is(NodeKind::BinOp)) {
            // If it is a binary operator then it is pointer arithmetic but
            // pointer arithmetic does not return the address of the pointer
            // but instead returns the pointer value itself (or in llvm terms
            // it is the eqv. of returning i32* (the pointer) rather than i32**
            // the address of the pointer).
            return ll_ptr;
        } else if (expr->is(NodeKind::UnaryOp)) {
            auto unary_op = static_cast<UnaryOp*>(expr);
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
        } else if (expr->is(NodeKind::This)) {
            // We store the 'this' pointer by value not by address.
            return ll_ptr;
        } else if (expr->is(NodeKind::Cast)) {
            // Because casting calls gen_rvalue the value has already been
            // loading.
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

llvm::Value* acorn::IRGenerator::gen_ternary(Ternary* ternary,
                                             llvm::Value* ll_dest_addr,
                                             bool is_assign_op,
                                             bool try_move) {

    if (ternary->type->is_aggregate()) {
        // Basically have to create an if statement.

        if (!ll_dest_addr) {
            ll_dest_addr = gen_unseen_alloca(ternary->type, "tmp.aggr");
        }

        auto ll_then_bb = gen_bblock("tern.then");
        auto ll_else_bb = gen_bblock("tern.else");
        auto ll_end_bb  = gen_bblock("tern.end");

        gen_branch_on_condition(ternary->cond, ll_then_bb, ll_else_bb);

        // Then block
        insert_bblock_at_end(ll_then_bb);
        builder.SetInsertPoint(ll_then_bb);
        gen_assignment(ll_dest_addr, ternary->type, ternary->lhs, ternary->loc, nullptr, is_assign_op, try_move);
        gen_branch_if_not_term(ll_end_bb);

        // Else block
        insert_bblock_at_end(ll_else_bb);
        builder.SetInsertPoint(ll_else_bb);
        gen_assignment(ll_dest_addr, ternary->type, ternary->rhs, ternary->loc, nullptr, is_assign_op, try_move);
        gen_branch_if_not_term(ll_end_bb);

        // Continuing forward into a new block after the ternary.
        insert_bblock_at_end(ll_end_bb);
        builder.SetInsertPoint(ll_end_bb);

        return ll_dest_addr;
    } else if (!ternary->is_foldable) {
        // Still have to create an if statement because it is possible that it modifies
        // memory in some way on one of the paths.

        auto ll_then_bb = gen_bblock("tern.then");
        auto ll_else_bb = gen_bblock("tern.else");
        auto ll_end_bb  = gen_bblock("tern.end");

        gen_branch_on_condition(ternary->cond, ll_then_bb, ll_else_bb);

        // Then block
        insert_bblock_at_end(ll_then_bb);
        builder.SetInsertPoint(ll_then_bb);
        auto ll_lhs = gen_rvalue(ternary->lhs);
        gen_branch_if_not_term(ll_end_bb);

        // Else block
        insert_bblock_at_end(ll_else_bb);
        builder.SetInsertPoint(ll_else_bb);
        auto ll_rhs = gen_rvalue(ternary->rhs);
        gen_branch_if_not_term(ll_end_bb);

        insert_bblock_at_end(ll_end_bb);
        builder.SetInsertPoint(ll_end_bb);

        auto ll_result = builder.CreatePHI(gen_type(ternary->type), 2, "tern.res");
        ll_result->addIncoming(ll_lhs, ll_then_bb);
        ll_result->addIncoming(ll_rhs, ll_else_bb);
        return ll_result;
    } else {
        auto ll_cond = gen_condition(ternary->cond);
        auto ll_lhs = gen_rvalue(ternary->lhs);
        auto ll_rhs = gen_rvalue(ternary->rhs);

        return builder.CreateSelect(ll_cond, ll_lhs, ll_rhs);
    }
}
