#include "Parser.h"

#include <assert.h>
#include <format>
#include <stack>

#include "Type.h"
#include "Module.h"
#include "Util.h"
#include "SourceExpansion.h"

#define TypeTokens    \
     Token::KwVoid:   \
case Token::KwInt:    \
case Token::KwInt8:   \
case Token::KwInt16:  \
case Token::KwInt32:  \
case Token::KwInt64:  \
case Token::KwUInt8:  \
case Token::KwUInt16: \
case Token::KwUInt32: \
case Token::KwUInt64: \
case Token::KwConst:  \
case Token::KwChar:   \
case Token::KwChar16: \
case Token::KwChar32: \
case Token::KwUSize:  \
case Token::KwISize:  \
case Token::KwBool

#define ModifierTokens  \
     Token::KwNative:   \
case Token::KwDllimport

acorn::Parser::Parser(Context& context, Module& modl, SourceFile* file)
    : context(context),
      modl(modl),
      context_allocator(context.get_allocator()),
      file(file),
      logger(file->logger),
      lex(context, file->buffer, logger),
      type_table(context.type_table) {
}

void acorn::Parser::parse() {
    
    // Prime the parser.
    next_token();

    // Parsing imports.
    while (cur_token.is(Token::KwImport)) {
        parse_import_top();
    }

    // Have to set the previous token because the
    // current token does not exist.
    prev_token = cur_token;

    while (cur_token.is_not(Token::EOB)) {
        Node* node = parse_statement();
        if (!node) continue;

        if (node->is(NodeKind::Func)) {
            
            auto func = as<Func*>(node);
            if (func->name != Identifier::Invalid) {
                modl.add_global_function(func);
            }

            if (func->name == context.main_identifier) {
                context.add_canidate_main_function(func);
            }
        } else if (node->is(NodeKind::Var)) {

            auto var = as<Var*>(node);
            if (var->name != Identifier::Invalid) {
                modl.add_global_variable(var);
            }
        } else if (node->is(NodeKind::ComptimeIfStmt)) {
            modl.add_global_comptime_control_flow(node);
        } else {
            modl.mark_bad_scope(BadScopeLocation::Global, node, logger);
        }
    }

}

// Statement parsing
//--------------------------------------

void acorn::Parser::parse_import_top() {
    auto importn = parse_import();
    if (!importn) return;

    if (auto prev_import = modl.try_add_import(importn)) {
        error(importn->loc, "Duplicate import")
            .end_error(ErrCode::ParseDuplicateImport);
    }
}

acorn::ImportStmt* acorn::Parser::parse_import() {

    auto importn = new_node<ImportStmt>(cur_token);
    importn->file = file;
    next_token(); // Consuming 'import' token.

    Token start_token = cur_token;
    
    bool more_to_import = false;
    do {

        if (cur_token.is_not(Token::Identifier)) {
            error(cur_token, "Expected identifier for import location")
                .end_error(ErrCode::ParseExpectedImportIdentifier);
            skip_recovery();
            return nullptr;
        }
        Token ident_token = cur_token;
        next_token(); // Consuming the identifier.

        more_to_import = cur_token.is('.');
        if (more_to_import) {
            next_token();
        } else {
            importn->import_key = Identifier::get(ident_token.text());
        }
    } while (more_to_import);

    Token end_token = prev_token;

    const char* start = start_token.loc.ptr;
    const char* end = end_token.loc.ptr + end_token.loc.length;
    importn->location_key = llvm::StringRef(start, end - start);
    
    return importn;
}

acorn::Node* acorn::Parser::parse_statement() {
    uint32_t modifiers = 0;
    switch (cur_token.kind) {
    case Token::KwReturn: return parse_return();
    case Token::KwIf:     return parse_if();
    case Token::KwCTIf:   return parse_comptime_if();
    case Token::KwImport: {
        error(cur_token, "Import expected at top of file")
            .end_error(ErrCode::ParseImportNotTopOfFile);
        parse_import();
        return nullptr;
    }
    case ModifierTokens:
        modifiers = parse_modifiers();
        [[fallthrough]];
    case TypeTokens: {
        Type* type = parse_type();
        if (cur_token.is(Token::Identifier)) {
            if (peek_token(0).is('(')) {
                return parse_function(modifiers, type);
            } else {
                return parse_variable(modifiers, type);
            }
        } else {
            expect_identifier("for declaration");
            if (cur_token.is('=')) {
                return parse_variable(modifiers, type, Identifier());
            } else if (cur_token.is('(')) {
                return parse_function(modifiers, type, Identifier());
            } else if (cur_token.is_keyword() && peek_token(0).is('=')) {
                next_token(); // Consuming the extra keyword.
                return parse_variable(modifiers, type, Identifier());
            } else if (cur_token.is_keyword() && peek_token(0).is('(')) {
                next_token(); // Consuming the extra keyword.
                return parse_function(modifiers, type, Identifier());
            } else {
                skip_recovery();
            }
            return nullptr;
        }
    }
    case '{':
        return parse_scope();
    case ')': case '}': {
        // Handling these cases as if it is special because the skip recovery.
        // will treat them as recovery points.
        error("Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        next_token();
        skip_recovery();
        return nullptr;
    }
    default:
        return parse_assignment_and_expr();
    }
}

template<typename D, bool uses_linkname>
D* acorn::Parser::new_declaration(uint32_t modifiers, Identifier name, Token loc_token) {
    D* decl = new_node<D>(loc_token);
    decl->file      = file;
    decl->modifiers = modifiers;
    decl->name      = name;
    if constexpr (uses_linkname) {
        decl->linkname = linkname;
    }
    linkname = "";
    return decl;
}

acorn::Func* acorn::Parser::parse_function(uint32_t modifiers, Type* type) {
    return parse_function(modifiers, type, expect_identifier());
}

acorn::Func* acorn::Parser::parse_function(uint32_t modifiers, Type* type, Identifier name) {
    
    Func* func = new_declaration<Func, true>(modifiers, name, prev_token);
    func->return_type = type;

    Func* prev_func = cur_func;
    cur_func = func;

    // Parsing parameters.
    expect('(');
    if (cur_token.is_not(')') && cur_token.is_not('{')) {
        bool more_params = false, full_reported = false;
        uint32_t param_idx = 0;
        do {
            Var* param = parse_variable();
            param->param_idx = param_idx++;

            if (func->params.size() != MAX_FUNC_PARAMS) {
                func->params.push_back(param);
            } else if (!full_reported) {
                // TODO: The parameter might not refer to a location correctly.
                error(param->loc,
                      "Exceeded maximum number of function parameters. Max (%s)", MAX_FUNC_PARAMS)
                    .end_error(ErrCode::ParseExceededMaxFuncParams);
                full_reported = true;
            }

            more_params = cur_token.is(',');
            if (more_params) {
                next_token(); // Consuming ',' token.
            }
        } while (more_params);
        
    }
    expect(')', "for function declaration");

    // Parsing the scope of the function.
    if (!func->has_modifier(Modifier::Native) || cur_token.is('{')) {
        if (func->has_modifier(Modifier::Native)) {
            error(cur_token, "Native functions do not have bodies")
                .end_error(ErrCode::ParseNativeFuncNoHaveBodies);
        }
        func->scope = new_node<ScopeStmt>(cur_token);
        expect('{');
        while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
            Node* stmt = parse_statement();
            if (!stmt) continue;

            func->scope->push_back(stmt);
        }
        expect('}', "for function body");
    }

    cur_func = prev_func;

    context.add_unchecked_decl(func);
    
    return func;
}

acorn::Var* acorn::Parser::parse_variable() {
    return parse_variable(0, parse_type());
}

acorn::Var* acorn::Parser::parse_variable(uint32_t modifiers, Type* type) {
    return parse_variable(modifiers, type, expect_identifier());
}

acorn::Var* acorn::Parser::parse_variable(uint32_t modifiers, Type* type, Identifier name) {
    
    Var* var = new_declaration<Var, true>(modifiers, name, prev_token);
    var->type = type;
    
    if (cur_token.is('=')) {
        next_token(); // Consume '=' token.
        var->assignment = parse_expr();
    }

    if (!cur_func) {
        var->is_global = true;
        context.add_unchecked_decl(var);
    }

    return var;
}

uint32_t acorn::Parser::parse_modifiers() {
    uint32_t modifiers = 0;

    while (cur_token.is_modifier()) {
        switch (cur_token.kind) {
        case Token::KwNative: {
            if (modifiers & Modifier::Native)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Native;

            next_token();
            if (cur_token.is('(')) {
                next_token();
                if (cur_token.is(Token::String8BitLiteral)) {
                    
                    linkname = cur_token.text();
                    linkname = linkname.substr(1, linkname.size() - 2);
                    
                    next_token();
                } else if (cur_token.is(Token::String16BitLiteral) ||
                           cur_token.is(Token::String32BitLiteral)) {
                    error(cur_token, "Linkage name cannot contain unicode")
                        .end_error(ErrCode::ParseNativeLinkNameHasUnicode);
                } else if (cur_token.is(Token::InvalidStringLiteral)) {
                    // They writed to write a string but it was improperly lexed so
                    // we will just eat the token.
                    next_token();
                } else {
                    error(cur_token, "Expected string literal for linkage name")
                        .end_error(ErrCode::ParseNativeLinkNameNotString);
                }
                
                expect(')', "for native modifier");
            }
            break;
        }
        case Token::KwDllimport: {
            if (modifiers & Modifier::DllImport)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::DllImport;

            next_token();
            break;
        }
        }
    }
    return modifiers;
}

acorn::ReturnStmt* acorn::Parser::parse_return() {
    
    ReturnStmt* ret = new_node<ReturnStmt>(cur_token);

    bool has_value = lex.is_next_token_on_line() && peek_token(0).is_not('}');

    // Must go after checking if on a new line.
    next_token(); // Consuming 'return' keyword.
    
    if (has_value) {
        ret->value = parse_expr();
    }

    return ret;
}

acorn::IfStmt* acorn::Parser::parse_if() {

    IfStmt* ifs = new_node<IfStmt>(cur_token);
    next_token();
    
    // Note: If an error occures when trying to parse the expression
    //       the parser can simply recover at the next statement that
    //       represents the body of the if statement.
    switch (cur_token.kind) {
    case TypeTokens:
        ifs->cond = parse_variable();
        if (cur_token.is(';')) {
            next_token();
            ifs->post_variable_cond = parse_expr();
        }
        break;
    default:
        ifs->cond = parse_expr();
        break;
    }

    ifs->scope = parse_scope("for if");

    if (cur_token.is(Token::KwElIf)) {
        ifs->elseif = parse_if();
    } else if (cur_token.is(Token::KwElse)) {
        next_token(); // Consuming else token.
        ifs->elseif = parse_scope();
    }

    return ifs;
}

acorn::ComptimeIfStmt* acorn::Parser::parse_comptime_if(bool chain_start) {

    ComptimeIfStmt* ifs = new_node<ComptimeIfStmt>(cur_token);
    ifs->file = file;
    next_token();

    ifs->cond = parse_expr();
    ifs->scope = new_node<ScopeStmt>(cur_token);

    auto add_statement = [this](ScopeStmt* stmts, Node* stmt) finline{
        if (!cur_func &&
            !(stmt->is(NodeKind::Func) || stmt->is(NodeKind::Var))) {
            modl.mark_bad_scope(BadScopeLocation::Global, stmt, logger);
        }
        stmts->push_back(stmt);
    };

    while (cur_token.is_not(Token::KwCTEndIf) &&
           cur_token.is_not(Token::KwCTIf) &&
           cur_token.is_not(Token::EOB)) {
        if (cur_token.is(Token::KwCTElIf)) {
            ifs->elseif = parse_comptime_if(false);
            break;
        } else if (cur_token.is(Token::KwCTElse)) {
            
            ScopeStmt* else_stmts = new_node<ScopeStmt>(cur_token);
            
            next_token();
            while (cur_token.is_not(Token::KwCTEndIf) &&
                   cur_token.is_not(Token::KwCTIf) &&
                   cur_token.is_not(Token::EOB) &&
                   cur_token.is_not(Token::KwCTElIf) &&
                   cur_token.is_not(Token::KwCTElse)) {
                add_statement(else_stmts, parse_statement());
            }
            ifs->elseif = else_stmts;
            break;
        } else {
            add_statement(ifs->scope, parse_statement());
        }
    }

    if (chain_start) {
        if (cur_token.is_not(Token::KwCTEndIf)) {
            error(cur_token.loc, "Expected #endif for comptime #if statement")
                .end_error(ErrCode::ParseMissingComptimeEndIf);
        } else {
            next_token();
        }
    }

    return ifs;
}

acorn::ScopeStmt* acorn::Parser::parse_scope(const char* closing_for) {
    ScopeStmt* scope = new_node<ScopeStmt>(cur_token);
    if (cur_token.is('{')) {
        next_token(); // Consuming '{' token.

        while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
            if (Node* stmt = parse_statement()) {
                scope->push_back(stmt);
            }
        }

        expect('}', closing_for);
    } else {
        // Single statement scope.
        scope->push_back(parse_statement());
    }
    return scope;
}

// Expression parsing
//--------------------------------------

acorn::Type* acorn::Parser::parse_type() {
    Token first_token = cur_token;
    auto type = parse_base_type();

    while (cur_token.is('*')) {
        type = type_table.get_ptr_type(type);
        next_token();
    }

    llvm::SmallVector<Expr*, 8> arr_lengths;
    while (cur_token.is('[')) {
        next_token();
        
        arr_lengths.push_back(parse_expr());

        expect(']');
    }

    for (auto itr = arr_lengths.rbegin(); itr != arr_lengths.rend(); ++itr) {
        Expr* length_expr = *itr;

        bool resolvable = length_expr->is(NodeKind::Number);
        Number* number;
        if (resolvable) {
            number = as<Number*>(length_expr);
            resolvable &= number->type->is_integer() && number->type->get_number_of_bits() <= 32 &&
                          number->value_s32 > 0;
        }

        if (resolvable) {
            // handle common cases first in which it can resolve the length
            // immediately.
            type = type_table.get_arr_type(type, number->value_s32);
        } else {
            type = UnresolvedArrayType::create(context.get_allocator(), type, length_expr);
        }
    }

    if (type->is(context.const_void_type)) {
        SourceLoc error_loc = {
            .ptr = first_token.loc.ptr,
            .length = static_cast<uint16_t>(prev_token.loc.ptr + prev_token.loc.length - first_token.loc.ptr)
        };
        error(error_loc, "'const void' is not a valid type")
            .end_error(ErrCode::ParseConstVoidNotType);
        return nullptr;
    }
    return type;
}

acorn::Type* acorn::Parser::parse_base_type() {
    bool is_const = cur_token.is(Token::KwConst);
    if (is_const) {
        next_token();
    }
    
#define ty_const(t) \
if (is_const) {     \
return type_table.get_const_type(t); }

#define ty(t) { \
next_token();   \
ty_const(t);    \
return t; }

    switch (cur_token.kind) {
    case Token::KwVoid:   ty(context.void_type);
    case Token::KwInt:    ty(context.int_type);
    case Token::KwInt8:   ty(context.int8_type);
    case Token::KwInt16:  ty(context.int16_type);
    case Token::KwInt32:  ty(context.int32_type);
    case Token::KwInt64:  ty(context.int64_type);
    case Token::KwUInt8:  ty(context.uint8_type);
    case Token::KwUInt16: ty(context.uint16_type);
    case Token::KwUInt32: ty(context.uint32_type);
    case Token::KwUInt64: ty(context.uint64_type);
    case Token::KwBool:   ty(context.bool_type);
    case Token::KwChar:   ty(context.char_type);
    case Token::KwChar16: ty(context.char16_type);
    case Token::KwChar32: ty(context.char32_type);
    case Token::KwISize:  ty(context.isize_type);
    case Token::KwUSize:  ty(context.usize_type);
    default: {
        error("Expected type").end_error(ErrCode::ParseInvalidType);
        return context.invalid_type;
    }
    }

#undef ty_const
#undef ty
}

acorn::Expr* acorn::Parser::parse_assignment_and_expr() {
    Expr* lhs = parse_expr();
    switch (cur_token.kind) {
    case '=':
    case Token::AddEq:
    case Token::SubEq:
    case Token::MulEq:
    case Token::DivEq:
    case Token::ModEq:
    case Token::AndEq:
    case Token::OrEq:
    case Token::CaretEq:
    case Token::TildeEq:
    case Token::LtLtEq:
    case Token::GtGtEq: {
        BinOp* bin_op = new_node<BinOp>(cur_token);
        bin_op->op = cur_token.kind;
        next_token();
        bin_op->lhs = lhs;
        bin_op->rhs = parse_expr();
        return bin_op;
    }
    }
    return lhs;
}

acorn::Expr* acorn::Parser::parse_expr() {
    return parse_binary_expr();
}

std::pair<acorn::Token, acorn::Token> acorn::Parser::split_number_from_sign(Token token) {
    
    tokkind op_sign = *token.loc.ptr;
    Token op = Token(op_sign, SourceLoc{ token.loc.ptr, 1 });
    
    token.loc.ptr += 1;
    token.loc.length -= 1;

    return { op, token };
}

acorn::Expr* acorn::Parser::parse_binary_expr() {

    struct StackUnit {
        Token op;
        Expr* expr;
    };
    std::stack<StackUnit> op_stack;

    Expr* lhs = parse_postfix();

    auto get_op = [this]() finline {
        if (cur_token.is_number_literal()) {
            auto text = cur_token.text();
            if (text[0] == '-' || text[0] == '+') {
                auto [new_op, number_token] = split_number_from_sign(cur_token);
                cur_token = new_op;
                peeked_tokens[peeked_size++] = number_token;
                return new_op;
            }
        }
        return cur_token;
    };

    Token op = get_op(), next_op;

    int prec;
    while ((prec = context.get_op_precedence(op)) != -1) {
        next_token(); // Consuming the operator.

        Expr* rhs = parse_postfix();
        next_op = get_op();
        int next_prec = context.get_op_precedence(next_op);

        if (next_prec != -1 && next_prec > prec) {
            // Delaying the operation until later since the next operator
            // has a higher precedence.
            op_stack.push(StackUnit{ op, lhs });
            lhs = rhs;
            op = next_op;
        } else {
            if (lhs->is(NodeKind::Number) && rhs->is(NodeKind::Number)) {
                lhs = fold_number(op, lhs, rhs);
            } else {
                lhs = new_binary_op(op, lhs, rhs);
            }
            

            while (!op_stack.empty()) {
                StackUnit unit = op_stack.top();
                // Still possible to have the right side have higher precedence.
                if (next_prec != -1 && next_prec > context.get_op_precedence(unit.op)) {
                    break;
                }
                op_stack.pop();
                if (lhs->is(NodeKind::Number) && rhs->is(NodeKind::Number)) {
                    lhs = fold_number(unit.op, unit.expr, lhs);
                } else {
                    lhs = new_binary_op(unit.op, unit.expr, lhs);
                }
            }

            op = get_op();
        }
    }

    return lhs;
}

template<typename T>
acorn::Expr* acorn::Parser::fold_int(Token op, Number* lhs, Number* rhs, Type* to_type) {

    constexpr bool is_signed = std::is_signed_v<T>;
    T lval = is_signed ? lhs->value_s64 : lhs->value_u64;
    T rval = is_signed ? rhs->value_s64 : rhs->value_u64;

    auto calc = [op, lhs, rhs, to_type](T result) finline {
        if constexpr (is_signed) {
            lhs->value_s64 = result;
        } else {
            rhs->value_u64 = result;
        }

        // Going to treat the lhs as the result of the evaluation since
        // it is already a number and can just be reused.
        lhs->type = to_type;

        // Need to expand the source location because if an error occures
        // later and it needs to display the full error location.
        auto s = lhs->uses_expanded_loc ? lhs->expanded_loc.ptr : lhs->loc.ptr;;
        auto e = rhs->uses_expanded_loc ? (rhs->expanded_loc.ptr + rhs->expanded_loc.length)
                                        : (rhs->loc.ptr + rhs->loc.length);
        
        lhs->uses_expanded_loc = true;
        lhs->expanded_loc = PointSourceLoc{
            s,
            as<uint16_t>(e - s),
            op.loc.ptr,
            op.loc.length
        };
        return lhs;
    };

    auto report_overflow = [this, op, to_type, lhs, rhs]() finline {
        auto loc_op = new_binary_op(op, lhs, rhs);
        logger.begin_error(expand(loc_op), "Operator '%s' numeric overflow. Cannot fit for type '%s'",
              token_kind_to_string(context, op.kind), to_type)
            .end_error(ErrCode::NumericOverflow);
        return new_node<InvalidExpr>(op.loc);
    };

    auto report_underflow = [this, op, to_type, lhs, rhs]() finline {
        auto loc_op = new_binary_op(op, lhs, rhs);
        logger.begin_error(expand(loc_op), "Operator '%s' numeric underflow. Cannot fit into type '%s'",
              token_kind_to_string(context, op.kind), to_type)
            .end_error(ErrCode::NumericUnderflow);
        return new_node<InvalidExpr>(op.loc);
    };

    switch (op.kind) {
    case '+': {
        // Note: Even if one of the values is a signed negative value for the unsigned
        //       result case there cannot be negative underflow since the values are
        //       first cast to unsigned integers and what would be negative overflow is
        //       actually then caught as overflow.

        if (rval > 0 && lval > std::numeric_limits<T>::max() - rval) {
            return report_overflow();
        }
        
        if constexpr (is_signed) {
            if (rval < 0 && lval < std::numeric_limits<T>::min() - rval) {
                return report_underflow();
            }
        }
        return calc(lval + rval);
    }
    case '-': {
        if constexpr (!is_signed) { // unsigned
            if (lval < rval) {
                return report_underflow();
            }
        } else {
            if (rval < 0 && lval > std::numeric_limits<T>::max() + rval) {
                return report_overflow();
            }
            if (rval > 0 && lval < std::numeric_limits<T>::min() + rval) {
                return report_underflow();
            }
        }
        return calc(lval - rval);
    }
    case '*': {
        if constexpr (!is_signed) { // unsigned
            if (lval != 0 && rval > std::numeric_limits<T>::max() / lval) {
                return report_overflow();
            }
        } else {
            if (lhs != 0 && rhs != 0) {
                if (lval > std::numeric_limits<T>::max() / rval) {
                    return report_overflow();
                }
                if (lval < std::numeric_limits<T>::min() / rval) {
                    return report_underflow();
                }
            }
        }
        return calc(lval * rval);
    }
    case '/': {
        if (rval == 0) {
            // Let sema complain about division by zero.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval / rval);
    }
    case '%': {
        if (rval == 0) {
            // Let sema complain about division by zero.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval % rval);
    }
    case Token::LtLt: {
        if (rval < 0 || rval == 0 || (rval - 1) > to_type->get_number_of_bits()) {
            // These are shift error cases handled during sema.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval << rval);
    }
    case Token::GtGt: {
        if (rval < 0 || rval == 0 || (rval - 1) > to_type->get_number_of_bits()) {
            // These are shift error cases handled during sema.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval >> rval);
    }
    case '<':         return calc(lval <  rval);
    case '>':         return calc(lval >  rval);
    case Token::LtEq: return calc(lval <= rval);
    case Token::GtEq: return calc(lval >= rval);
    case Token::EqEq: return calc(lval == rval);
    case Token::ExEq: return calc(lval != rval);
    case '&':         return calc(lval &  rval);
    case '^':         return calc(lval ^  rval);
    case '|':         return calc(lval |  rval);
    default:
        // Handled during sema.
        return new_binary_op(op, lhs, rhs);
    }
}

acorn::Expr* acorn::Parser::fold_number(Token op, Expr* lhs, Expr* rhs) {

    auto fold_by_side = [op, this, lhs, rhs](Expr* side) -> Expr* finline {
        Number* lnum = as<Number*>(lhs);
        Number* rnum = as<Number*>(rhs);
        Type* to_type = side->type;
        switch (to_type->get_kind()) {
        case TypeKind::UInt8: case TypeKind::Char:
            return fold_int<uint8_t> (op, lnum, rnum, to_type);
        case TypeKind::UInt16: case TypeKind::Char16:
            return fold_int<uint16_t>(op, lnum, rnum, to_type);
        case TypeKind::UInt32: case TypeKind::Char32:
            return fold_int<uint32_t>(op, lnum, rnum, to_type);
        case TypeKind::UInt64: return fold_int<uint64_t>(op, lnum, rnum, to_type);
        case TypeKind::Int8:   return fold_int<int8_t> (op, lnum, rnum, to_type);
        case TypeKind::Int16:  return fold_int<int16_t>(op, lnum, rnum, to_type);
        case TypeKind::Int32: case TypeKind::Int:
            return fold_int<int32_t>(op, lnum, rnum, to_type);
        case TypeKind::Int64:  return fold_int<int64_t>(op, lnum, rnum, to_type);
        default:
            acorn_fatal("Missing numeric fold cast");
            return nullptr;
        }
    };

    if (lhs->type->is_integer() && rhs->type->is_integer()) {
        if (op.is(Token::LtLt) || op.is(Token::GtGt)) {
            // Shift cases are special because we assume that the left hand side
            // always has preference since it is the value worked on.
            if (lhs->type->is(context.int_type) || lhs->type->is(rhs->type)) {
                return fold_by_side(lhs);
            } else {
                // Just let it complain during sema that the types are incompatible.
                return new_binary_op(op, lhs, rhs);
            }
        } else {
            if (lhs->type->is(context.int_type)) { // rhs explicity type takes preference.
                return fold_by_side(rhs);
            } else if (rhs->type->is(context.int_type)) { // lhs explicity type takes preference.
                return fold_by_side(lhs);
            } else if (lhs->type->is(rhs->type)) {
                return fold_by_side(lhs);
            } else {
                // Just let it complain during sema that the types are incompatible.
                return new_binary_op(op, lhs, rhs);
            }
        }
    } else {
        acorn_fatal("unreachable");
        return nullptr;
    }
}

acorn::Expr* acorn::Parser::parse_postfix() {
    Expr* term = parse_term();
    if (cur_token.is(Token::AddAdd) || cur_token.is(Token::SubSub)) {
        // Language spec. if there is a whitespace then it does not consider it a post
        // inc/dec.
        const char* prev_ch_ptr = cur_token.loc.ptr - 1;
        if (std::isspace(*prev_ch_ptr)) {
            return term;
        }
        UnaryOp* unary_op = new_node<UnaryOp>(cur_token);
        unary_op->expr = term;
        if (cur_token.is(Token::AddAdd)) {
            unary_op->op = Token::PostAddAdd;
        } else {
            unary_op->op = Token::PostSubSub;
        }
        next_token();
        return unary_op;
    }
    return term;
}

acorn::Expr* acorn::Parser::parse_ident_ref_postfix(Expr* site) {
    while (cur_token.is('(') || cur_token.is('.')) {
        if (cur_token.is('(')) {
            site = parse_function_call(site);
        } else if (cur_token.is('.')) {
            site = parse_dot_operator(site);
        }
    }
    return site;
}

acorn::Expr* acorn::Parser::parse_function_call(Expr* site) {
    
    FuncCall* call = new_node<FuncCall>(cur_token);
    call->site = site;

    next_token(); // Consuming '(' token.

    if (cur_token.is_not(')')) {
        bool more_args = false, full_reported = false;
        do {
            Expr* arg;
            if (cur_token.is(Token::Identifier) && peek_token(0).is('=')) {
                
                auto named_arg = new_node<NamedValue>(cur_token);
                named_arg->name = Identifier::get(cur_token.text());
                arg = named_arg;
                next_token(); // Consuming identifier token.
                next_token(); // Consuming '=' token.

                named_arg->assignment = parse_expr();
            } else {
                arg = parse_expr();
            }

            if (call->args.size() != MAX_FUNC_PARAMS) {
                call->args.push_back(arg);
            } else if (!full_reported) {
                logger.begin_error(expand(arg), "Exceeded maximum number of function arguments. Max (%s)",
                                   MAX_FUNC_PARAMS)
                    .end_error(ErrCode::ParseExceededMaxFuncCallArgs);
                full_reported = true;
            }

            more_args = cur_token.is(',');
            if (more_args) {
                next_token();
            }
        } while (more_args);
    }

    expect(')', "for function call");

    return call;
}

acorn::Expr* acorn::Parser::parse_dot_operator(Expr* site) {

    auto dot = new_node<DotOperator>(cur_token);
    next_token(); // Consuming '.' token.

    dot->site = site;
    dot->ident = expect_identifier("for '.' operator");
    
    return dot;
}

acorn::Expr* acorn::Parser::parse_term() {
    switch (cur_token.kind) {
    case Token::IntLiteral:           return parse_int_literal();
    case Token::HexLiteral:           return parse_hex_literal();
    case Token::BinLiteral:           return parse_bin_literal();
    case Token::OctLiteral:           return parse_oct_literal();
    case Token::String8BitLiteral:    return parse_string8bit_literal();
    case Token::String16BitLiteral:   return parse_string16bit_literal();
    case Token::String32BitLiteral:   return parse_string32bit_literal();
    case Token::CharLiteral:          return parse_char_literal();
    case Token::InvalidStringLiteral:
    case Token::InvalidCharLiteral:
    case Token::InvalidNumberLiteral: {
        next_token();
        return new_node<InvalidExpr>(cur_token);
    }
    case Token::Identifier: {
        
        IdentRef* ref = new_node<IdentRef>(cur_token);
        ref->ident = Identifier::get(cur_token.text());

        next_token(); // Consuming the identifier.
        return parse_ident_ref_postfix(ref);
    }
    case '+': case '-': case '~': case '!':
    case '&': case Token::AddAdd: case Token::SubSub:
    case '*': {
        Token unary_token = cur_token;
        next_token(); // Consuming the unary token.

        bool unary_on_num_literal = cur_token.is(Token::IntLiteral);
        Token after_op_token = cur_token;
        Expr* expr = parse_term();

        if (unary_token.kind == '+' && expr->is(NodeKind::Number)) {
            return expr; // + has no effect on value.
        } else if (unary_token.kind == '-' && expr->is(NodeKind::Number)) {
            Number* num = as<Number*>(expr);

            switch (num->type->get_kind()) {
            case TypeKind::UInt64: num->value_u64 = -num->value_u64; break;
            case TypeKind::UInt32: case TypeKind::Char32:
                num->value_u32 = -num->value_u32; break;
            case TypeKind::UInt16: case TypeKind::Char16:
                num->value_u16 = -num->value_u16; break;
            case TypeKind::UInt8: case TypeKind::Char:
                num->value_u8  = -num->value_u8;  break;
            case TypeKind::Int64:  num->value_s64 = -num->value_s64; break;
            case TypeKind::Int32: case TypeKind::Int:
                num->value_s32 = -num->value_s32; break;
            case TypeKind::Int16:  num->value_s16 = -num->value_s16; break;
            case TypeKind::Int8:   num->value_s8  = -num->value_s8;  break;
            }

            return expr;
        } else if (unary_token.kind == '~' && expr->is(NodeKind::Number) &&
                   expr->type->is_integer()) {
            Number* num = as<Number*>(expr);
            
            switch (num->type->get_kind()) {
            case TypeKind::UInt64: num->value_u64 = ~num->value_u64; break;
            case TypeKind::UInt32: case TypeKind::Char32:
                num->value_u32 = ~num->value_u32; break;
            case TypeKind::UInt16: case TypeKind::Char16:
                num->value_u16 = ~num->value_u16; break;
            case TypeKind::UInt8: case TypeKind::Char:
                num->value_u8  = ~num->value_u8;  break;
            case TypeKind::Int64:  num->value_s64 = ~num->value_s64; break;
            case TypeKind::Int32: case TypeKind::Int:
                num->value_s32 = ~num->value_s32; break;
            case TypeKind::Int16:  num->value_s16 = ~num->value_s16; break;
            case TypeKind::Int8:   num->value_s8  = ~num->value_s8;  break;
            }

            return expr;
        }

        UnaryOp* unary_op = new_node<UnaryOp>(unary_token);
        unary_op->op = unary_token.kind;
        unary_op->expr = expr;
        
        return unary_op;
    }
    case '(': {
        next_token();
        Expr* expr = parse_expr();
        expect(')');
        return expr;
    }
    case '[':
        return parse_array();
    case Token::KwTrue: {
        Bool* b = new_node<Bool>(cur_token);
        next_token(); // Consuming 'true' token.
        b->value = true;
        b->type = context.bool_type;
        return b;
    }
    case Token::KwFalse: {
        Bool* b = new_node<Bool>(cur_token);
        next_token(); // Consuming 'false' token.
        b->value = false;
        b->type = context.bool_type;
        return b;
    }
    case Token::KwNull: {
        Null* null = new_node<Null>(cur_token);
        null->type = context.null_type;
        next_token();
        return null;
    }
    case Token::KwAs: {
        Cast* cast = new_node<Cast>(cur_token);
        next_token();
        expect('(');
        cast->explicit_cast_type = parse_type();
        expect(')');
        cast->value = parse_term();
        return cast;
    }
    default:
        error("Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        skip_recovery();
        return new_node<InvalidExpr>(cur_token);
    }
}

acorn::Expr* acorn::Parser::parse_int_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_number_literal<10, void_table, false>(text.data(), text.end());
}

static constinit uint64_t hex_table[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0-15
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 16-31
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 32-47
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, // 48-63 ('0' to '9')
    0, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 64-79 ('A' to 'F')
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 80-95
    0, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 96-111 ('a' to 'f')
    // The rest are zeroed out
};

acorn::Expr* acorn::Parser::parse_hex_literal() {
    const auto text = cur_token.text();
    return parse_number_literal<16, hex_table>(text.data() + 2, text.end());
}

acorn::Expr* acorn::Parser::parse_bin_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_number_literal<2, void_table, false>(text.data() + 2, text.end());
}

acorn::Expr* acorn::Parser::parse_oct_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_number_literal<8, void_table, false>(text.data() + 1, text.end());
}

namespace acorn {
    static char get_escape_char(char c) {
        switch (c) {
        case '\'': return '\'';
        case '\"': return '\"';
        case '\\': return '\\';
        case 'a':  return '\a';
        case 'b':  return '\b';
        case 'f':  return '\f';
        case 'n':  return '\n';
        case 'r':  return '\r';
        case 't':  return '\t';
        case 'v':  return '\v';
        default:   return c;
        }
    }

    template<typename T>
    static T parse_unicode_value(const char* start, const char* end) {
        const char* ptr = start;

        T value = 0;
        while (ptr != end) {
            char c = *ptr;
            ++ptr;

            value = value * 16;
            value += static_cast<T>(hex_table[c]);
        }

        return value;
    }
}

acorn::Expr* acorn::Parser::parse_string8bit_literal() {

    auto string = new_node<String>(cur_token);
    string->bit_type = String::Str8Bit;
    string->type = type_table.get_ptr_type(type_table.get_const_type(context.char_type));
    
    auto text = cur_token.text();
    const char* ptr = text.data() + 1; // +1 skip the "
    while (*ptr != '"') {
        if (*ptr == '\\') {
            ++ptr;
            string->text8bit += get_escape_char(*ptr);
            ++ptr;
        } else {
            string->text8bit += *ptr;
            ++ptr;
        }
    }
    next_token();
    return string;
}

acorn::Expr* acorn::Parser::parse_string16bit_literal() {

    auto string = new_node<String>(cur_token);
    string->bit_type = String::Str16Bit;
    string->type = type_table.get_ptr_type(type_table.get_const_type(context.char16_type));

    auto text = cur_token.text();
    const char* ptr = text.data() + 1; // +1 skip the "
    while (*ptr != '"') {
        if (*ptr == '\\') {
            ++ptr;
            if (*ptr == 'u') {
                ++ptr;
                string->text16bit += parse_unicode_value<char16_t>(ptr, ptr + 4);
                ptr += 4;
            } else {
                string->text16bit += static_cast<char16_t>(get_escape_char(*ptr));
                ++ptr;
            }
        } else {
            string->text16bit += static_cast<char16_t>(*ptr);
            ++ptr;
        }
    }
    next_token();
    return string;
}

acorn::Expr* acorn::Parser::parse_string32bit_literal() {
    
    auto string = new_node<String>(cur_token);
    string->bit_type = String::Str32Bit;
    string->type = type_table.get_ptr_type(type_table.get_const_type(context.char32_type));

    auto text = cur_token.text();
    const char* ptr = text.data() + 1; // +1 skip the "
    while (*ptr != '"') {
        if (*ptr == '\\') {
            ++ptr;
            if (*ptr == 'u') {
                ++ptr;
                string->text32bit += parse_unicode_value<char32_t>(ptr, ptr + 4);
                ptr += 4;
            } else if (*ptr == 'U') {
                ++ptr;
                string->text32bit += parse_unicode_value<char32_t>(ptr, ptr + 8);
                ptr += 8;
            } else {
                string->text32bit += static_cast<char32_t>(get_escape_char(*ptr));
                ++ptr;
            }
        } else {
            string->text32bit += static_cast<char32_t>(*ptr);
            ++ptr;
        }
    }
    next_token();
    return string;
}

acorn::Expr* acorn::Parser::parse_char_literal() {

    auto character = new_node<Number>(cur_token);

    auto text = cur_token.text();
    const char* ptr = text.data() + 1; // Skip the '

    if (*ptr == '\\' && *(ptr + 1) == 'u') {
        ptr += 2;
        character->value_u64 = parse_unicode_value<char16_t>(ptr, ptr + 4);
        character->type = context.char16_type;
    } else if (*ptr == '\\' && *(ptr + 1) == 'U') {
        ptr += 2;
        character->value_u64 = parse_unicode_value<char32_t>(ptr, ptr + 8);
        character->type = context.char32_type;
    } else if (*ptr == '\\') {
        character->value_u64 = get_escape_char(*ptr);
        character->type = context.char_type;
    } else {
        character->value_u64 = *ptr;
        character->type = context.char_type;
    }

    next_token();
    return character;
}

template<uint32_t radix, uint64_t convert_table[256], bool use_table>
acorn::Expr* acorn::Parser::parse_number_literal(const char* start, const char* end) {

    const char* ptr = start;
    bool neg_sign = *ptr == '-';
    if (*ptr == '-' || *ptr == '+') {
        ++ptr;
    }

    bool already_errored = false;
    uint64_t value = 0, prev_value;
    while (ptr != end) {
        char c = *ptr;

        if (c == number_seperator) {
            continue;
        } else if (c == '\'') {
            break;
        }

        ++ptr;

        prev_value = value;
        value = value * radix;
        if constexpr (use_table) {
            value += convert_table[c];
        } else {
            value += ((uint64_t)c - '0');
        }
        if (value / radix < prev_value) {
            already_errored = true;
            error(cur_token, "Integer value is too large")
                .end_error(ErrCode::ParseIntegerValueCalcOverflow);
            break;
        }
    }
    
    Number* number = new_node<Number>(cur_token);
    number->value_u64 = value;
    if (neg_sign) {
        if (!already_errored && number->value_u64 > 9223372036854775808ull) {
            error(cur_token, "Value too small to fit into 64 bit signed integer")
                .end_error(ErrCode::ParseIntegerValueCalcUnderflow);
        } else {
            number->value_s64 = -number->value_s64;
        }
    }

    if (*ptr == '\'') {
        ++ptr;
        bool is_signed = *ptr == 'i';
        ++ptr;

        auto check_range_fit = [this, number, neg_sign, already_errored]<typename T>(T) finline {
            if (!already_errored && !fits_in_range<T>(neg_sign ? number->value_s64 : number->value_s64)) {
                auto err_msg = get_error_msg_for_value_not_fit_type(number);
                logger.begin_error(number->loc, "%s", err_msg)
                      .end_error(ErrCode::ParseIntegerValueNotFitType);
            }
        };

        if (*ptr == '8') {
            if (is_signed) {
                number->type = context.int8_type;
                check_range_fit((int8_t)0);
            } else {
                number->type = context.uint8_type;
                check_range_fit((uint8_t)0);
            }
        } else if (*ptr == '1') {
            if (is_signed) number->type = context.int16_type , check_range_fit((int16_t)0);
            else           number->type = context.uint16_type, check_range_fit((uint16_t)0);
        } else if (*ptr == '3') {
            if (is_signed) number->type = context.int32_type , check_range_fit((int32_t)0);
            else           number->type = context.uint32_type, check_range_fit((uint32_t)0);
        } else {
            if (is_signed) number->type = context.int64_type, check_range_fit((int64_t)0);
            else           number->type = context.uint64_type;
        }
    } else {
        auto fit_range = [this, number]<typename V>(V value) finline {
            if (fits_in_range<int32_t>(value)) {
                number->type = context.int_type;
            } else if (fits_in_range<int64_t>(value)) {
                number->type = context.int64_type;
            } else {
                number->type = context.uint64_type;
            }
        };

        if (neg_sign) {
            fit_range(number->value_s64);
        } else {
            fit_range(number->value_u64);
        }
    }

    next_token();
    return number;
}

acorn::Expr* acorn::Parser::parse_array() {
    Array* arr = new_node<Array>(cur_token);
    next_token(); // Consuming '[' token.

    bool more_values = false;
    do {

        arr->elms.push_back(parse_expr());

        more_values = cur_token.is(',');
        if (more_values) {
            next_token();
        }
    } while (more_values);

    expect(']', "for array");
    return arr;
}

// Utility functions
//--------------------------------------

void acorn::Parser::next_token() {
    prev_token = cur_token;
    if (peeked_size > 0) {
        cur_token = peeked_tokens[--peeked_size];
    } else {
        cur_token = lex.next_token();
    }
}

acorn::Token acorn::Parser::peek_token(size_t n) {
    assert(n < MAX_PEEKED_TOKENS && "Peek index exceeds the maximum number of peeked tokens.");
    
    // Ensure the tokens up to n are stored.
    while (peeked_size <= n) {
        peeked_tokens[peeked_size++] = lex.next_token();
    }

    return peeked_tokens[n];
}

void acorn::Parser::expect(tokkind kind, const char* for_msg) {
    if (cur_token.is(kind)) {
        next_token();
    } else {
        const std::string str_kind = token_kind_to_string(context, kind);
        const std::string arrow_msg = std::format("add '{}' here", str_kind);
        const bool is_closing = kind == ')' || kind == '}';
        const auto closing_msg = is_closing ? "closing" : "";
        auto fixed_for_msg = for_msg ? std::string{ " " } + for_msg : "";

        logger.begin_error(prev_token.loc, "Expected %s '%s' token%s", closing_msg, str_kind, fixed_for_msg)
              .add_arrow_msg(Logger::ArrowLoc::After, arrow_msg)
              .end_error(ErrCode::ParseExpect);
    }
}

acorn::Identifier acorn::Parser::expect_identifier(const char* for_msg) {
    if (cur_token.is(Token::Identifier)) {
        Identifier identifier = Identifier::get(cur_token.text());
        next_token();
        return identifier;
    } else {
        auto fixed_for_msg = for_msg ? std::string{" "} + for_msg : "";
        logger.begin_error(cur_token.loc, "Expected identifier%s", fixed_for_msg);
        if (cur_token.is_keyword()) {
            logger.add_line("Help: '%s' is a keyword", to_string(context, cur_token));
        }
        logger.end_error(ErrCode::ParseExpectIdent);
        return Identifier();
    }
}

acorn::Expr* acorn::Parser::new_binary_op(Token op_tok, Expr* lhs, Expr* rhs) {
    BinOp* bin_op = new_node<BinOp>(op_tok);
    bin_op->op = op_tok.kind;
    bin_op->lhs = lhs;
    bin_op->rhs = rhs;
    return bin_op;
};

void acorn::Parser::skip_recovery() {
    while (true) {
        switch (cur_token.kind) {
        case Token::EOB:
        case ')': // TODO: Might want to count these so it doesn't just recover at bad times.
        case '{':
        case '}':
            return;
        case ModifierTokens:
            return;
        case TypeTokens:
            if (peek_token(0).is(Token::Identifier))
                return;
            next_token();
            break;
        case Token::KwImport:
        case Token::KwIf:
        case Token::KwReturn:
        case Token::KwCTIf:
            return;
        case Token::KwElIf: {
            // Replace current token with if/#if statement so that it thinks
            // it is a valid statement.
            cur_token = Token(Token::KwIf, cur_token.loc);
            return;
        }
        case Token::KwCTElIf: {
            cur_token = Token(Token::KwCTIf, cur_token.loc);
            return;
        }
        default:
            next_token();
            break;
        }
    }
}
