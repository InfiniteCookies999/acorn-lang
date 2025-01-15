#include "Parser.h"

#include <assert.h>
#include <format>
#include <stack>
#include <cfenv>

#include "Type.h"
#include "Module.h"
#include "Util.h"
#include "SourceExpansion.h"
#include "SourceFile.h"
#include "Sema.h"

#define TypeTokens     \
     Token::KwVoid:    \
case Token::KwInt:     \
case Token::KwInt8:    \
case Token::KwInt16:   \
case Token::KwInt32:   \
case Token::KwInt64:   \
case Token::KwUInt8:   \
case Token::KwUInt16:  \
case Token::KwUInt32:  \
case Token::KwUInt64:  \
case Token::KwFloat32: \
case Token::KwFloat64: \
case Token::KwConst:   \
case Token::KwChar:    \
case Token::KwChar16:  \
case Token::KwChar32:  \
case Token::KwUSize:   \
case Token::KwISize:   \
case Token::KwBool:    \
case Token::KwAuto

#define ModifierTokens   \
     Token::KwNative:    \
case Token::KwDllimport: \
case Token::KwPublic:    \
case Token::KwPrivate:   \
case Token::KwReadonly

acorn::Parser::Parser(Context& context, Module& modl, SourceFile* file)
    : context(context),
      modl(modl),
      allocator(context.get_allocator()),
      file(file),
      logger(file->logger),
      lex(context, file->buffer, logger),
      type_table(context.type_table) {
}

void acorn::Parser::parse() {

    // Prime the parser.
    next_token();

    if (cur_token.is(Token::KwCTFile)) {
        parse_comptime_file_info();
    }

    // Parsing imports.
    while (parsing_import_tops &&
           (cur_token.is(Token::KwImport) || cur_token.is(Token::KwCTIf))) {
        if (cur_token.is(Token::KwImport)) {
            parse_import_top();
        } else {
            parse_comptime_if();
        }
    }
    parsing_import_tops = false;

    // Have to set the previous token because the
    // current token does not exist.
    prev_token = cur_token;

    while (cur_token.is_not(Token::EOB)) {
        Node* node = parse_statement();
        if (!node) continue;

        add_node_to_global_scope(node);
    }
}

void acorn::Parser::add_node_to_global_scope(Node* node) {

    auto process_variable = [this](Var* var) finline {
        if (var->name != Identifier::Invalid) {
            context.add_unchecked_decl(var);
            file->add_variable(var);
        }
    };

    if (node->is(NodeKind::Func)) {

        auto func = static_cast<Func*>(node);
        if (func->name != Identifier::Invalid) {
            context.add_unchecked_decl(func);
            file->add_function(func);
        }

        if (func->name == context.main_identifier) {
            context.add_canidate_main_function(func);
        }
    } else if (node->is(NodeKind::VarList)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->list) {
            process_variable(var);
        }
        vlist->list.clear();
    } else if (node->is(NodeKind::Var)) {
        process_variable(static_cast<Var*>(node));
    } else if (node->is(NodeKind::Struct) || node->is(NodeKind::Enum)) {

        auto structn = static_cast<Decl*>(node);
        if (structn->name != Identifier::Invalid) {
            context.add_unchecked_decl(structn);
            file->add_composite(structn);
        }
    } else {
        modl.mark_bad_scope(ScopeLocation::Global, node, logger);
    }
}

// Statement parsing
//--------------------------------------

void acorn::Parser::parse_import_top() {
    auto importn = parse_import();
    if (!importn) return;

    if (auto prev_import = file->try_add_import(importn)) {
        error(importn->loc, "Duplicate import")
            .end_error(ErrCode::ParseDuplicateImport);
    }
}

acorn::ImportStmt* acorn::Parser::parse_import() {

    auto importn = new_node<ImportStmt>(cur_token);
    importn->file = file;
    next_token(); // Consuming 'import' token.

    if (cur_token.is('.')) {
        next_token();
        importn->within_same_modl = true;
    } else if (cur_token.is(Token::DotDot)) {
        next_token();
        importn->within_parent_modl = true;
    }

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

        importn->key.push_back({ Identifier::get(ident_token.text()), ident_token.loc });

        more_to_import = cur_token.is('.');
        if (more_to_import) {
            next_token();
        }
    } while (more_to_import);

    if (cur_token.is(Token::KwStatic)) {
        next_token();
        importn->is_static = true;
    }

    expect(';');

    return importn;
}

acorn::Node* acorn::Parser::parse_statement() {

    uint32_t modifiers = 0;
    switch (cur_token.kind) {
    case Token::KwReturn: {
        auto stmt = parse_return();
        expect(';');
        return stmt;
    }
    case Token::KwIf:     return parse_if();
    case Token::KwCTIf: {
        parse_comptime_if();
        return nullptr;
    }
    case Token::KwLoop:   return parse_loop();
    case Token::KwImport: {
        error(cur_token, "Import expected at top of file")
            .end_error(ErrCode::ParseImportNotTopOfFile);
        parse_import();
        expect(';');
        return nullptr;
    }
    case Token::KwContinue:
    case Token::KwBreak: {
        auto stmt = parse_loop_control();
        expect(';');
        return stmt;
    }
    case Token::KwSwitch: return parse_switch();
    case Token::Identifier: {
        if (cur_struct) {
            auto ident = Identifier::get(cur_token.text());
            if (cur_struct->name == ident && peek_token(0).is('(')) {
                next_token(); // Consuming the identifier token.
                return parse_function(0, context.void_type, false, cur_struct->name);
            }
        }

        return parse_ident_decl_or_expr(false);
    }
    case ModifierTokens:
        modifiers = parse_modifiers();
        if (cur_token.is(Token::KwStruct)) {
            next_token(); // Consuming 'struct' token.
            auto name = expect_identifier("for struct");
            return parse_struct(modifiers, name);
        } else if (cur_token.is(Token::KwEnum)) {
            return parse_enum(modifiers);
        } else if (cur_token.is(Token::Identifier)) {
            auto ident = Identifier::get(cur_token.text());
            if (cur_struct && cur_struct->name == ident && peek_token(0).is('(')) {
                next_token(); // Consuming the identifier token.
                return parse_function(modifiers, context.void_type, false, cur_struct->name);
            }
        } else if (cur_token.is('~') && peek_token(0).is(Token::Identifier)) {
            auto peek0 = peek_token(0);
            auto ident = Identifier::get(peek0.text());
            if (cur_struct && cur_struct->name == ident && peek_token(1).is('(')) {
                // Encountered a destructor!
                next_token(); // Consuming '~' token.
                next_token(); // Consuming the identifier token.
                auto text = peek0.text();
                auto name = Identifier::get(llvm::StringRef(text.data() - 1, text.size() + 1));
                auto destructor = parse_function(modifiers, context.void_type, false, ident);
                destructor->is_destructor = true;
                return destructor;
            }
        }

        [[fallthrough]];
    case TypeTokens: {
        return parse_decl(modifiers, parse_type_for_decl());
    }
    case '~': {
        auto peek0 = peek_token(0);
        if (cur_struct && peek0.is(Token::Identifier)) {
            auto ident = Identifier::get(peek0.text());
            if (cur_struct->name == ident && peek_token(1).is('(')) {
                // Encountered a destructor!
                next_token(); // Consuming '~' token.
                next_token(); // Consuming the identifier token.
                auto text = peek0.text();
                auto name = Identifier::get(llvm::StringRef(text.data() - 1, text.size() + 1));
                auto destructor = parse_function(0, context.void_type, false, name);
                destructor->is_destructor = true;
                return destructor;
            }
        }
        return parse_expr();
    }
    case Token::KwStruct: {
        return parse_struct();
    }
    case Token::KwEnum: {
        return parse_enum(0);
    }
    case Token::KwCopyobj: {
        auto peek0 = peek_token(0);
        if (peek0.is(Token::Identifier)) {
            auto ident = Identifier::get(peek0.text());
            if (cur_struct && cur_struct->name == ident && peek_token(1).is('(')) {
                next_token(); // Consuming the 'copyobj' token.
                next_token(); // Consuming the identifier token.
                return parse_function(modifiers, context.void_type, false, cur_struct->name, true);
            }
        }

        return parse_assignment_and_expr();
    }
    case Token::KwMoveobj: {
        auto peek0 = peek_token(0);
        if (peek0.is(Token::Identifier)) {
            auto ident = Identifier::get(peek0.text());
            if (cur_struct && cur_struct->name == ident && peek_token(1).is('(')) {
                next_token(); // Consuming the 'moveobj' token.
                next_token(); // Consuming the identifier token.
                return parse_function(modifiers, context.void_type, false, cur_struct->name, false, true);
            }
        }

        return parse_assignment_and_expr();
    }
    case '{':
        return parse_scope();
    case '}': case ',': {
        // Handling these cases as if it is special because the skip recovery.
        // will treat them as recovery points.
        error("Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        next_token();
        skip_recovery();
        return nullptr;
    }
    case ';': {
        next_token();
        return nullptr;
    }
    default: {
        auto stmt = parse_assignment_and_expr();;
        expect(';');
        return stmt;
    }
    }
}

acorn::Node* acorn::Parser::parse_ident_decl_or_expr(bool is_for_expr) {

    Token peek1 = peek_token(0);
    Token peek2 = peek_token(1);

    if (
        (peek1.is(Token::Identifier))    || // ident ident
        (peek1.is('*') && peek2.is('*')) || // ident**
        (peek1.is('*') && peek2.is(Token::Identifier)) ||  // ident* ident
        (peek1.is('*') && peek2.is('[')) || // ident*[
        (peek1.is('$')) ||                  // ident$
        (peek1.is('*') && peek2.is('!')) || // ident*!
        (peek1.is('^'))                     // ident^
        ) {

        if (!is_for_expr) {
            return parse_decl(0, parse_type());
        } else {
            return parse_variable(0, parse_type());
        }
    } else if (peek1.is('[')) {

        Token name_token = cur_token;
        next_token();

        bool is_garenteed_type = false;

        llvm::SmallVector<Expr*, 8> indexes;
        llvm::SmallVector<Token> bracket_tokens;
        while (cur_token.is('[')) {
            if (peek_token(0).is(Token::DotDot)) {
                // Slice type encountered.
                is_garenteed_type = true;
                break;
            }

            ++bracket_count;
            bracket_tokens.push_back(cur_token);
            next_token();
            if (cur_token.is_not(']')) {
                indexes.push_back(parse_expr());
            } else {
                indexes.push_back(nullptr);
                is_garenteed_type = true;
            }
            expect(']');
            --bracket_count;
        }
        if (cur_token.is('*') || cur_token.is('^') || cur_token.is('$')) {
            is_garenteed_type = true;
        }

        if (cur_token.is(Token::Identifier) || is_garenteed_type) {
            // Variable declarations.
            auto type = construct_type_from_identifier(name_token, false);
            type = construct_unresolved_bracket_type(type, indexes);
            type = parse_optional_container_types(type);
            type = parse_optional_function_type(type);
            if (!is_for_expr) {
                return parse_decl(0, type);
            } else {
                return parse_variable(0, type);
            }
        } else {
            // Memory accessing.
            IdentRef* ref = new_node<IdentRef>(name_token);
            auto name = Identifier::get(name_token.text());
            ref->ident = name;

            Expr* site = ref;
            for (size_t i = 0; i < indexes.size(); i++) {
                auto mem_access = new_node<MemoryAccess>(bracket_tokens[i]);
                mem_access->site = site;
                mem_access->index = indexes[i];
                site = mem_access;
            }

            return parse_assignment_and_expr(parse_expr(parse_postfix(site)));
        }
    }

    auto stmt = parse_assignment_and_expr();
    if (!is_for_expr) {
        expect(';');
    }
    return stmt;
}

acorn::Node* acorn::Parser::parse_decl(uint32_t modifiers, Type* type) {
    if (cur_token.is(Token::Identifier)) {
        if (peek_token(0).is('(')) {
            return parse_function(modifiers, type);
        } else {
            auto stmt = parse_variable_list(modifiers, type);
            expect(';');
            return stmt;
        }
    } else if (cur_token.is('^')) {
        next_token();
        return parse_function(modifiers, type, true, expect_identifier());
    } else {
        expect_identifier("for declaration");
        if (cur_token.is('=')) {
            auto stmt = parse_variable(modifiers, type, Identifier());
            expect(';');
            return stmt;
        } else if (cur_token.is('(')) {
            return parse_function(modifiers, type, false, Identifier());
        } else if (cur_token.is_keyword() && peek_token(0).is('=')) {
            next_token(); // Consuming the extra keyword.
            auto stmt = parse_variable(modifiers, type, Identifier());
            expect(';');
            return stmt;
        } else if (cur_token.is_keyword() && peek_token(0).is('(')) {
            next_token(); // Consuming the extra keyword.
            return parse_function(modifiers, type, false, Identifier());
        } else {
            skip_recovery();
        }
        return nullptr;
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
    bool has_implicit_return_ptr = cur_token.is('^');
    if (has_implicit_return_ptr) {
        next_token();
    }
    return parse_function(modifiers, type, has_implicit_return_ptr, expect_identifier());
}

acorn::Func* acorn::Parser::parse_function(uint32_t modifiers,
                                           Type* type,
                                           bool has_implicit_return_ptr,
                                           Identifier name,
                                           bool is_copy_constructor,
                                           bool is_move_constructor) {

    // -- Debug
    // if (name != Identifier::Invalid) {
    //     Logger::debug("parsing function: %s", name);
    // }

    if (has_implicit_return_ptr) {
        type = type_table.get_ptr_type(type);
    }

    Func* func = new_declaration<Func, true>(modifiers, name, prev_token);
    func->return_type = type;
    func->has_implicit_return_ptr = has_implicit_return_ptr;
    func->is_copy_constructor = is_copy_constructor;
    func->is_move_constructor = is_move_constructor;

    Func* prev_func = cur_func;
    cur_func = func;

    // Parsing parameters.
    ++paran_count;
    expect('(');
    if (cur_token.is_not(')') && cur_token.is_not('{')) {
        bool more_params = false, full_reported = false;
        uint32_t param_idx = 0;
        do {
            if (cur_token.is(Token::DotDotDot)) {
                func->uses_native_varargs = true;
                next_token();
                break;
            }

            Type* type = parse_type_for_decl();
            bool has_implicit_ptr = cur_token.is('^');
            if (has_implicit_ptr) {
                next_token(); // Consuming '^' token.
                type = type_table.get_ptr_type(type);
            }
            Var* param = parse_variable(0, type);
            param->param_idx = param_idx++;
            param->has_implicit_ptr = has_implicit_ptr;

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

    --paran_count;

    if (cur_token.is(Token::KwConst)) {
        func->is_constant = true;
        next_token();
    }

    // Parsing the scope of the function.
    if (!func->has_modifier(Modifier::Native) || cur_token.is('{')) {
        if (func->has_modifier(Modifier::Native)) {
            error(cur_token, "Native functions do not have bodies")
                .end_error(ErrCode::ParseNativeFuncNoHaveBodies);
        }
        func->scope = new_node<ScopeStmt>(cur_token);
        expect('{');
        while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
            add_node_to_scope(func->scope, parse_statement());
        }
        expect('}', "for function body");
        func->scope->end_loc = prev_token.loc;
    } else if (func->has_modifier(Modifier::Native)) {
        expect(';');
    }

    cur_func = prev_func;

    return func;
}

acorn::Var* acorn::Parser::parse_variable() {
    return parse_variable(0, parse_type_for_decl());
}

acorn::Var* acorn::Parser::parse_variable(uint32_t modifiers, Type* type) {
    return parse_variable(modifiers, type, expect_identifier("for variable"));
}

acorn::Var* acorn::Parser::parse_variable(uint32_t modifiers, Type* type, Identifier name) {

    Var* var = new_declaration<Var, true>(modifiers, name, prev_token);
    var->parsed_type = type;

    if (cur_token.is('=')) {
        next_token(); // Consume '=' token.

        if (cur_token.is(Token::SubSubSub)) {
            next_token();
            var->should_default_initialize = false;
        } else {
            var->assignment = parse_expr();
        }
    }

    return var;
}

acorn::Node* acorn::Parser::parse_variable_list(uint32_t modifiers, Type* type) {

    auto vlist = &var_list;

    auto name = expect_identifier("for variable");
    auto first_var = parse_variable(modifiers, type, name);

    if (cur_token.is_not(',')) {
        return first_var;
    }

    vlist->list.push_back(first_var);

    while (true) {
        next_token(); // Consuming ',' token

        auto name = expect_identifier("for variable");
        vlist->list.push_back(parse_variable(modifiers, type, name));

        if (cur_token.is_not(',')) {
            break;
        }
    }

    return vlist;
}

acorn::Struct* acorn::Parser::parse_struct() {
    next_token(); // Consuming 'struct' token.
    auto name = expect_identifier("for struct");
    return parse_struct(0, name);
}

acorn::Struct* acorn::Parser::parse_struct(uint32_t modifiers, Identifier name) {

    if (name != Identifier::Invalid) {
        if (auto existing_import = file->find_import(name)) {
            error(prev_token, "Name of struct '%s' conflicts with import", name)
                .add_line([this, existing_import](auto& logger) {
                    auto [line_number, _] =
                        file->line_table.get_line_and_column_number(existing_import->loc.ptr);

                    logger.fmt_print("import defined at line: %s%s%s", Color::BrightYellow, line_number, Color::BrightWhite);
                 })
                .end_error(ErrCode::ParseDataTypeNameConflictWithImport);
        }
    }

    auto structn = new_declaration<Struct, false>(modifiers, name, prev_token);
    structn->nspace = allocator.alloc_type<Namespace>();
    new (structn->nspace) Namespace(modl, ScopeLocation::Struct);
    structn->struct_type = StructType::create(allocator, structn);

    auto prev_struct = cur_struct;
    cur_struct = structn;

    expect('{');
    while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
        Node* node = parse_statement();
        if (!node) continue;

        add_node_to_struct(structn, node);
    }
    expect('}', "for struct body");

    cur_struct = prev_struct;

    return structn;
}

acorn::Enum* acorn::Parser::parse_enum(uint32_t modifiers) {

    next_token(); // Consuming 'enum' token.

    Token name_token = cur_token;
    auto name = expect_identifier();
    Enum* enumn = new_declaration<Enum, false>(modifiers, name, name_token);
    enumn->enum_type = EnumType::create(allocator, enumn);

    if (cur_token.is(Token::ColCol)) {
        next_token(); // Consuming '::' token.
        enumn->enum_type->set_values_type(parse_type());
    }

    expect('{');

    bool more_values = true;
    do {

        Token value_name_token = cur_token;
        Identifier value_name = expect_identifier("for enum value");
        Expr* value_assignment = nullptr;
        if (cur_token.is('=')) {
            next_token();
            value_assignment = parse_expr();
        }

        enumn->values.emplace_back(0, value_name, value_name_token.loc, value_assignment);

        more_values = cur_token.is(',');
        if (more_values) {
            next_token();
            if (cur_token.is('}')) {
                more_values = false;
            }
        }
    } while (more_values);

    expect('}', "for enum");

    return enumn;
}

void acorn::Parser::add_node_to_struct(Struct* structn, Node* node) {

    auto process_var = [structn](Var* var) finline {
        if (var->name != Identifier::Invalid) {
            structn->nspace->add_variable(var);
            structn->fields.push_back(var);
            var->structn = structn;
        }
    };

    if (node->is(NodeKind::Var)) {
        process_var(static_cast<Var*>(node));
    } else if (node->is(NodeKind::VarList)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->list) {
            process_var(var);
        }
        vlist->list.clear();
    } else if (node->is(NodeKind::Func)) {
        auto func = static_cast<Func*>(node);
        if (func->name != Identifier::Invalid) {
            if (func->is_destructor) {
                if (structn->destructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->destructor });
                }

                structn->destructor = func;
                structn->needs_destruction = true;
                func->structn = structn;
            } else if (func->is_copy_constructor) {
                if (structn->copy_constructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->copy_constructor });
                }

                structn->copy_constructor = func;
                func->structn = structn;
                func->is_constructor = true;
                structn->needs_copy_call = true;
            } else if (func->is_move_constructor) {
                if (structn->move_constructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->move_constructor });
                }

                structn->move_constructor = func;
                func->structn = structn;
                func->is_constructor = true;
                structn->needs_move_call = true;
            } else if (func->name == cur_struct->name) {
                func->is_constructor = true;
                if (func->params.empty()) {
                    structn->default_constructor = func;
                }
                structn->constructors.push_back(func);
                func->structn = structn;
            } else {
                structn->nspace->add_function(func);
                func->structn = structn;
            }
            context.add_unchecked_decl(func);
        }
    } else {
        modl.mark_bad_scope(ScopeLocation::Struct, node, logger);
    }
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
        case Token::KwPublic: {
            if (modifiers & Modifier::Public)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Public;

            next_token();
            break;
        }
        case Token::KwPrivate: {
            if (modifiers & Modifier::Private)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Private;

            next_token();
            break;
        }
        case Token::KwReadonly: {
            if (modifiers & Modifier::Readonly)
                error(cur_token, "Duplicate modifier")
                .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Readonly;

            next_token();
            break;
        }
        }
    }
    return modifiers;
}

acorn::ReturnStmt* acorn::Parser::parse_return() {

    ReturnStmt* ret = new_node<ReturnStmt>(cur_token);

    next_token(); // Consuming 'return' keyword.

    if (cur_token.is_not(';')) {
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
    case Token::Identifier: {
        size_t off = cur_token.is(Token::KwConst) ? 1 : 0;

        if (peek_token(off).is('*') && peek_token(off + 1).is(Token::Identifier) && peek_token(off + 2).is('=')) {
            ifs->cond = parse_variable();
            if (cur_token.is(';')) {
                next_token();
                ifs->post_variable_cond = parse_expr();
            }
        } else {
            allow_struct_initializer = false;
            ifs->cond = parse_expr();
            allow_struct_initializer = true;
        }
        break;
    }
    default:
        allow_struct_initializer = false;
        ifs->cond = parse_expr();
        allow_struct_initializer = true;
        break;
    }

    ifs->scope = parse_scope("for if");

    if (cur_token.is(Token::KwElIf)) {
        ifs->elseif = parse_if();
    } else if (cur_token.is(Token::KwElse)) {
        next_token(); // Consuming else token.
        if (cur_token.is(Token::KwIf)) {
            // We do not want the user to use 'else if'.
            SourceLoc error_loc = SourceLoc::from_ptrs(prev_token.loc.ptr,
                                                       cur_token.loc.ptr + cur_token.loc.length);
            error(error_loc, "Must use 'elif' for else if statements")
                .end_error(ErrCode::ParseMustUseElif);
        }
        ifs->elseif = parse_scope();
    }

    return ifs;
}

void acorn::Parser::parse_comptime_if(bool chain_start, bool takes_path) {

    next_token();

    auto cond = parse_expr();

    bool top_took_path = !takes_path;
    if (!context.has_errors()) {
        Sema analyzer(context, file, logger);
        takes_path &= analyzer.check_comptime_cond(cond);
    } else {
        // Give up and do not even try sema because there are parse
        // errors somewhere.
        takes_path = false;
    }

    auto add_statement = [this](Node* stmt) finline {
        if (cur_func) {
            if (stmt->is(NodeKind::VarList)) {
                auto vlist = static_cast<VarList*>(stmt);
                for (auto var : vlist->list) {
                    cur_func->scope->push_back(var);
                }
                vlist->list.clear();
            } else {
                cur_func->scope->push_back(stmt);
            }
        } else if (cur_struct) {
            add_node_to_struct(cur_struct, stmt);
        } else {
            add_node_to_global_scope(stmt);
        }
    };

    bool can_take_else_path = !top_took_path && !takes_path;
    while (cur_token.is_not(Token::KwCTEndIf) &&
           cur_token.is_not(Token::EOB)) {
        if (cur_token.is(Token::KwCTElIf)) {
            parse_comptime_if(false, !takes_path);
            break;
        } else if (cur_token.is(Token::KwCTElse)) {

            next_token();
            while (cur_token.is_not(Token::KwCTEndIf) &&
                   cur_token.is_not(Token::EOB)) {
                if (cur_token.is(Token::KwCTIf)) {
                    parse_comptime_if(true, can_take_else_path);
                } else if (parsing_import_tops && cur_token.is(Token::KwImport)) {
                    if (takes_path) {
                        parse_import_top();
                    } else {
                        parse_import();
                    }
                } else {
                    parsing_import_tops = false;
                    auto node = parse_statement();
                    if (node && can_take_else_path) {
                        add_statement(node);
                    } else if (node && node->is(NodeKind::VarList)) {
                        // We still have to clear the variable list because we called
                         // parse_statement.
                        auto vlist = static_cast<VarList*>(node);
                        vlist->list.clear();
                    }
                }
            }

            break;
        } else {
            if (cur_token.is(Token::KwCTIf)) {
                parse_comptime_if(true, takes_path);
            } else if (parsing_import_tops && cur_token.is(Token::KwImport)) {
                if (takes_path) {
                    parse_import_top();
                } else {
                    parse_import();
                }
            } else {
                parsing_import_tops = false;
                auto node = parse_statement();
                if (node && takes_path) {
                    add_statement(node);
                } else if (node && node->is(NodeKind::VarList)) {
                    // We still have to clear the variable list because we called
                    // parse_statement.
                    auto vlist = static_cast<VarList*>(node);
                    vlist->list.clear();
                }
            }
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
}

acorn::Node* acorn::Parser::parse_loop() {
    Token loop_token = cur_token;
    next_token();

    auto loop_with_var = [this, loop_token](Var* var) finline -> Node* {
        if (var->assignment || !cur_token.is(':')) {
            return parse_range_loop(loop_token, var);
        } else {
            return parse_iterator_loop(loop_token, var);
        }
    };

    switch (cur_token.kind) {
    case TypeTokens: {
        Var* var = parse_variable();
        return loop_with_var(var);
    }
    case Token::Identifier: {
        allow_struct_initializer = false;
        Node* expr = parse_ident_decl_or_expr(true);
        allow_struct_initializer = true;
        if (expr->is(NodeKind::Var)) {
            Var* var = static_cast<Var*>(expr);
            return loop_with_var(var);
        } else {
            auto loop = new_node<PredicateLoopStmt>(loop_token);
            loop->cond = static_cast<Expr*>(expr);
            loop->scope = parse_scope();
            return loop;
        }
    }
    case ';':
        return parse_range_loop(loop_token, nullptr);
    default:
        return parse_predicate_loop(loop_token);
    }
}

acorn::PredicateLoopStmt* acorn::Parser::parse_predicate_loop(Token loop_token) {
    auto loop = new_node<PredicateLoopStmt>(loop_token);

    if (cur_token.is_not('{')) {
        allow_struct_initializer = false;
        loop->cond = parse_expr();
        allow_struct_initializer = true;
    }
    loop->scope = parse_scope();

    return loop;
}

acorn::RangeLoopStmt* acorn::Parser::parse_range_loop(Token loop_token, Node* init_node) {
    auto loop = new_node<RangeLoopStmt>(loop_token);

    if (init_node) {
        loop->init_node = init_node;
    }

    expect(';');

    if (cur_token.is_not(';')) {
        loop->cond = parse_expr();
    }

    expect(';');

    if (cur_token.is_not('{')) {
        allow_struct_initializer = false;
        loop->inc = parse_assignment_and_expr();
        allow_struct_initializer = true;
        loop->scope = parse_scope();
    } else { // loop node; expr; {}
        loop->scope = parse_scope("for loop");
    }

    return loop;
}

acorn::IteratorLoopStmt* acorn::Parser::parse_iterator_loop(Token loop_token, Var* var) {
    auto loop = new_node<IteratorLoopStmt>(loop_token);

    expect(':');

    loop->var = var;

    allow_struct_initializer = false;
    loop->container = parse_expr();
    allow_struct_initializer = true;

    loop->scope = parse_scope("for loop");

    return loop;
}

acorn::LoopControlStmt* acorn::Parser::parse_loop_control() {
    auto loop_control = new_node<LoopControlStmt>(cur_token);
    loop_control->kind = cur_token.is(Token::KwContinue) ? NodeKind::ContinueStmt : NodeKind::BreakStmt;
    next_token(); // Consuming 'break' or 'continue' token.
    return loop_control;
}

acorn::SwitchStmt* acorn::Parser::parse_switch() {
    auto switchn = new_node<SwitchStmt>(cur_token);
    next_token();

    allow_struct_initializer = false;
    switchn->on = parse_expr();
    allow_struct_initializer = true;

    expect('{');

    while (cur_token.is(Token::KwCase)) {
        Token case_token = cur_token;
        next_token();

        Expr* cond = nullptr;
        if (cur_token.is_not(':')) {
            // Default case is when there is no condition.
            cond = parse_expr();
        }

        expect(':');

        // Use the previous token so it associates the scope start with the ':' token.
        ScopeStmt* scope = new_node<ScopeStmt>(prev_token);
        while (cur_token.is_not('}') && cur_token.is_not(Token::KwCase) && cur_token.is_not(Token::EOB)) {
            add_node_to_scope(scope, parse_statement());
        }
        scope->end_loc = prev_token.loc;

        switchn->cases.emplace_back(cond, scope);

        if (!cond) {
            if (switchn->default_scope) {
                error(case_token, "Duplicate default case for switch")
                    .end_error(ErrCode::ParseDuplicateDefaultCaseForSwitch);
            } else {
                switchn->default_scope = scope;
            }
        }
    }

    expect('}', "for switch");

    return switchn;
}

acorn::ScopeStmt* acorn::Parser::parse_scope(const char* closing_for) {

    ScopeStmt* scope = new_node<ScopeStmt>(cur_token);

    if (cur_token.is('{')) {
        next_token(); // Consuming '{' token.

        while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
            add_node_to_scope(scope, parse_statement());
        }

        expect('}', closing_for);
    } else {
        // Single statement scope.
        add_node_to_scope(scope, parse_statement());
    }

    scope->end_loc = prev_token.loc;
    return scope;
}

void acorn::Parser::add_node_to_scope(ScopeStmt* scope, Node* node) {
    if (!node) return;

    if (node->is(NodeKind::VarList)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->list) {
            scope->push_back(var);
        }
        vlist->list.clear();
    } else {
        scope->push_back(node);
    }
}

void acorn::Parser::parse_comptime_file_info() {
    Token start_token = cur_token;
    next_token();

    ++paran_count;
    expect('(');

    if (cur_token.is_not(')')) {
        bool more_args = false;
        int count = 0;
        bool already_set_access = false, already_set_namespace = false;
        do {
            Token ident_token = cur_token;
            Identifier identifier = expect_identifier("for named argument of #file statement");
            if (identifier.get_id() == Identifier::Invalid) {
                skip_recovery(false);
                goto NextCTFileArgLab;
            }

            if (!expect('=', "for named argument of #file statement")) {
                skip_recovery(false);
                goto NextCTFileArgLab;
            }

            if (identifier == context.access_identifier) {
                if (already_set_access) {
                    error(ident_token, "Already set access")
                        .end_error(ErrCode::ParseAlreadySetAccessForComptimefile);
                }

                already_set_access = true;
                if (cur_token.is(Token::KwPublic)) {
                    next_token();
                    file->set_default_access(Modifier::Public);
                } else if (cur_token.is(Token::KwPrivate)) {
                    next_token();
                    file->set_default_access(Modifier::Private);
                } else {
                    error(prev_token, "Expected access modifier")
                        .end_error(ErrCode::ParseExpectedAccessValueForComptimeFile);
                    skip_recovery(false);
                }
            } else if (identifier == context.namespace_identifier) {
                if (already_set_namespace) {
                    error(ident_token, "Already set namespace")
                        .end_error(ErrCode::ParseAlreadySetNamespaceForComptimeFile);
                }

                already_set_namespace = true;
                if (cur_token.is(Token::Identifier)) {
                    Identifier name = Identifier::get(cur_token.text());
                    auto nspace = modl.get_or_create_namespace(context, name);
                    file->set_namespace(nspace);
                    next_token();
                } else {
                    error(prev_token, "Expected identifier for namespace")
                        .end_error(ErrCode::ParseExpectNamespaceIdentForComptimeFile);
                    skip_recovery(false);
                }
            } else {
                error(ident_token, "Expected '%s' argument name", context.access_identifier)
                    .end_error(ErrCode::ParseUknownArgNameForComptimeFile);
                skip_recovery(false);
            }

        NextCTFileArgLab:
            ++count;
            more_args = cur_token.is(',');
            if (more_args) {
                next_token();
            }
        } while (more_args);
    } else {
        error(start_token.loc, "Expected arguments for #file statement")
            .end_error(ErrCode::ParseNoArgsForComptimeFile);
    }

    expect(')');
    --paran_count;
}

// Expression parsing
//--------------------------------------

acorn::Type* acorn::Parser::parse_type_for_decl() {
    if (cur_token.is(Token::KwConst)) {
        if (peek_token(0).is(Token::Identifier)) {
            switch (peek_token(1).kind) {
            case Token::Identifier:
            case '*':
            case '$':
            case '[':
            case '^':
                return parse_type();
            default:
                next_token();
                return context.const_auto_type;
            }
        }

        return parse_type();
    } else {
        return parse_type();
    }
}

acorn::Type* acorn::Parser::parse_type() {
    Token first_token = cur_token;
    auto type = parse_base_type();

    if (type == context.auto_type) {
        if (cur_token.is('*')) {
            next_token();
            return context.auto_ptr_type;
        }
        return type;
    }

    type = parse_optional_container_types(type);
    type = parse_optional_function_type(type);

    if (type->is(context.const_void_type)) {
        SourceLoc error_loc = SourceLoc::from_ptrs(first_token.loc.ptr,
                                                   prev_token.loc.ptr + prev_token.loc.length);

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
        if (cur_token.is('(')) {
            next_token();
            Type* type = parse_type();
            expect(')', "for const type");
            return type_table.get_const_type(type);
        }
    }

#define ty_const(t) \
if (is_const) {     \
return type_table.get_const_type(t); }

#define ty(t) { \
next_token();   \
ty_const(t);    \
return t; }

    switch (cur_token.kind) {
    case Token::KwVoid:    ty(context.void_type);
    case Token::KwInt:     ty(context.int_type);
    case Token::KwInt8:    ty(context.int8_type);
    case Token::KwInt16:   ty(context.int16_type);
    case Token::KwInt32:   ty(context.int32_type);
    case Token::KwInt64:   ty(context.int64_type);
    case Token::KwUInt8:   ty(context.uint8_type);
    case Token::KwUInt16:  ty(context.uint16_type);
    case Token::KwUInt32:  ty(context.uint32_type);
    case Token::KwUInt64:  ty(context.uint64_type);
    case Token::KwBool:    ty(context.bool_type);
    case Token::KwChar:    ty(context.char_type);
    case Token::KwChar16:  ty(context.char16_type);
    case Token::KwChar32:  ty(context.char32_type);
    case Token::KwISize:   ty(context.isize_type);
    case Token::KwUSize:   ty(context.usize_type);
    case Token::KwFloat32: ty(context.float32_type);
    case Token::KwFloat64: ty(context.float64_type);
    case Token::KwAuto: {
        if (is_const) {
            error(prev_token, "Auto cannot be const")
                .add_line("If you want a constant auto type just 'const'")
                .end_error(ErrCode::ParseAutoCannotBeConst);
        }
        next_token();
        return context.auto_type;
    }
    case Token::Identifier: {
        Token name_token = cur_token;
        next_token();
        return construct_type_from_identifier(name_token, is_const);
    }
    default: {
        error("Expected type").end_error(ErrCode::ParseInvalidType);
        return context.invalid_type;
    }
    }

#undef ty_const
#undef ty
}

acorn::Type* acorn::Parser::construct_type_from_identifier(Token name_token, bool is_const) {
    auto name = Identifier::get(name_token.text());
    return UnresolvedCompositeType::create(allocator, name, name_token.loc, is_const);
}

acorn::Type* acorn::Parser::construct_unresolved_bracket_type(Type* base_type,
                                                              const llvm::SmallVector<Expr*, 8>& exprs) {

    Type* type = base_type;

    bool encountered_assign_det_arr_type = false, reported_error_about_elm_must_have_length = false;
    for (auto itr = exprs.rbegin(); itr != exprs.rend(); ++itr) {
        Expr* expr = *itr;
        if (expr == nullptr) {
            type = type_table.get_assigned_det_arr_type(type);

            encountered_assign_det_arr_type = true;
            continue;
        }

        if (encountered_assign_det_arr_type && !reported_error_about_elm_must_have_length) {
            auto loc_ptr = expand(expr).ptr;

            while (*loc_ptr != ']' && *loc_ptr != '\0') {
                ++loc_ptr;
            }
            while (*loc_ptr != '[' && *loc_ptr != '\0') {
                ++loc_ptr;
            }

            acorn_assert(*loc_ptr != '\0', "Failed to find [ character");

            SourceLoc error_loc = {
                .ptr = loc_ptr,
                .length = 1
            };
            error(error_loc, "Element type must have length")
                .end_error(ErrCode::ParseElmTypeMustHaveArrLen);

            reported_error_about_elm_must_have_length = true;
        }

        bool resolvable = expr->is(NodeKind::Number);
        Number* number;
        if (resolvable) {
            number = static_cast<Number*>(expr);
            resolvable &= number->type->is_integer() && number->type->get_number_of_bits() <= 32 &&
                          number->value_s32 > 0;
        }

        if (resolvable) {
            // handle common cases first in which it can resolve the length
            // immediately.
            type = type_table.get_arr_type(type, number->value_s32);
        } else {
            type = UnresolvedBracketType::create(allocator, type, expr);
        }
    }

    return type;
}

acorn::Type* acorn::Parser::parse_optional_function_type(Type* base_type) {
    if (cur_token.is('$')) {
        next_token();
        next_token();
        llvm::SmallVector<Type*> param_types;
        if (cur_token.is_not(')')) {
            bool more_param_types = true;
            while (more_param_types) {

                auto type = parse_type();
                param_types.push_back(type);
                if (type == context.void_type) {
                    error(prev_token, "Parameter type cannot be void")
                        .end_error(ErrCode::ParseFuncParamTypeCannotBeVoid);
                }

                // Allow for an optional name in the parameter type.
                if (cur_token.is(Token::Identifier)) {
                    next_token();
                }

                more_param_types = cur_token.is(',');
                if (more_param_types) {
                    next_token();
                }
            }
        }
        expect(')', "for function type");

        return type_table.get_function_type(base_type, param_types);
    }
    return base_type;
}

acorn::Type* acorn::Parser::parse_optional_container_types(Type* type) {
    while (cur_token.is('*') || cur_token.is('[')) {
        if (cur_token.is('*')) {
            type = type_table.get_ptr_type(type);
            next_token();
        } else if (peek_token(0).is(Token::DotDot)) {
            next_token(); // Consuming '[' token.
            next_token(); // Consuming '..' token.
            expect(']', "for slice type");
            type = type_table.get_slice_type(type);
            continue;
        } else {
            llvm::SmallVector<Expr*, 8> arr_lengths;

            while (cur_token.is('[')) {
                ++bracket_count;
                next_token();

                arr_lengths.push_back(cur_token.is_not(']') ? parse_expr()
                                                            : nullptr);

                expect(']');
                --bracket_count;
            }

            type = construct_unresolved_bracket_type(type, arr_lengths);
        }
    }

    return type;
}

acorn::Expr* acorn::Parser::parse_assignment_and_expr() {
    return parse_assignment_and_expr(parse_expr());
}

acorn::Expr* acorn::Parser::parse_assignment_and_expr(Expr* lhs) {
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
    return parse_expr(parse_postfix());
}

acorn::Expr* acorn::Parser::parse_expr(Expr* lhs) {
    lhs = parse_binary_expr(lhs);

    if (cur_token.is('?')) {
        Ternary* ternary = new_node<Ternary>(cur_token);
        ternary->cond = lhs;
        next_token();
        ternary->lhs = parse_expr();
        expect(':', "for ternary expression");
        ternary->rhs = parse_expr();
        return ternary;
    } else {
        return lhs;
    }
}

std::pair<acorn::Token, acorn::Token> acorn::Parser::split_number_from_sign(Token token) {

    tokkind op_sign = *token.loc.ptr;
    Token op = Token(op_sign, SourceLoc{ token.loc.ptr, 1 });

    token.loc.ptr += 1;
    token.loc.length -= 1;

    return { op, token };
}

acorn::Expr* acorn::Parser::parse_binary_expr(Expr* lhs) {

    struct StackUnit {
        Token op;
        Expr* expr;
    };
    std::stack<StackUnit> op_stack;

    auto get_op = [this]() finline {
        if (cur_token.is_number_literal()) {
            auto text = cur_token.text();
            if (text[0] == '-' || text[0] == '+') {
                auto [new_op, number_token] = split_number_from_sign(cur_token);
                cur_token = new_op;

                // Shift all elements up one since we split the current token.
                for (size_t i = peeked_size; i > 0; i--) {
                    peeked_tokens[i] = peeked_tokens[i - 1];
                }

                peeked_tokens[0] = number_token;
                ++peeked_size;

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
                rhs = unit.expr;
                if (lhs->is(NodeKind::Number) && rhs->is(NodeKind::Number)) {
                    lhs = fold_number(unit.op, rhs, lhs);
                } else {
                    lhs = new_binary_op(unit.op, rhs, lhs);
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
    T lval = static_cast<T>(is_signed ? lhs->value_s64 : lhs->value_u64);
    T rval = static_cast<T>(is_signed ? rhs->value_s64 : rhs->value_u64);

    auto calc = [op, lhs, rhs, to_type](T result) finline {
        // Going to treat the lhs as the result of the evaluation since
        // it is already a number and can just be reused.

        if constexpr (is_signed) {
            lhs->value_s64 = result;
        } else {
            rhs->value_u64 = result;
        }

        lhs->type = to_type;

        // Need to expand the source location because if an error occures
        // later and it needs to display the full error location.
        auto s = lhs->uses_expanded_loc ? lhs->expanded_loc.ptr : lhs->loc.ptr;;
        auto e = rhs->uses_expanded_loc ? (rhs->expanded_loc.ptr + rhs->expanded_loc.length)
                                        : (rhs->loc.ptr + rhs->loc.length);

        lhs->uses_expanded_loc = true;
        lhs->expanded_loc = PointSourceLoc{
            s,
            static_cast<uint16_t>(e - s),
            op.loc.ptr,
            op.loc.length
        };
        return lhs;
    };

    switch (op.kind) {
    case '+': {
        // Note: Even if one of the values is a signed negative value for the unsigned
        //       result case there cannot be negative underflow since the values are
        //       first cast to unsigned integers and what would be negative overflow is
        //       actually then caught as overflow.

        if (rval > 0 && lval > std::numeric_limits<T>::max() - rval) {
            return report_overflow(op, lhs, rhs, to_type);
        }

        if constexpr (is_signed) {
            if (rval < 0 && lval < std::numeric_limits<T>::min() - rval) {
                return report_underflow(op, lhs, rhs, to_type);
            }
        }
        return calc(lval + rval);
    }
    case '-': {
        if constexpr (!is_signed) { // unsigned
            if (lval < rval) {
                return report_underflow(op, lhs, rhs, to_type);
            }
        } else {
            if (rval < 0 && lval > std::numeric_limits<T>::max() + rval) {
                return report_overflow(op, lhs, rhs, to_type);
            }
            if (rval > 0 && lval < std::numeric_limits<T>::min() + rval) {
                return report_underflow(op, lhs, rhs, to_type);
            }
        }
        return calc(lval - rval);
    }
    case '*': {
        if constexpr (!is_signed) { // unsigned
            if (lval != 0 && rval > std::numeric_limits<T>::max() / lval) {
                return report_overflow(op, lhs, rhs, to_type);
            }
        } else {
            if (lhs != 0 && rhs != 0) {
                if (lval > std::numeric_limits<T>::max() / rval) {
                    return report_overflow(op, lhs, rhs, to_type);
                }
                if (lval < std::numeric_limits<T>::min() / rval) {
                    return report_underflow(op, lhs, rhs, to_type);
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

template<typename T>
acorn::Expr* acorn::Parser::fold_float(Token op, Number* lhs, Number* rhs, Type* to_type, T lval, T rval) {

    auto calc = [this, op, to_type, lhs, rhs](T result) finline {
        // Going to treat the lhs as the result of the evaluation since
        // it is already a number and can just be reused.

        if (to_type == context.float32_type) {
            lhs->value_f32 = result;
        } else {
            lhs->value_f64 = result;
        }

        // Need to expand the source location because if an error occures
        // later and it needs to display the full error location.
        lhs->type = to_type;

        auto s = lhs->uses_expanded_loc ? lhs->expanded_loc.ptr : lhs->loc.ptr;;
        auto e = rhs->uses_expanded_loc ? (rhs->expanded_loc.ptr + rhs->expanded_loc.length)
                                        : (rhs->loc.ptr + rhs->loc.length);

        lhs->uses_expanded_loc = true;
        lhs->expanded_loc = PointSourceLoc{
            s,
            static_cast<uint16_t>(e - s),
            op.loc.ptr,
            op.loc.length
        };
        return lhs;
    };


    // We use feclearexcept and fetestexcept because when floating point arithmetic
    // overflow and underflow occure they cause a CPU trap which is handled by the
    // OS.
    //
    // TODO: FE_UNDERFLOW is implementation defined and I am not sure what the best
    // approach to detecting overflow and underflow are so leaving this problem for
    // later.

    switch (op.kind) {
    case '+': {
        //std::feclearexcept(FE_ALL_EXCEPT);

        /*if (std::fetestexcept(FE_OVERFLOW)) {
            return report_overflow(op, lhs, rhs, to_type);
        }*/

        return calc(lval + rval);
    }
    case '-': return calc(lval - rval);
    case '*': return calc(lval * rval);
    case '/': {
        if (rval == 0) {
            // Let sema complain about division by zero.
            return new_binary_op(op, lhs, rhs);
        }

        return calc(lval / rval);
    }
    case '<': return calc(lval < rval);
    case '>': return calc(lval > rval);
    case Token::LtEq: return calc(lval <= rval);
    case Token::GtEq: return calc(lval >= rval);
    default:
        // Handled during sema.
        return new_binary_op(op, lhs, rhs);
    }

}

acorn::Expr* acorn::Parser::report_overflow(Token op, Expr* lhs, Expr* rhs, Type* to_type) {
    auto loc_op = new_binary_op(op, lhs, rhs);
    logger.begin_error(expand(loc_op), "Operator '%s' numeric overflow. Cannot fit for type '%s'",
            token_kind_to_string(context, op.kind), to_type)
        .end_error(ErrCode::NumericOverflow);
    return new_node<InvalidExpr>(op.loc);
}

acorn::Expr* acorn::Parser::report_underflow(Token op, Expr* lhs, Expr* rhs, Type* to_type) {
    auto loc_op = new_binary_op(op, lhs, rhs);
    logger.begin_error(expand(loc_op), "Operator '%s' numeric underflow. Cannot fit into type '%s'",
            token_kind_to_string(context, op.kind), to_type)
        .end_error(ErrCode::NumericUnderflow);
    return new_node<InvalidExpr>(op.loc);
}

acorn::Expr* acorn::Parser::fold_number(Token op, Expr* lhs, Expr* rhs) {

    auto fold_int_by_side = [op, this, lhs, rhs](Expr* side) -> Expr* finline {
        Number* lnum = static_cast<Number*>(lhs);
        Number* rnum = static_cast<Number*>(rhs);
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
                return fold_int_by_side(lhs);
            } else {
                // Just let it complain during sema that the types are incompatible.
                return new_binary_op(op, lhs, rhs);
            }
        } else {
            if (lhs->type->is(context.int_type)) { // rhs explicity type takes preference.
                return fold_int_by_side(rhs);
            } else if (rhs->type->is(context.int_type)) { // lhs explicity type takes preference.
                return fold_int_by_side(lhs);
            } else if (lhs->type->is(rhs->type)) {
                return fold_int_by_side(lhs);
            } else {
                // Just let it complain during sema that the types are incompatible.
                return new_binary_op(op, lhs, rhs);
            }
        }
    } else if (lhs->type->is_float()) {
        Number* lnum = static_cast<Number*>(lhs);
        Number* rnum = static_cast<Number*>(rhs);
        if (lhs->type == rhs->type) {
            if (lhs->type == context.float32_type) {
                return fold_float<float>(op, lnum, rnum, lhs->type, lnum->value_f32, rnum->value_f32);
            } else {
                return fold_float<double>(op, lnum, rnum, lhs->type, lnum->value_f64, rnum->value_f64);
            }
        } else if (rhs->type->is_integer()) {
            if (lhs->type == context.float32_type) {
                float rval = static_cast<float>(rhs->type->is_signed() ? rnum->value_s64 : rnum->value_u64);
                return fold_float<float>(op, lnum, rnum, lhs->type, lnum->value_f32, rval);
            } else {
                double rval = static_cast<double>(rhs->type->is_signed() ? rnum->value_s64 : rnum->value_u64);
                return fold_float<double>(op, lnum, rnum, lhs->type, lnum->value_f64, rval);
            }
        } else {
            // Just let it complain during sema that the types are incompatible.
            return new_binary_op(op, lhs, rhs);
        }
    } else if (rhs->type->is_float()) {
        Number* lnum = static_cast<Number*>(lhs);
        Number* rnum = static_cast<Number*>(rhs);
        if (lhs->type == rhs->type) {
            if (lhs->type == context.float32_type) {
                return fold_float<float>(op, lnum, rnum, lhs->type, lnum->value_f32, rnum->value_f32);
            } else {
                return fold_float<double>(op, lnum, rnum, lhs->type, lnum->value_f64, rnum->value_f64);
            }
        } else if (rhs->type->is_integer()) {
            if (rhs->type == context.float32_type) {
                float lval = static_cast<float>(lhs->type->is_signed() ? lnum->value_s64 : lnum->value_u64);
                return fold_float<float>(op, lnum, rnum, lhs->type, lval, rnum->value_f32);
            } else {
                double lval = static_cast<double>(lhs->type->is_signed() ? lnum->value_s64 : lnum->value_u64);
                return fold_float<double>(op, lnum, rnum, lhs->type, lval, rnum->value_f64);
            }
        } else {
            // Just let it complain during sema that the types are incompatible.
            return new_binary_op(op, lhs, rhs);
        }
    } else {
        acorn_fatal("unreachable");
        return nullptr;
    }
}

acorn::Expr* acorn::Parser::parse_postfix() {
    return parse_postfix(parse_term());
}

acorn::Expr* acorn::Parser::parse_postfix(Expr* term) {
    if (cur_token.is(Token::AddAdd) || cur_token.is(Token::SubSub)) {
        // Language spec. if there is a whitespace then it does not consider it a post
        // inc/dec.
        const char* prev_ch_ptr = cur_token.loc.ptr - 1;
        if (is_whitespace(*prev_ch_ptr)) {
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
    } else if (cur_token.is('(')) {
        return parse_postfix(parse_function_call(term));
    } else if (cur_token.is('.')) {
        return parse_postfix(parse_dot_operator(term));
    } else if (cur_token.is('[')) {
        return parse_postfix(parse_memory_access(term));
    }
    return term;
}

acorn::FuncCall* acorn::Parser::parse_function_call(Expr* site) {

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

                if (call->non_named_args_offset == -1) {
                    call->non_named_args_offset = call->args.size();
                }
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

acorn::Expr* acorn::Parser::parse_memory_access(Expr* site) {

    llvm::SmallVector<Expr*, 8>     indexes;
    llvm::SmallVector<SourceLoc, 8> bracket_locations;

    while (cur_token.is('[')) {
        if (peek_token(0).is(Token::DotDot)) {
            break;
        }

        bracket_locations.push_back(cur_token.loc);
        next_token(); // Consuming '[' token.
        ++bracket_count;

        indexes.push_back(parse_expr());

        expect(']');
        --bracket_count;
    }

    if (site->is(NodeKind::IdentRef)) {
        auto peek0 = peek_token(0);
        auto peek1 = peek_token(1);
       if (peek_if_expr_is_type(cur_token, peek0)) {

            auto ref = static_cast<IdentRef*>(site);

            auto type = UnresolvedCompositeType::create(allocator, ref->ident, ref->loc, false);
            type = construct_unresolved_bracket_type(type, indexes);
            type = parse_optional_container_types(type);
            type = parse_optional_function_type(type);

            TypeExpr* type_expr = new_node<TypeExpr>(cur_token);
            type_expr->expr_type = type;

            SourceLoc start_loc = ref->loc;
            SourceLoc end_loc   = prev_token.loc;

            SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                           end_loc.ptr + end_loc.length);
            type_expr->loc = type_location;
            return type_expr;
        }
    }

    size_t count = 0;
    for (Expr* index : indexes) {
        auto mem_access = new_node<MemoryAccess>(bracket_locations[count]);
        mem_access->site = site;
        mem_access->index = index;

        site = mem_access;
        ++count;
    }

    return site;
}

acorn::Expr* acorn::Parser::parse_term() {
    switch (cur_token.kind) {
    case Token::IntLiteral:           return parse_int_literal();
    case Token::HexLiteral:           return parse_hex_literal();
    case Token::BinLiteral:           return parse_bin_literal();
    case Token::OctLiteral:           return parse_oct_literal();
    case Token::Float32Literal:       return parse_float32_literal();
    case Token::Float64Literal:       return parse_float64_literal();
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
    case '\\': case Token::BackslashBackslash: {
        bool file_local = cur_token.is('\\');
        next_token();

        Token ident_token = cur_token;

        IdentRef* ref = new_node<IdentRef>(cur_token);
        ref->ident = expect_identifier();
        ref->relative_enforcement = file_local ? IdentRef::RelativeEnforcement::File
                                               : IdentRef::RelativeEnforcement::Module;

        if (allow_struct_initializer && cur_token.is('{')) {
            return parse_struct_initializer(ref);
        }

        return ref;
    }
    case Token::Identifier: {

        auto parse_as_type_expr = [this]() finline{
            Token ident_token = cur_token;
            next_token();
            Type* type = construct_type_from_identifier(ident_token, false);
            type = parse_optional_container_types(type);
            type = parse_optional_function_type(type);

            TypeExpr* type_expr = new_node<TypeExpr>(ident_token);
            type_expr->expr_type = type;

            SourceLoc start_loc = ident_token.loc;
            SourceLoc end_loc   = prev_token.loc;

            SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                           end_loc.ptr + end_loc.length);
            type_expr->loc = type_location;
            return type_expr;
        };

        auto peek0 = peek_token(0);
        auto peek1 = peek_token(1);
        if (peek_if_expr_is_type(peek0, peek1)) {
            Token ident_token = cur_token;
            next_token();
            Type* type = construct_type_from_identifier(ident_token, false);
            type = parse_optional_container_types(type);
            type = parse_optional_function_type(type);

            TypeExpr* type_expr = new_node<TypeExpr>(ident_token);
            type_expr->expr_type = type;

            SourceLoc start_loc = ident_token.loc;
            SourceLoc end_loc   = prev_token.loc;

            SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                           end_loc.ptr + end_loc.length);
            type_expr->loc = type_location;
            return type_expr;
        }

        IdentRef* ref = new_node<IdentRef>(cur_token);
        ref->ident = Identifier::get(cur_token.text());

        next_token(); // Consuming the identifier.

        if (allow_struct_initializer && cur_token.is('{')) {
            return parse_struct_initializer(ref);
        }

        return ref;
    }
    case '+': case '-': case '~': case '!':
    case '&': case Token::AddAdd: case Token::SubSub:
    case '*': {
        Token unary_token = cur_token;
        next_token(); // Consuming the unary token.

        bool unary_on_num_literal = cur_token.is(Token::IntLiteral);
        Token after_op_token = cur_token;
        Expr* expr = parse_postfix();

        if (unary_token.kind == '+' && expr->is(NodeKind::Number)) {
            return expr; // + has no effect on value.
        } else if (unary_token.kind == '-' && expr->is(NodeKind::Number)) {
            Number* num = static_cast<Number*>(expr);

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
            case TypeKind::Float32: num->value_f32 = -num->value_f32; break;
            case TypeKind::Float64: num->value_f64 = -num->value_f64; break;
            }

            return expr;
        } else if (unary_token.kind == '~' && expr->is(NodeKind::Number) &&
                   expr->type->is_integer()) {
            Number* num = static_cast<Number*>(expr);

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
        ++paran_count;
        expect('(');
        cast->explicit_cast_type = parse_type();
        expect(')');
        --paran_count;
        cast->value = parse_postfix();
        return cast;
    }
    case Token::KwThis: {
        This* thisn = new_node<This>(cur_token);
        next_token();
        return thisn;
    }
    case Token::KwSizeof: {
        SizeOf* sof = new_node<SizeOf>(cur_token);
        next_token();
        ++paran_count;
        expect('(');
        sof->type_with_size = parse_type();
        expect(')');
        --paran_count;
        return sof;
    }
    case Token::KwMoveobj: {
        MoveObj* move_obj = new_node<MoveObj>(cur_token);
        next_token();
        ++paran_count;
        expect('(');
        move_obj->value = parse_expr();
        expect(')');
        --paran_count;
        return move_obj;
    }
    case TypeTokens: {
        Token start_token = cur_token;
        TypeExpr* type_expr = new_node<TypeExpr>(cur_token);
        type_expr->expr_type = parse_type();

        SourceLoc start_loc = start_token.loc;
        SourceLoc end_loc   = prev_token.loc;

        SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                       end_loc.ptr + end_loc.length);
        type_expr->loc = type_location;
        return type_expr;
    }
    case Token::KwCTReflect: {
        Reflect* reflect = new_node<Reflect>(cur_token);
        reflect->reflect_kind = context.get_reflect_kind(cur_token.text());
        next_token();
        ++paran_count;
        expect('(');
        reflect->expr = parse_expr();
        expect(')');
        --paran_count;
        return reflect;
    }
    default:
        error("Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        skip_recovery();
        return new_node<InvalidExpr>(cur_token);
    }
}

acorn::Number* acorn::Parser::parse_int_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    auto number = parse_number_literal<10, void_table, false>(text.data(), text.end());
    char last_char = text.back();
    if (last_char == 'f') {
        number->uses_strict_type = true;
        number->type = context.float32_type;
    } else if (last_char == 'd') {
        number->uses_strict_type = true;
        number->type = context.float64_type;
    }
    return number;
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

acorn::Number* acorn::Parser::parse_hex_literal() {
    const auto text = cur_token.text();
    return parse_number_literal<16, hex_table>(text.data() + 2, text.end());
}

acorn::Number* acorn::Parser::parse_bin_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_number_literal<2, void_table, false>(text.data() + 2, text.end());
}

acorn::Number* acorn::Parser::parse_oct_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_number_literal<8, void_table, false>(text.data() + 1, text.end());
}

acorn::Number* acorn::Parser::parse_float32_literal() {

    auto [value, parse_error] = parse_float32_bits(allocator, cur_token.text());

    Number* number = new_node<Number>(cur_token);
    number->value_f32 = value;
    number->type = context.float32_type;

    if (parse_error != FloatParseError::None) {
        report_float_error(parse_error);
    }

    next_token();
    return number;
}

acorn::Number* acorn::Parser::parse_float64_literal() {

    auto [value, parse_error] = parse_float64_bits(allocator, cur_token.text());

    Number* number = new_node<Number>(cur_token);
    number->value_f64 = value;
    number->type = context.float64_type;

    if (parse_error != FloatParseError::None) {
        report_float_error(parse_error);
    }

    next_token();
    return number;
}

void acorn::Parser::report_float_error(FloatParseError parse_error) {
    if (parse_error == FloatParseError::Overflow) {
        error(cur_token, "Float value too large")
            .end_error(ErrCode::NumericOverflow);
    } else {
        error(cur_token, "Float value too small")
            .end_error(ErrCode::NumericUnderflow);
    }
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
        case '0':  return '\0';
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
        character->value_u64 = get_escape_char(*(ptr + 1));
        character->type = context.char_type;
    } else {
        character->value_u64 = *ptr;
        character->type = context.char_type;
    }

    next_token();
    return character;
}

template<uint32_t radix, uint64_t convert_table[256], bool use_table>
acorn::Number* acorn::Parser::parse_number_literal(const char* start, const char* end) {

    const char* ptr = start;
    bool neg_sign = *ptr == '-';
    if (*ptr == '-' || *ptr == '+') {
        ++ptr;
    }

    bool already_errored = false;
    uint64_t value = 0, prev_value;
    while (ptr != end) {
        char c = *ptr;

        if (c == NUMBER_SEPERATOR) {
            ++ptr;
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
        number->uses_strict_type = true;
        ++ptr;
        bool is_signed = *ptr == 'i';
        ++ptr;

        auto report_fits_error = [this, number]() finline {
            auto err_msg = get_error_msg_for_value_not_fit_type(number);
            logger.begin_error(number->loc, "%s", err_msg)
                .end_error(ErrCode::ParseIntegerValueNotFitType);
        };

        auto check_range_fit = [report_fits_error, number, neg_sign, already_errored]<typename T>(T) finline {
            if (already_errored) {
                return;
            }

            if constexpr (std::is_unsigned_v<T>) {
                if (neg_sign) {
                    using SignedT = std::make_signed_t<T>;
                    if (!fits_in_range<SignedT>(number->value_s64)) {
                        report_fits_error();
                        return;
                    }

                    number->value_u64 = (T) (SignedT) number->value_s64;
                    return;
                }

                if (!fits_in_range<T>(number->value_u64)) {
                    report_fits_error();
                }
            } else {
                if (!fits_in_range<T>(number->value_s64)) {
                    report_fits_error();
                }
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
            if (is_signed) number->type = context.int16_type, check_range_fit((int16_t)0);
            else           number->type = context.uint16_type, check_range_fit((uint16_t)0);
        } else if (*ptr == '3') {
            if (is_signed) number->type = context.int32_type, check_range_fit((int32_t)0);
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

    ++bracket_count;
    next_token(); // Consuming '[' token.

    bool more_values = false;
    if (cur_token.is_not(']')) {
        do {

            bool uses_assigned_index = false;
            if (cur_token.is('[')) {
                switch (peek_token(0).kind) {
                case Token::IntLiteral:
                case Token::BinLiteral:
                case Token::HexLiteral:
                case Token::OctLiteral:
                case Token::CharLiteral:
                    if (peek_token(1).is(']') && peek_token(2).is('=')) {
                        uses_assigned_index = true;

                        next_token(); // Consume [

                        Token number_token = cur_token;

                        auto number = static_cast<Number*>(parse_expr());

                        next_token(); // Consume ]
                        next_token(); // Consume =
                        auto expr = parse_expr();

                        if (number_token.text()[0] != '-') {

                            // TODO: deal with value possibly being larger.
                            uint32_t index = number->value_u32;

                            if (index < arr->elms.size()) {
                                if (arr->elms[index]) {
                                    error(number_token, "Array index %s already assigned", index)
                                        .end_error(ErrCode::ParseArrayIndexAlreadyAssigned);
                                } else {
                                    arr->elms[index] = expr;
                                }
                            } else {
                                for (size_t i = arr->elms.size(); i < index; i++) {
                                    arr->elms.push_back(nullptr);
                                }
                                arr->elms.push_back(expr);
                            }
                        } else {
                            error("Array index cannot be negative")
                                .end_error(ErrCode::ParseArrayIndexCannotBeNeg);
                        }
                    }
                    break;
                }
            }

            if (!uses_assigned_index) {
                arr->elms.push_back(parse_expr());
            }

            more_values = cur_token.is(',');
            if (more_values) {
                next_token();
                if (cur_token.is(']')) {
                    more_values = false;
                }
            }
        } while (more_values);
    }

    expect(']', "for array");
    --bracket_count;
    return arr;
}

acorn::Expr* acorn::Parser::parse_struct_initializer(IdentRef* ref) {
    auto initializer = new_node<StructInitializer>(cur_token);
    initializer->ref = ref;

    next_token();

    if (cur_token.is_not('}')) {
        bool more_values = false;
        do {
            Expr* value;
            if (cur_token.is(Token::Identifier) && peek_token(0).is('=')) {

                auto named_value = new_node<NamedValue>(cur_token);
                named_value->name = Identifier::get(cur_token.text());
                value = named_value;
                next_token(); // Consuming identifier token.
                next_token(); // Consuming '=' token.

                named_value->assignment = parse_expr();

                if (initializer->non_named_vals_offset == -1) {
                    initializer->non_named_vals_offset = initializer->values.size();
                }
            } else {
                value = parse_expr();
            }

            initializer->values.push_back(value);

            more_values = cur_token.is(',');
            if (more_values) {
                next_token();
                if (cur_token.is('}')) {
                    more_values = false;
                }
            }
        } while (more_values);
    }

    expect('}', "for struct initializer");

    return initializer;
}

// Utility functions
//--------------------------------------

void acorn::Parser::next_token() {
    prev_token = cur_token;
    if (peeked_size > 0) {
        cur_token = peeked_tokens[0];
        std::rotate(peeked_tokens, peeked_tokens + 1, peeked_tokens + peeked_size);
        --peeked_size;
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

bool acorn::Parser::expect(tokkind kind, const char* for_msg) {
    if (cur_token.is(kind)) {
        next_token();
        return true;
    } else {
        const std::string str_kind = token_kind_to_string(context, kind);
        const std::string arrow_msg = std::format("add '{}' here", str_kind);
        const bool is_closing = kind == ')' || kind == '}';
        const auto closing_msg = is_closing ? " closing" : "";
        auto fixed_for_msg = for_msg ? std::string{ " " } + for_msg : "";

        logger.begin_error(prev_token.loc, "Expected%s '%s' token%s", closing_msg, str_kind, fixed_for_msg)
              .add_arrow_msg(Logger::ArrowPosition::After, arrow_msg)
              .end_error(ErrCode::ParseExpect);
        return false;
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

bool acorn::Parser::peek_if_expr_is_type(Token tok0, Token tok1) {
    if (tok0.is('*')) {
        switch (tok1.kind) {
        case ')':
        case '[': // This leads to less than ideal parsing and makes the assumption that there
                    // cannot be multiplication by arrays.
        case ']':
        case ';':
        case '*':
        case '$':
            return true;
        default:
            break;
        }
    } else if (tok0.is('$')) {
        return true;
    } else if (tok0.is('[') && tok1.is(Token::DotDot)) {
        return true;
    }
    return false;
}

acorn::Expr* acorn::Parser::new_binary_op(Token op_tok, Expr* lhs, Expr* rhs) {
    BinOp* bin_op = new_node<BinOp>(op_tok);
    bin_op->op = op_tok.kind;
    bin_op->lhs = lhs;
    bin_op->rhs = rhs;
    return bin_op;
};

void acorn::Parser::skip_recovery(bool stop_on_modifiers) {
    while (true) {
        switch (cur_token.kind) {
        case Token::EOB:
        case '{':
        case '}':
        case ';':
        case ',':
            return;
        case ')': {
            if (paran_count > 0)
                return;
            next_token();
            break;
        }
        case ']': {
            if (bracket_count > 0)
                return;
            next_token();
            break;
        }
        case ModifierTokens: {
            if (stop_on_modifiers)
                return;
            next_token();
            break;
        }
        case Token::Identifier: {
            if (peek_token(0).is('=')) {
                return;
            }
            next_token();
            break;
        }
        case Token::KwImport:
        case Token::KwIf:
        case Token::KwReturn:
        case Token::KwCTIf:
        case Token::KwStruct:
        case Token::KwEnum:
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