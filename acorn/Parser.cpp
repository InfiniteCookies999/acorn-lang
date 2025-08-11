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
     Token::KW_VOID:   \
case Token::KW_INT:    \
case Token::KW_INT8:   \
case Token::KW_INT16:  \
case Token::KW_INT32:  \
case Token::KW_INT64:  \
case Token::KW_UINT8:  \
case Token::KW_UINT16: \
case Token::KW_UINT32: \
case Token::KW_UINT64: \
case Token::KW_FLOAT:  \
case Token::KW_DOUBLE: \
case Token::KW_CONST:  \
case Token::KW_CHAR:   \
case Token::KW_CHAR16: \
case Token::KW_USIZE:  \
case Token::KW_ISIZE:  \
case Token::KW_BOOL

#define ModifierTokens    \
     Token::KW_NATIVE:    \
case Token::KW_DLLIMPORT: \
case Token::KW_PUBLIC:    \
case Token::KW_PRIVATE:   \
case Token::KW_READONLY

acorn::Parser::Parser(Context& context, Module& modl, SourceFile* file)
    : context(context)
    , modl(modl)
    , allocator(context.get_allocator())
    , file(file)
    , logger(file->logger)
    , lexer(context, file->buffer, logger)
    , type_table(context.type_table) {
}

void acorn::Parser::parse() {

    // Prime the parser.
    next_token();

    // Have to set the previous token because the
    // current token does not exist.
    prev_token = cur_token;


    if (cur_token.is(Token::KW_CT_FILE)) {
        parse_comptime_file_info();
    }

    // Parsing imports.
    while (parsing_import_tops &&
           (cur_token.is(Token::KW_IMPORT) || cur_token.is(Token::KW_CT_IF))) {
        if (cur_token.is(Token::KW_IMPORT)) {
            parse_import_top();
        } else {
            parse_comptime_if();
        }
    }
    parsing_import_tops = false;

    while (cur_token.is_not(Token::EOB)) {
        Node* node = parse_statement();
        if (!node) continue;

        add_node_to_global_scope(node);
    }

}

// Statement processing
//--------------------------------------

template<typename D, bool uses_linkname>
D* acorn::Parser::new_declaration(uint32_t modifiers, Token loc_token) {
    D* decl = new_node<D>(loc_token);
    decl->file      = file;
    decl->modifiers = modifiers;
    if constexpr (uses_linkname) {
        decl->linkname = linkname;
    }
    linkname = "";
    return decl;
}

void acorn::Parser::add_node_to_global_scope(Node* node) {

    auto process_variable = [this](Var* var) finline {
        if (var->name != Identifier::Invalid) {
            context.add_unchecked_decl(var);
            file->add_variable(var);
        }
    };

    if (node->is(NodeKind::FUNC)) {

        auto func = static_cast<Func*>(node);

        if (func->is_constructor || func->is_destructor) {
            modl.mark_bad_scope(ScopeLocation::GLOBAL, node, logger);
            return;
        }

        if (func->name != Identifier::Invalid) {
            if (!func->is_generic()) {
                context.add_unchecked_decl(func);
            }
            file->add_function(func);
        }

        if (func->name == context.main_identifier) {
            context.add_canidate_main_function(func);
        }
    } else if (node->is(NodeKind::STRUCT)) {

        auto structn = static_cast<Struct*>(node);
        if (structn->name != Identifier::Invalid) {
            if (!structn->is_generic) {
                context.add_unchecked_decl(structn);
            }
            file->add_composite(structn);
        }
    } else if (node->is(NodeKind::ENUM) || node->is(NodeKind::INTERFACE)) {

        auto composite = static_cast<Decl*>(node);
        if (composite->name != Identifier::Invalid) {
            context.add_unchecked_decl(composite);
            file->add_composite(composite);
        }
    } else if (node->is(NodeKind::VAR)) {
        process_variable(static_cast<Var*>(node));
    } else if (node->is(NodeKind::VAR_LIST)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->vars) {
            process_variable(var);
        }
        vlist->vars.clear();
    } else {
        modl.mark_bad_scope(ScopeLocation::GLOBAL, node, logger);
    }
}

void acorn::Parser::add_node_to_struct(Struct* structn, Node* node) {

    auto process_var = [structn](Var* var) finline {
        if (var->name != Identifier::Invalid) {
            structn->nspace->add_variable(var);
            var->field_idx = static_cast<uint32_t>(structn->fields.size());
            structn->fields.push_back(var);
            var->structn = structn;
            var->non_generic_struct_instance = structn;
        }
    };

    if (node->is(NodeKind::VAR)) {
        process_var(static_cast<Var*>(node));
    } else if (node->is(NodeKind::VAR_LIST)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->vars) {
            process_var(var);
        }
        vlist->vars.clear();
    } else if (node->is(NodeKind::FUNC)) {
        auto func = static_cast<Func*>(node);
        if (func->name != Identifier::Invalid) {
            func->structn = structn;
            func->non_generic_struct_instance = structn;

            if (func->is_destructor) {
                if (structn->destructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->destructor });
                }

                structn->destructor = func;
                structn->needs_destruction = true;
            } else if (func->is_copy_constructor) {
                if (structn->copy_constructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->copy_constructor });
                }

                structn->copy_constructor = func;
                structn->needs_copy_call = true;
            } else if (func->is_move_constructor) {
                if (structn->move_constructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->move_constructor });
                }

                structn->move_constructor = func;
                structn->needs_move_call = true;
            } else if (func->is_constructor) {
                if (func->params.empty()) {
                    structn->default_constructor = func;
                }
                structn->constructors.push_back(func);
            } else {
                structn->nspace->add_function(func);
            }
            if (!func->is_generic()) {
                context.add_unchecked_decl(func);
            }
        }
    } else {
        modl.mark_bad_scope(ScopeLocation::STRUCT, node, logger);
    }
}

void acorn::Parser::add_node_to_interface(Interface* interfacen, Node* node) {
    if (node->is(NodeKind::FUNC)) {
        auto func = static_cast<Func*>(node);
        func->interfacen = interfacen;
        func->interface_idx = cur_interface->functions.size();
        cur_interface->functions.push_back(func);
    } else {
        modl.mark_bad_scope(ScopeLocation::INTERFACE, node, logger);
    }

}

void acorn::Parser::add_node_to_scope(ScopeStmt* scope, Node* node) {
    if (!node) return;

    if (node->is(NodeKind::VAR_LIST)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->vars) {
            scope->push_back(var);
        }
        vlist->vars.clear();
    } else {
        scope->push_back(node);
    }
}

// Statement parsing
//--------------------------------------

void acorn::Parser::parse_comptime_file_info() {
    Token start_token = cur_token;
    next_token();

    ++paren_count;
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
                if (cur_token.is(Token::KW_PUBLIC)) {
                    next_token();
                    file->set_default_access(Modifier::Public);
                } else if (cur_token.is(Token::KW_PRIVATE)) {
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
                if (cur_token.is(Token::IDENTIFIER)) {
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
        error(start_token.get_location(), "Expected arguments for #file statement")
            .end_error(ErrCode::ParseNoArgsForComptimeFile);
    }

    expect(')');
    --paren_count;
}

void acorn::Parser::parse_import_top() {
    auto importn = parse_import();
    if (!importn) return;

    if (auto prev_import = file->try_add_import(importn)) {
        const auto& last_key_part = importn->key.back();
        auto name = last_key_part.name.to_string();
        logger.begin_error(importn->get_key_location(true), "Duplicate import of '%s'", name)
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
    } else if (cur_token.is(Token::DOT_DOT)) {
        next_token();
        importn->within_parent_modl = true;
    }

    Token start_token = cur_token;

    bool more_to_import = false;
    do {

        if (cur_token.is_not(Token::IDENTIFIER)) {
            error(cur_token, "Expected identifier for import location")
                .end_error(ErrCode::ParseExpectedImportIdentifier);
            skip_recovery();
            return nullptr;
        }
        Token ident_token = cur_token;
        next_token(); // Consuming the identifier.

        importn->key.push_back({ Identifier::get(ident_token.text()), ident_token.get_location()});

        more_to_import = cur_token.is('.');
        if (more_to_import) {
            next_token();
        }
    } while (more_to_import);

    if (cur_token.is(Token::KW_STATIC)) {
        next_token();
        importn->is_static = true;
    }

    expect(';');

    return importn;
}

acorn::Node* acorn::Parser::parse_statement() {
    switch (cur_token.kind) {
    case ModifierTokens: {
        uint32_t modifiers = parse_modifiers();
        if (cur_token.is(Token::KW_FN)) {
            return parse_function(modifiers, false);
        } else if (cur_token.is(Token::KW_CONST) && peek_token(0).is(Token::KW_FN)) {
            next_token(); // Consuming 'const' token.
            return parse_function(modifiers, true);
        } else if (cur_token.is(Token::IDENTIFIER)) {
            return parse_variable_list(modifiers);
        } else if (Token::KW_STRUCT) {
            return parse_struct(modifiers);
        } else if (Token::KW_ENUM) {
            return parse_enum(modifiers);
        } else if (Token::KW_INTERFACE) {
            return parse_interface(modifiers);
        } else {
            error(cur_token, "Expected declaration")
                .end_error(ErrCode::ParseExpectedDeclaration);
            skip_recovery();
            return nullptr;
        }
    }
    case Token::KW_FN:
        return parse_function(0, false);
    case Token::KW_GENERICS: {
        size_t prev_size = cur_generics.size();
        parse_generics(cur_generics);

        Node* node = nullptr;
        uint32_t modifiers = parse_modifiers();
        if (cur_token.is(Token::KW_FN)) {
            node = parse_function(modifiers, false);
        } else if (cur_token.is(Token::KW_CONST) && peek_token(0).is(Token::KW_FN)) {
            next_token(); // Consuming 'const' token.
            node = parse_function(modifiers, true);
        } else if (cur_token.is(Token::KW_STRUCT)) {
            node = parse_struct(modifiers);
        } else {
            error(cur_token, "Expected declaration")
                .end_error(ErrCode::ParseExpectedDeclaration);
            skip_recovery();
        }

        // Remove old ones.
        cur_generics.resize(prev_size);
        return node;
    }
    case Token::KW_CONST: {
        if (peek_token(0).is(Token::KW_FN)) {
            next_token(); // Consuming 'const' token.
            return parse_function(0, true);
        } else {
            Expr* expr = parse_assignment_and_expr();
            expect(';');
            return expr;
        }
    }
    case Token::KW_STRUCT:
        return parse_struct(0);
    case Token::KW_ENUM:
        return parse_enum(0);
    case Token::KW_INTERFACE:
        return parse_interface(0);
    case Token::IDENTIFIER: {
        auto peek0 = peek_token(0);
        if (peek0.is(':')) {
            Var* var = parse_variable(0);
            expect(';');
            return var;
        } else if (peek0.is(',')) {
            return parse_variable_list(0);
        } else if (peek0.is('*') && peek_token(1).is(':')) {
            Var* var = parse_variable(0);
            expect(';');
            return var;
        } else if (peek0.is('*') && peek_token(1).is(',')) {
            return parse_variable_list(0);
        } else {
            Expr* expr = parse_assignment_and_expr();
            expect(';');
            return expr;
        }
    }
    case Token::KW_RETURN: {
        auto ret = parse_return();
        expect(';');
        return ret;
    }
    case Token::KW_IF:
        return parse_if();
    case Token::KW_CT_IF: {
        parse_comptime_if();
        return nullptr;
    }
    case Token::KW_LOOP:
        return parse_loop();
    case Token::KW_BREAK: case Token::KW_CONTINUE:
        return parse_loop_control();
    case Token::KW_SWITCH:
        return parse_switch();
    case Token::KW_TRY: {
        auto stmt = parse_try();
        expect(';');
        return stmt;
    }
    case Token::KW_RAISE: {
        auto stmt = parse_raise();
        expect(';');
        return stmt;
    }
    case Token::KW_RECOVER: {
        auto stmt = parse_recover();
        expect(';');
        return stmt;
    }
    case Token::KW_CT_ABORTS: {
        next_token(); // Consuming '#aborts' token.
        auto structn = parse_struct(0);
        structn->aborts_error = true;
        return structn;
    }
    case Token::KW_UNINIT_NEW: {
        auto new_call = new_node<UninitNewCallStmt>(cur_token);
        next_token();
        ++paren_count;
        expect('(');
        new_call->address = parse_expr();
        expect(',');
        new_call->value = parse_expr();
        expect(')');
        --paren_count;
        return new_call;
    }
    case Token::KW_DELETE: {
        auto delete_call = new_node<DeleteCallStmt>(cur_token);
        next_token();
        ++paren_count;
        expect('(');
        delete_call->address = parse_expr();
        expect(')');
        --paren_count;
        return delete_call;
    }
    case '{':
        return parse_scope();
    case ';': {
        next_token();
        return nullptr;
    }
    case '}': case ',': {
        // Handling these cases as if it is special because the skip recovery.
        // will treat them as recovery points.
        error(cur_token, "Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        next_token();
        skip_recovery();
        return nullptr;
    }
    case '.': {
        error(cur_token, "Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        next_token();
        skip_recovery();
        return nullptr;
    }
    default: {
        Expr* stmt = parse_assignment_and_expr();
        expect(';');
        return stmt;
    }
    }
}

acorn::Func* acorn::Parser::parse_function(uint32_t modifiers, bool is_const) {

    expect(Token::KW_FN);

    Func* func = new_declaration<Func, true>(modifiers, cur_token);
    func->is_constant = is_const;
    func->generics    = cur_generics;

    // Check for constructor.
    switch (cur_token.kind) {
    case Token::KW_NEW:
        func->name = context.new_identifier;
        func->is_constructor = true;
        next_token();
        break;
    case Token::KW_COPYOBJ:
        func->name = context.copyobj_identifier;
        func->is_copy_constructor = true;
        func->is_constructor = true;
        next_token();
        break;
    case Token::KW_MOVEOBJ:
        func->name = context.moveobj_identifier;
        func->is_move_constructor = true;
        func->is_constructor = true;
        next_token();
        break;
    case Token::KW_DELETE:
        func->name = context.delete_identifier;
        func->is_destructor = true;
        next_token();
        break;
    default:
        func->name = expect_identifier("for function declaration");
        break;
    }

    Func* prev_func = cur_func;
    cur_func = func;

    // Parsing parameters.
    size_t num_default_params = 0;
    ++paren_count;
    expect('(');
    if (cur_token.is_not(')') && cur_token.is_not('{')) {
        bool more_params = false, full_reported = false;
        uint32_t param_idx = 0;
        do {
            if (cur_token.is(Token::DOT_DOT_DOT)) {
                func->uses_native_varargs = true;
                next_token();
                break;
            }

            Var* param = parse_variable(0, true);

            param->param_idx = param_idx++;
            if (param->assignment) {
                ++num_default_params;
            } else {
                if (cur_token.is(Token::DOT_DOT_DOT)) {
                    func->uses_varargs = true;
                    param->parsed_type = type_table.get_slice_type(param->parsed_type);
                    next_token();
                }
            }

            if (func->params.size() != MAX_FUNC_PARAMS) {
                func->params.push_back(param);
            } else if (!full_reported) {
                // TODO: The parameter might not refer to a location correctly.
                error(param->loc,
                      "Exceeded maximum number of function parameters. Max: %s", MAX_FUNC_PARAMS)
                    .end_error(ErrCode::ParseExceededMaxFuncParams);
                full_reported = true;
            }

            more_params = cur_token.is(',');
            if (more_params) {
                next_token();
            }
        } while (more_params);
    }
    expect(')');
    --paren_count;

    if (num_default_params != 0) {
        func->default_params_offset = func->params.size() - num_default_params;
    }

    // Parsing return type.
    if (cur_token.is_not('{') && cur_token.is_not(';') && cur_token.is_not('|') && cur_token.is_not(Token::KW_RAISES)) {
        expect(Token::ARROW);
        func->parsed_return_type = parse_type();
    } else {
        func->parsed_return_type = context.void_type;
    }
    if (cur_token.is('^')) {
        next_token();
        if (func->parsed_return_type) {
            func->parsed_return_type = type_table.get_ptr_type(func->parsed_return_type);
        }
        func->has_implicit_return_ptr = true;
    }


    if (cur_token.is('|') || cur_token.is(Token::KW_RAISES)) {
        expect('|', "for raised errors");
        expect(Token::KW_RAISES);
        parse_raised_errors(func->raised_errors);
    }

    bool expects_body = !(func->has_modifier(Modifier::Native) || cur_interface);
    if (expects_body || cur_token.is('{')) {

        if (func->has_modifier(Modifier::Native)) {
            error(cur_token, "Native functions should not have bodies")
                .end_error(ErrCode::ParseNativeFuncNoHaveBodies);
        } else if (cur_interface) {
            error(cur_token, "Interface functions should not have bodies")
                .end_error(ErrCode::ParseInterfaceFuncNoHaveBodies);
        }

        // Parsing scope.
        func->scope = new_node<ScopeStmt>(cur_token);
        expect('{');
        while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
            add_node_to_scope(func->scope, parse_statement());
        }
        expect('}', "for function body");
        func->scope->end_loc = prev_token.get_location();
    } else {
        expect(';');
    }

    cur_func = prev_func;
    return func;
}

acorn::VarList* acorn::Parser::parse_variable_list(uint32_t modifiers) {
    return parse_variable_list(modifiers, &var_list);
}

acorn::VarList* acorn::Parser::parse_variable_list(uint32_t modifiers, VarList* var_list) {

    llvm::SmallVector<std::tuple<Token, bool, Token>> name_tokens;
    bool more_variables = false;
    do {

        Token name_token = cur_token;
        expect_identifier("for variable declaration");

        if (cur_token.is('*')) {
            name_tokens.push_back({ name_token, true, cur_token });
            next_token();
        } else {
            name_tokens.push_back({ name_token, false, {} });
        }

        more_variables = cur_token.is(',');
        if (more_variables) {
            next_token();
        }
    } while (more_variables);

    Type* type;
    expect(':');
    if (cur_token.is('=')) {
        type = context.auto_type;
    } else if (cur_token.is(':')) {
        type = context.const_auto_type;
    } else {
        type = parse_type();
    }

    for (auto& [name_token, ignore1, ignore2] : name_tokens) {
        Var* var = new_declaration<Var, true>(modifiers, name_token);
        if (name_token.is(Token::IDENTIFIER)) {
            var->name = Identifier::get(name_token.text());
        }
        var->parsed_type = type;
        var_list->vars.push_back(var);
    }

    if (cur_token.is('=') || cur_token.is(':')) {
        next_token();

        llvm::SmallVector<Expr*, 4> assignments;
        bool more_expressions = false;
        do {

            Expr* assignment = parse_expr();
            assignments.push_back(assignment);

            more_expressions = cur_token.is(',');
            if (more_expressions) {
                next_token();
            }
        } while (more_expressions);


        if (assignments.size() > var_list->vars.size()) {
            if (!context.has_errors()) {
                Expr* first_extra_assignment = assignments[var_list->vars.size()];
                logger.begin_error(expand(first_extra_assignment), "Too many assignments for the number of variables")
                    .end_error(ErrCode::ParseTooManyAssignmentsForNumberOfVariables);
            }
        } else if (assignments.size() != var_list->vars.size()) {

            // TODO (maddie): this is where we would check if the number of assignments is one,
            // then allow for decomposition if so.

            if (!context.has_errors()) {
                auto last_assignment_loc  = expand(assignments.back());
                auto first_assignment_loc = expand(assignments.front());

                auto error_loc = SourceLoc::from_ptrs(first_assignment_loc.ptr, last_assignment_loc.end());
                error(error_loc, "Too few assignments for the number of variables")
                    .end_error(ErrCode::ParseTooFewAssignmentsForNumberOfVariables);
            }
        } else {
            for (size_t i = 0; i < var_list->vars.size(); i++) {
                auto assignment = assignments[i];
                Var* var = var_list->vars[i];
                var->assignment = assignment;

                bool infers_ptr_type = std::get<1>(name_tokens[i]);
                Token auto_ptr_type  = std::get<2>(name_tokens[i]);
                check_for_auto_ptr_with_type(var, infers_ptr_type, auto_ptr_type);

                if (infers_ptr_type) {
                    if (var->parsed_type == context.auto_type) {
                        var->parsed_type = context.auto_ptr_type;
                    } else {
                        var->parsed_type = context.auto_const_ptr_type;
                    }
                }
            }
        }
    }

    return var_list;
}

acorn::Var* acorn::Parser::parse_variable(uint32_t modifiers, bool may_parse_implicit_ptr) {

    Var* var = new_declaration<Var, true>(modifiers, cur_token);
    var->name = expect_identifier("for variable declaration");

    bool infers_ptr_type = false;
    Token auto_ptr_token;
    if (cur_token.is('*')) {
        auto_ptr_token = cur_token;
        infers_ptr_type = true;
        next_token();
    }

    expect(':');
    if (cur_token.is('=')) {
        var->parsed_type = context.auto_type;
    } else if (cur_token.is(':')) {
        var->parsed_type = context.const_auto_type;
    } else {
        var->parsed_type = parse_type();
        if (may_parse_implicit_ptr && cur_token.is('^')) {
            next_token(); // Consuming '^' token.
            if (var->parsed_type) { // Check to make sure it parsed.
                var->parsed_type = type_table.get_ptr_type(var->parsed_type);
                var->has_implicit_ptr = true;
            }
        }
    }

    check_for_auto_ptr_with_type(var, infers_ptr_type, auto_ptr_token);

    if (infers_ptr_type) {
        if (var->parsed_type == context.auto_type) {
            var->parsed_type = context.auto_ptr_type;
        } else {
            var->parsed_type = context.auto_const_ptr_type;
        }
    }

    if (cur_token.is('=') || cur_token.is(':')) {
        next_token();

        if (cur_token.is(Token::SUB_SUB_SUB)) {
            next_token();
            var->should_default_initialize = false;
        } else if (cur_token.is(Token::KW_TRY)) {
            var->assignment = parse_try();
            var->assignment->tryn->catch_recoveree = var;
        } else {
            // only assign here because we only want the current variable around to determine
            // if the assignment contains generic information.
            cur_var = var;
            var->assignment = parse_expr();
            cur_var = nullptr;
        }
    }

    return var;
}

acorn::Struct* acorn::Parser::parse_struct(uint32_t modifiers) {

    next_token(); // Consuming 'struct' token.

    Struct* structn;
    if (cur_generics.empty()) {
        structn = new_declaration<Struct, false>(modifiers, cur_token);
        structn->struct_type = StructType::create(allocator, structn);
    } else {
        auto generic_struct = new_declaration<UnboundGenericStruct, false>(modifiers, cur_token);
        generic_struct->generics = cur_generics;
        structn = generic_struct;
        structn->is_generic = true;
    }

    structn->name = expect_identifier("for struct declaration");
    structn->nspace = allocator.alloc_type<Namespace>();
    new (structn->nspace) Namespace(modl, ScopeLocation::STRUCT);

    check_composite_name_conflict_with_imports(structn->name, "struct");

    auto prev_struct = cur_struct;
    cur_struct = structn;

    // Parse interface extensions.
    if (cur_token.is(':') && peek_token(0).is(':')
        && *(cur_token.buffer_ptr + 1) == ':' // Check that there is not a space because it expects it to basically be a single joined token.
        ) {
        // Consuming '::' token.
        next_token();
        next_token();
        bool more_extensions = false;
        do {

            bool is_dynamic = cur_token.is('*');
            if (is_dynamic) {
                next_token();
            }

            Token extension_name_token = cur_token;
            auto extension_name = expect_identifier("for interface extension");
            structn->unresolved_extensions.push_back({
                extension_name,
                extension_name_token.get_location(),
                is_dynamic
            });

            more_extensions = cur_token.is(',');
            if (more_extensions) {
                next_token();
            }
        } while (more_extensions);
    }

    // Parse body.
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

    Enum* enumn = new_declaration<Enum, false>(modifiers, cur_token);
    enumn->name = expect_identifier("for enum declaration");
    enumn->enum_type = EnumType::create(allocator, enumn);

    check_composite_name_conflict_with_imports(enumn->name, "enum");

    if (cur_token.is(':') && peek_token(0).is(':')
        && *(cur_token.buffer_ptr + 1) == ':' // Check that there is not a space because it expects it to basically be a single joined token.
        ) {
        // Consuming '::' token.
        next_token();
        next_token();
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

        enumn->values.emplace_back(0, value_name, value_name_token.get_location(), value_assignment);

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

acorn::Interface* acorn::Parser::parse_interface(uint32_t modifiers) {

    next_token(); // Consuming 'interface' token.

    Interface* interfacen = new_declaration<Interface, false>(modifiers, cur_token);
    interfacen->name = expect_identifier("for interface declaration");
    interfacen->interface_type = InterfaceType::create(allocator, interfacen);

    check_composite_name_conflict_with_imports(interfacen->name, "interface");

    auto prev_interface = cur_interface;
    cur_interface = interfacen;

    expect('{');
    while (cur_token.is_not('}') && cur_token.is_not(Token::EOB)) {
        auto stmt = parse_statement();
        add_node_to_interface(interfacen, stmt);
    }
    expect('}', "for interface");

    cur_interface = prev_interface;
    return interfacen;
}

void acorn::Parser::check_composite_name_conflict_with_imports(Identifier name, const char* composite_type_str) {
    if (name == Identifier::Invalid) {
        return;
    }

    if (auto existing_import = file->find_import(name)) {
        error(prev_token, "Name of %s '%s' conflicts with import", composite_type_str, name)
            .add_line([this, existing_import](auto& logger) {
                auto [line_number, _] =
                    file->line_table.get_line_and_column_number(existing_import->loc.ptr);

                logger.fmt_print("import defined at line: %s%s%s", Color::BrightYellow, line_number, Color::BrightWhite);
                })
            .end_error(ErrCode::ParseDataTypeNameConflictWithImport);
    }
}

void acorn::Parser::parse_raised_errors(llvm::SmallVector<RaisedError>& raised_errors) {
    bool more_raised_errors = false;
    do {

        Token name_token = cur_token;
        Identifier raised_error_name = expect_identifier("for raised error");
        raised_errors.push_back(RaisedError{
                                   raised_error_name,
                                   name_token.get_location(),
                                   nullptr
                                });

        more_raised_errors = cur_token.is(',');
        if (more_raised_errors) {
            next_token();
        }
    } while (more_raised_errors);
}

uint32_t acorn::Parser::parse_modifiers() {
    uint32_t modifiers = 0;

    while (cur_token.is_modifier()) {
        switch (cur_token.kind) {
        case Token::KW_NATIVE: {
            if (modifiers & Modifier::Native)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Native;

            next_token();
            if (cur_token.is('(')) {
                next_token();
                // TODO (maddie): can linknames take utf8?
                if (cur_token.is(Token::STRING_LITERAL)) {

                    linkname = cur_token.text();
                    linkname = linkname.substr(1, linkname.size() - 2);

                    next_token();
                } else if (cur_token.is(Token::INVALID_STRING_LITERAL)) {
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
        case Token::KW_DLLIMPORT: {
            if (modifiers & Modifier::DllImport)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::DllImport;

            next_token();
            break;
        }
        case Token::KW_PUBLIC: {
            if (modifiers & Modifier::Public)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Public;

            next_token();
            break;
        }
        case Token::KW_PRIVATE: {
            if (modifiers & Modifier::Private)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Private;

            next_token();
            break;
        }
        case Token::KW_READONLY: {
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

void acorn::Parser::parse_generics(llvm::SmallVector<acorn::Generic*>& generics) {

    next_token(); // Consuming 'generics' token.

    ++paren_count;
    expect('(');
    bool more_generics = false;
    do {

        auto genericn = new_node<Generic>(cur_token);
        genericn->name = expect_identifier("for generic type name");

        if (genericn->name != Identifier::Invalid) {
            genericn->type = GenericType::create(allocator, genericn, false);
        }

        genericn->index = generics.size();
        generics.push_back(genericn);

        more_generics = cur_token.is(',');
        if (more_generics) {
            next_token();
        }
    } while (more_generics);
    expect(')');
    --paren_count;
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

    scope->end_loc = prev_token.get_location();
    return scope;
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

    IfStmt* ifn = new_node<IfStmt>(cur_token);
    next_token();

    ++paren_count;
    expect('(');
    if (cur_token.is(Token::IDENTIFIER) &&
        (peek_token(0).is(':') || peek_token(0).is('*') && peek_token(1).is(':'))) {
        ifn->cond = parse_variable(0);
        if (cur_token.is(';')) {
            next_token();
            ifn->post_variable_cond = parse_expr();
        }
    } else {
        ifn->cond = parse_expr();
    }
    expect(')');
    --paren_count;

    ifn->scope = parse_scope("for if");

    if (cur_token.is(Token::KW_ELFIF)) {
        ifn->elseif = parse_if();
    } else if (cur_token.is(Token::KW_ELSE)) {
        next_token(); // Consuming else token.
        if (cur_token.is(Token::KW_IF)) {
            // We do not want the user to use 'else if'.
            SourceLoc error_loc = SourceLoc::from_ptrs(prev_token.buffer_ptr,
                                                       cur_token.buffer_ptr + cur_token.lexeme_length);
            error(error_loc, "Should use 'elif' for else if statements")
                .end_error(ErrCode::ParseMustUseElif);
        }
        ifn->elseif = parse_scope();
    }

    return ifn;
}

void acorn::Parser::parse_comptime_if(bool chain_start, bool takes_path) {

    next_token(); // Consuming '#if' token.

    auto cond = parse_expr();

    bool top_took_path = !takes_path;
    if (!context.has_errors()) {
        Sema analyzer(context, file, logger);
        takes_path &= analyzer.check_comptime_cond(cond, "#if");
    } else {
        // Give up and do not even try sema because there are parse
        // errors somewhere.
        takes_path = false;
    }

    auto add_statement = [this](Node* stmt) finline {
        if (cur_func) {
            if (stmt->is(NodeKind::VAR_LIST)) {
                auto vlist = static_cast<VarList*>(stmt);
                for (auto var : vlist->vars) {
                    cur_func->scope->push_back(var);
                }
                vlist->vars.clear();
            } else {
                cur_func->scope->push_back(stmt);
            }
        } else if (cur_struct) {
            add_node_to_struct(cur_struct, stmt);
        } else if (cur_interface) {
            // TODO add_node_to_interface(cur_interface, stmt);
        } else {
            add_node_to_global_scope(stmt);
        }
    };

    bool can_take_else_path = !top_took_path && !takes_path;
    while (cur_token.is_not(Token::KW_CT_ENDIF) &&
           cur_token.is_not(Token::EOB)) {

        if (cur_token.is(Token::KW_CT_ELIF)) {
            parse_comptime_if(false, !takes_path);
            break;
        } else if (cur_token.is(Token::KW_CT_ELSE)) {

            next_token();
            while (cur_token.is_not(Token::KW_CT_ENDIF) &&
                   cur_token.is_not(Token::EOB)) {

                if (cur_token.is(Token::KW_CT_IF)) {
                    parse_comptime_if(true, can_take_else_path);
                }
                // Check if at the top of the file and parsing imports because if `add_statement`
                // is called it will just complain that the imports are not at the top of file.
                else if (parsing_import_tops && cur_token.is(Token::KW_IMPORT)) {
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
                    } else if (node && node->is(NodeKind::VAR_LIST)) {
                        // We still have to clear the variable list because we called
                         // parse_statement.
                        auto vlist = static_cast<VarList*>(node);
                        vlist->vars.clear();
                    }
                }
            }

            break;
        } else {
            if (cur_token.is(Token::KW_CT_IF)) {
                parse_comptime_if(true, takes_path);
            }
            // Check if at the top of the file and parsing imports because if `add_statement`
            // is called it will just complain that the imports are not at the top of file.
            else if (parsing_import_tops && cur_token.is(Token::KW_IMPORT)) {
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
                } else if (node && node->is(NodeKind::VAR_LIST)) {
                    // We still have to clear the variable list because we called
                    // parse_statement.
                    auto vlist = static_cast<VarList*>(node);
                    vlist->vars.clear();
                }
            }
        }
    }

    if (chain_start) {
        if (cur_token.is_not(Token::KW_CT_ENDIF)) {
            error(cur_token.get_location(), "Expected #endif for comptime #if statement")
                .end_error(ErrCode::ParseMissingComptimeEndIf);
        } else {
            next_token();
        }
    }
}

acorn::Node* acorn::Parser::parse_loop() {
    Token loop_token = cur_token;
    next_token(); // Consuming 'loop' token.

    if (cur_token.is('{')) {
        auto loop = new_node<PredicateLoopStmt>(loop_token);
        loop->scope = parse_scope();
        return loop;
    }

    // Expect early so that we have the best chance at predicting
    // which loop it is.
    ++paren_count;
    expect('(');

    Token peek0 = peek_token(0);

    if (cur_token.is(Token::IDENTIFIER) && peek0.is(Token::KW_IN)) {
        Var* var = new_declaration<Var, false>(0, cur_token);
        var->name = Identifier::get(cur_token.text());
        var->parsed_type = context.auto_type;

        next_token(); // Consuming name token.

        return parse_iterator_loop(loop_token, { var }, false);
    } else if (cur_token.is(Token::IDENTIFIER) && peek0.is('*') && peek_token(1).is(Token::KW_IN)) {
        Var* var = new_declaration<Var, false>(0, cur_token);
        var->name = Identifier::get(cur_token.text());
        var->parsed_type = context.auto_type;

        next_token(); // Consuming '*' token.
        next_token(); // Consuming name token.

        return parse_iterator_loop(loop_token, { var }, true);
    } else if (cur_token.is(Token::IDENTIFIER) && peek0.is(':')) {
        Var* var = parse_variable(0);

        if (!var->assignment && cur_token.is(Token::KW_IN)) {
            return parse_iterator_loop(loop_token, { var }, false);
        }

        return parse_range_loop(loop_token, var);
    } else if (cur_token.is(';')) {
        return parse_range_loop(loop_token, nullptr);
    } else if (cur_token.is(Token::IDENTIFIER) && peek0.is('*') && peek_token(1).is(':')) {
        return parse_range_loop(loop_token, parse_variable(0));
    } else if (cur_token.is(Token::IDENTIFIER) && (
               peek0.is(',') || (peek0.is('*') && peek_token(1).is(',')))
        ) {
        auto var_list = new_node<VarList>(cur_token);
        parse_variable_list(0, var_list);

        return parse_range_loop(loop_token, var_list);
    } else {

        Expr* cond = parse_assignment_and_expr();

        if (cur_token.is(';')) {
            return parse_range_loop(loop_token, cond);
        }

        expect(')');
        --paren_count;

        auto loop = new_node<PredicateLoopStmt>(loop_token);
        loop->cond = cond;
        loop->scope = parse_scope();

        return loop;
    }
}

acorn::Node* acorn::Parser::parse_iterator_loop(Token loop_token, Node* vars, bool var_as_pointer) {
    next_token(); // Consuming 'in' token.

    auto loop = new_node<IteratorLoopStmt>(loop_token);
    loop->vars = std::move(vars);
    loop->var_auto_ptr = var_as_pointer;

    loop->container = parse_expr();
    expect(')'); // Opening expected in `parse_loop`.
    --paren_count;

    loop->scope = parse_scope("for loop");

    return loop;
}

acorn::Node* acorn::Parser::parse_range_loop(Token loop_token, Node* init_node) {
    auto loop = new_node<RangeLoopStmt>(loop_token);
    loop->init_node = init_node;

    expect(';', "for range loop");

    if (cur_token.is_not(';')) {
        loop->cond = parse_expr();
    }

    expect(';', "for range loop");


    if (cur_token.is_not(')')) {
        loop->inc = parse_assignment_and_expr();
        expect(')'); // Opening expected in `parse_loop`.
        --paren_count;
        loop->scope = parse_scope();
    } else { // loop (node; expr;) {}
        expect(')'); // Opening expected in `parse_loop`.
        --paren_count;
        loop->scope = parse_scope("for loop");
    }

    return loop;
}

acorn::Node* acorn::Parser::parse_loop_control() {
    auto loop_control = new_node<LoopControlStmt>(cur_token);
    loop_control->kind = cur_token.is(Token::KW_CONTINUE) ? NodeKind::CONTINUE_STMT : NodeKind::BREAK_STMT;
    next_token(); // Consuming 'break' or 'continue' token.
    return loop_control;
}

acorn::Node* acorn::Parser::parse_switch() {

    auto switchn = new_node<SwitchStmt>(cur_token);
    next_token();

    ++paren_count;
    expect('(');
    switchn->on = parse_expr();
    expect(')');
    --paren_count;

    expect('{');
    while (cur_token.is(Token::KW_CASE)) {
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
        while (cur_token.is_not('}') && cur_token.is_not(Token::KW_CASE) && cur_token.is_not(Token::EOB)) {
            add_node_to_scope(scope, parse_statement());
        }
        scope->end_loc = prev_token.get_location();

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

acorn::Node* acorn::Parser::parse_raise() {
    RaiseStmt* raise = new_node<RaiseStmt>(cur_token);

    next_token(); // Consuming the 'raise' token.

    raise->expr = parse_expr();
    return raise;
}

acorn::Node* acorn::Parser::parse_recover() {
    auto recover = new_node<RecoverStmt>(cur_token);
    next_token();
    recover->value = parse_expr();
    return recover;
}

// Type parsing
//--------------------------------------

acorn::Type* acorn::Parser::parse_type() {
    return parse_trailing_type_info(parse_base_type());
}

acorn::Type* acorn::Parser::parse_base_type() {

    bool is_const = cur_token.is(Token::KW_CONST);
    if (is_const) {
        next_token();
        if (cur_token.is('(')) {
            next_token();
            Type* type = parse_type();
            expect(')', "for const type");
            return type_table.get_const_type(type);
        }
    }

#define ty(t) {                          \
next_token();                            \
if (is_const)                            \
    return type_table.get_const_type(t); \
return t;                                \
}

    switch (cur_token.kind) {
    case Token::KW_VOID:    ty(context.void_type);
    case Token::KW_INT:     ty(context.int_type);
    case Token::KW_INT8:    ty(context.int8_type);
    case Token::KW_INT16:   ty(context.int16_type);
    case Token::KW_INT32:   ty(context.int32_type);
    case Token::KW_INT64:   ty(context.int64_type);
    case Token::KW_UINT8:   ty(context.uint8_type);
    case Token::KW_UINT16:  ty(context.uint16_type);
    case Token::KW_UINT32:  ty(context.uint32_type);
    case Token::KW_UINT64:  ty(context.uint64_type);
    case Token::KW_BOOL:    ty(context.bool_type);
    case Token::KW_CHAR:    ty(context.char_type);
    case Token::KW_CHAR16:  ty(context.char16_type);
    case Token::KW_ISIZE:   ty(context.isize_type);
    case Token::KW_USIZE:   ty(context.usize_type);
    case Token::KW_FLOAT:   ty(context.float_type);
    case Token::KW_DOUBLE:  ty(context.double_type);
    case Token::IDENTIFIER: {
        Token name_token = cur_token;
        auto name = Identifier::get(cur_token.text());
        next_token();
        if (cur_token.is_not('$')) {
            // TODO (maddie): we want to be able to do generic types that take generic types
            // and do partial binding.
            if (!cur_generics.empty() && cur_token.is_not('(')) {
                if (Type* generic_type = find_generic_type(name)) {
                    if (is_const) {
                        generic_type = type_table.get_const_type(generic_type);
                    }
                    return generic_type;
                }
            }

            if (cur_token.is('(')) {
                llvm::SmallVector<Expr*> bound_exprs;
                size_t non_named_generic_args_offsets = -1;
                parse_function_call_args(bound_exprs, non_named_generic_args_offsets);
                auto unresolved_type = UnresolvedGenericCompositeType::create(allocator,
                                                                              name,
                                                                              name_token.get_location(),
                                                                              std::move(bound_exprs),
                                                                              non_named_generic_args_offsets);
                if (is_const) {
                    unresolved_type = type_table.get_const_type(unresolved_type);
                }
                return unresolved_type;
            }

            auto unresolved_type = UnresolvedCompositeType::create(allocator, name, name_token.get_location());
            if (is_const) {
                unresolved_type = type_table.get_const_type(unresolved_type);
            }
            return unresolved_type;
        } else {
            next_token();
            auto unresolved_type = UnresolvedEnumValueType::create(allocator, name, name_token.get_location());
            if (is_const) {
                unresolved_type = type_table.get_const_type(unresolved_type);
            }
            return unresolved_type;
        }
    }
    case Token::KW_FN: {
        next_token();

        bool uses_native_varargs = false;
        llvm::SmallVector<Type*> param_types;
        expect('(', "for function type");
        if (cur_token.is_not(')')) {
            bool more_param_types = false;
            do {

                if (cur_token.is(Token::DOT_DOT_DOT)) {
                    next_token();
                    uses_native_varargs = true;
                    break;
                }

                // Allow for an optional name for the parameter.
                if (cur_token.is(Token::IDENTIFIER) && peek_token(0).is(':')) {
                    next_token();
                    next_token();
                }

                auto type = parse_type();
                param_types.push_back(type);
                if (type == context.void_type) {
                    error(prev_token, "Parameter type cannot be void")
                        .end_error(ErrCode::ParseFuncParamTypeCannotBeVoid);
                }

                more_param_types = cur_token.is(',');
                if (more_param_types) {
                    next_token();
                }
            } while (more_param_types);
        }
        expect(')', "for function type");

        Type* return_type;
        if (cur_token.is(Token::ARROW)) {
            next_token();
            return_type = parse_type();
        } else {
            return_type = context.void_type;
        }

        llvm::SmallVector<RaisedError> raised_errors;
        if (cur_token.is('|')) {
            next_token();
            expect(Token::KW_RAISES);
            parse_raised_errors(raised_errors);
        }

        auto func_type = type_table.get_function_type(return_type, std::move(param_types), std::move(raised_errors), uses_native_varargs);
        if (is_const) {
            return type_table.get_const_type(func_type);
        }
        return func_type;
    }
    default:
        error(cur_token, "Expected type")
            .end_error(ErrCode::ParseInvalidType);
        return nullptr;
    }

#undef ty
}

acorn::Type* acorn::Parser::parse_trailing_type_info(Type* type) {
    while (cur_token.is('*') || cur_token.is('[')) {
        if (cur_token.is('*')) {
            type = type_table.get_ptr_type(type);
            next_token();
        } else {
            if (peek_token(0).is(Token::DOT_DOT)) {
                next_token(); // Consuming '[' token.
                next_token(); // Consuming '..' token.
                expect(']', "for slice type");
                type = type_table.get_slice_type(type);
            } else {
                llvm::SmallVector<ArrayTypeExpr, 8> arr_lengths;

                while (cur_token.is('[') && !peek_token(0).is(Token::DOT_DOT)) {
                    ++bracket_count;
                    next_token();

                    auto arr_expr = cur_token.is_not(']')
                        ? ArrayTypeExpr{ .expr = parse_expr()                      , .has_expr = true  }
                        : ArrayTypeExpr{ .empty_elm_loc = prev_token.get_location(), .has_expr = false };
                    arr_lengths.push_back(arr_expr);

                    expect(']');
                    --bracket_count;
                }

                type = construct_array_type(type, arr_lengths);
            }
        }
    }

    return type;
}

acorn::Type* acorn::Parser::construct_array_type(Type* base_type, const llvm::SmallVector<ArrayTypeExpr, 8>& exprs) {

    Type* type = base_type;
    SourceLoc last_empty_expr_loc;
    bool encountered_assign_det_arr_type = false;
    bool reported_error_about_elm_must_have_length = false;
    for (auto itr = exprs.rbegin(); itr != exprs.rend(); ++itr) {
        ArrayTypeExpr arr_expr = *itr;
        if (!arr_expr.has_expr) {
            type = type_table.get_assigned_det_arr_type(type);
            last_empty_expr_loc = arr_expr.empty_elm_loc;
            encountered_assign_det_arr_type = true;
            continue;
        }

        if (encountered_assign_det_arr_type && !reported_error_about_elm_must_have_length) {
            error(last_empty_expr_loc, "Element type should have length")
                .end_error(ErrCode::ParseElmTypeMustHaveArrLen);
            reported_error_about_elm_must_have_length = true;
        }

        bool resolvable = arr_expr.expr->is(NodeKind::NUMBER);
        Number* number;
        if (resolvable) {
            number = static_cast<Number*>(arr_expr.expr);
            resolvable &= number->type->is_integer() && number->type->get_number_of_bits() <= 32 &&
                          number->value_s32 > 0;
        }

        if (resolvable) {
            // handle common cases first in which it can resolve the length
            // immediately.
            type = type_table.get_arr_type(type, number->value_s32);
        } else {
            type = UnresolvedArrayType::create(allocator, type, arr_expr.expr);
        }
    }

    return type;
}

// Expression functions
//--------------------------------------

acorn::Expr* acorn::Parser::parse_assignment_and_expr() {
    Expr* lhs = parse_expr();
    switch (cur_token.kind) {
    case '=':
    case Token::POUND_EQ:
    case Token::ADD_EQ:
    case Token::SUB_EQ:
    case Token::MUL_EQ:
    case Token::DIV_EQ:
    case Token::MOD_EQ:
    case Token::AND_EQ:
    case Token::OR_EQ:
    case Token::CARET_EQ:
    case Token::TILDE_EQ:
    case Token::LT_LT_EQ:
    case Token::GT_GT_EQ: {
        BinOp* bin_op = new_node<BinOp>(cur_token);
        bin_op->op = cur_token.kind;
        next_token();
        bin_op->lhs = lhs;
        if (cur_token.is(Token::KW_TRY)) {
            bin_op->rhs = parse_try();
            bin_op->rhs->tryn->catch_recoveree = bin_op;
        } else {
            bin_op->rhs = parse_expr();
        }
        return bin_op;
    }
    }
    return lhs;
}

acorn::Expr* acorn::Parser::parse_expr() {
    Expr* lhs = parse_binary_expr(parse_expr_trail());

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

acorn::Expr* acorn::Parser::parse_try(Var* tried_on_var) {

    Try* tryn = new_node<Try>(cur_token);

    next_token(); // Consuming 'try' token.

    bool catches_error = cur_token.is(Token::IDENTIFIER) && peek_token(0).is(':');
    if (catches_error) {
        tryn->caught_var = new_declaration<Var, true>(0, cur_token);
        tryn->caught_var->name = Identifier::get(cur_token.text());

        next_token(); // Consume the name token.
        next_token(); // Consume the ':' token.
    }

    cur_var = tried_on_var;
    ++paren_count;
    expect('(');
    Expr* caught_expr = parse_expr();
    expect(')');
    --paren_count;
    cur_var = nullptr;

    tryn->caught_expr = caught_expr;
    caught_expr->tryn = tryn;

    if (catches_error || cur_token.is('{')) {
        tryn->catch_scope = parse_scope("for catch block");
    }

    return caught_expr;
}

acorn::Expr* acorn::Parser::parse_binary_expr(Expr* lhs) {

    struct StackUnit {
        Token op;
        Expr* expr;
    };
    std::stack<StackUnit> op_stack;

    // Help function to look ahead and determine if it needs to split the sign symbol
    // from the number and treat it as part of the binary expression instead.
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

        Expr* rhs = parse_expr_trail();
        next_op = get_op();
        int next_prec = context.get_op_precedence(next_op);

        if (next_prec != -1 && next_prec > prec) {
            // Delaying the operation until later since the next operator
            // has a higher precedence.
            op_stack.push(StackUnit{ op, lhs });
            lhs = rhs;
            op = next_op;
        } else {
            if (lhs->is(NodeKind::NUMBER) && rhs->is(NodeKind::NUMBER)) {
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
                if (lhs->is(NodeKind::NUMBER) && rhs->is(NodeKind::NUMBER)) {
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

acorn::Expr* acorn::Parser::new_binary_op(Token op_tok, Expr* lhs, Expr* rhs) {
    BinOp* bin_op = new_node<BinOp>(op_tok);
    bin_op->op = op_tok.kind;
    bin_op->lhs = lhs;
    bin_op->rhs = rhs;
    return bin_op;
}

std::pair<acorn::Token, acorn::Token> acorn::Parser::split_number_from_sign(Token token) {

    TokenKind op_sign = *token.buffer_ptr;
    Token op = Token(op_sign, token.buffer_ptr, 1);

    token.buffer_ptr    += 1;
    token.lexeme_length -= 1;

    return { op, token };
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
            static_cast<uint32_t>(e - s),
            op.buffer_ptr,
            op.lexeme_length
        };

        lhs->trivially_reassignable = lhs->trivially_reassignable && rhs->trivially_reassignable;
        return lhs;
    };

    switch (op.kind) {
    case '+': {
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
        if (rval < 0 && lval > std::numeric_limits<T>::max() + rval) {
            return report_overflow(op, lhs, rhs, to_type);
        }

        if constexpr (is_signed) {
            if (rval > 0 && lval < std::numeric_limits<T>::min() + rval) {
                return report_underflow(op, lhs, rhs, to_type);
            }
        }
        return calc(lval - rval);
    }
    case '*': {
        if (rval != 0 && lval > std::numeric_limits<T>::max() / rval) {
            return report_overflow(op, lhs, rhs, to_type);
        }

        if constexpr (is_signed) {
            if (lval == ((T)-1) && rval == std::numeric_limits<T>::min()) {
                return report_overflow(op, lhs, rhs, to_type);
            }
            if (rval == ((T)-1) && lval == std::numeric_limits<T>::min()) {
                return report_overflow(op, lhs, rhs, to_type);
            }

            if (rval != 0 && lval < std::numeric_limits<T>::min() / rval) {
                return report_underflow(op, lhs, rhs, to_type);
            }
        }
        return calc(lval * rval);
    }
    case '/': {
        if constexpr (is_signed) {
            if (rval == ((T)-1) && lval == std::numeric_limits<T>::min()) {
                return report_overflow(op, lhs, rhs, to_type);
            }
        }

        if (rval == 0) {
            // Let sema complain about division by zero.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval / rval);
    }
    case '%': {
        if constexpr (is_signed) {
            if (rval == ((T)-1) && lval == std::numeric_limits<T>::min()) {
                return report_overflow(op, lhs, rhs, to_type);
            }
        }

        if (rval == 0) {
            // Let sema complain about division by zero.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval % rval);
    }
    case Token::LT_LT: {
        if (rval < 0 || rval == 0 || ((rval - 1) > (T)to_type->get_number_of_bits())) {
            // These are shift error cases handled during sema.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval << rval);
    }
    case Token::GT_GT: {
        if (rval < 0 || rval == 0 || (rval - 1) > (T)to_type->get_number_of_bits()) {
            // These are shift error cases handled during sema.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval >> rval);
    }
    case '<':         return calc(lval <  rval);
    case '>':         return calc(lval >  rval);
    case Token::LT_EQ: return calc(lval <= rval);
    case Token::GT_EQ: return calc(lval >= rval);
    case Token::EQ_EQ: return calc(lval == rval);
    case Token::EX_EQ: return calc(lval != rval);
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

        if constexpr (std::is_same_v<float, T>) {
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
            static_cast<uint32_t>(e - s),
            op.buffer_ptr,
            op.lexeme_length
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
    case Token::LT_EQ: return calc(lval <= rval);
    case Token::GT_EQ: return calc(lval >= rval);
    default:
        // Handled during sema.
        return new_binary_op(op, lhs, rhs);
    }

}

acorn::Expr* acorn::Parser::report_overflow(Token op, Expr* lhs, Expr* rhs, Type* to_type) {
    auto loc_op = new_binary_op(op, lhs, rhs);
    logger.begin_error(expand(loc_op), "Operator '%s' numeric overflow. Cannot fit for type '%s'",
            token_kind_to_string(op.kind, context), to_type)
        .end_error(ErrCode::NumericOverflow);
    return new_node<InvalidExpr>(op.get_location());
}

acorn::Expr* acorn::Parser::report_underflow(Token op, Expr* lhs, Expr* rhs, Type* to_type) {
    auto loc_op = new_binary_op(op, lhs, rhs);
    logger.begin_error(expand(loc_op), "Operator '%s' numeric underflow. Cannot fit into type '%s'",
            token_kind_to_string(op.kind, context), to_type)
        .end_error(ErrCode::NumericUnderflow);
    return new_node<InvalidExpr>(op.get_location());
}

acorn::Expr* acorn::Parser::fold_number(Token op, Expr* lhs, Expr* rhs) {

    auto fold_int_by_side = [op, this, lhs, rhs](Expr* side) -> Expr* finline {
        Number* lnum = static_cast<Number*>(lhs);
        Number* rnum = static_cast<Number*>(rhs);
        Type* to_type = side->type;
        switch (to_type->get_kind()) {
        case TypeKind::UINT8: case TypeKind::CHAR:
            return fold_int<uint8_t> (op, lnum, rnum, to_type);
        case TypeKind::UINT16: case TypeKind::CHAR16:
            return fold_int<uint16_t>(op, lnum, rnum, to_type);
        case TypeKind::UINT32:
            return fold_int<uint32_t>(op, lnum, rnum, to_type);
        case TypeKind::UINT64: return fold_int<uint64_t>(op, lnum, rnum, to_type);
        case TypeKind::INT8:   return fold_int<int8_t> (op, lnum, rnum, to_type);
        case TypeKind::INT16:  return fold_int<int16_t>(op, lnum, rnum, to_type);
        case TypeKind::INT32: case TypeKind::INT:
            return fold_int<int32_t>(op, lnum, rnum, to_type);
        case TypeKind::INT64:  return fold_int<int64_t>(op, lnum, rnum, to_type);
        default:
            acorn_fatal("Missing numeric fold cast");
            return nullptr;
        }
    };

    if (lhs->type->is_integer() && rhs->type->is_integer()) {
        if (op.is(Token::LT_LT) || op.is(Token::GT_GT)) {
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
            if (lhs->type == context.float_type) {
                return fold_float<float>(op, lnum, rnum, lhs->type, lnum->value_f32, rnum->value_f32);
            } else {
                return fold_float<double>(op, lnum, rnum, lhs->type, lnum->value_f64, rnum->value_f64);
            }
        } else if (rhs->type->is_integer()) {
            if (lhs->type == context.float_type) {
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
            if (lhs->type == context.float_type) {
                return fold_float<float>(op, lnum, rnum, lhs->type, lnum->value_f32, rnum->value_f32);
            } else {
                return fold_float<double>(op, lnum, rnum, lhs->type, lnum->value_f64, rnum->value_f64);
            }
        } else if (rhs->type->is_integer()) {
            if (rhs->type == context.float_type) {
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

acorn::Expr* acorn::Parser::parse_expr_trail() {
    return parse_expr_trail(parse_term());
}

acorn::Expr* acorn::Parser::parse_expr_trail(Expr* term) {
    if (cur_token.is('(')) {
        FuncCall* call = parse_function_call(term);
        if (cur_token.is('{') && call->site->is(NodeKind::IDENT_REF)) {
            return parse_expr_trail(parse_struct_initializer(call));
        }

        if (term->is(NodeKind::IDENT_REF) && peek_if_expr_is_type(cur_token, peek_token(0))) {
            auto ref = static_cast<IdentRef*>(call->site);
            auto un_composite_type = UnresolvedGenericCompositeType::create(context.get_allocator(),
                                                                            ref->ident,
                                                                            ref->loc,
                                                                            std::move(call->args),
                                                                            call->non_named_args_offset);
            Type* type = parse_trailing_type_info(un_composite_type);
            TypeExpr* type_expr = new_node<TypeExpr>(ref->loc);
            type_expr->parsed_expr_type = type;

            SourceLoc start_loc = ref->loc;
            SourceLoc end_loc = prev_token.get_location();

            SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                           end_loc.ptr + end_loc.length);
            type_expr->loc = type_location;
            return type_expr;
        }

        return parse_expr_trail(call);
    } else if (cur_token.is('.')) {
        auto dot = new_node<DotOperator>(cur_token);
        next_token(); // Consuming '.' token.

        dot->site = term;
        dot->ident = expect_identifier("for '.' operator");

        return parse_expr_trail(dot);
    } else if (cur_token.is(Token::ADD_ADD) || cur_token.is(Token::SUB_SUB)) {

        UnaryOp* unary_op = new_node<UnaryOp>(cur_token);
        unary_op->expr = term;
        if (cur_token.is(Token::ADD_ADD)) {
            unary_op->op = Token::POST_ADD_ADD;
        } else {
            unary_op->op = Token::POST_SUB_SUB;
        }
        next_token();
        return unary_op;
    } else if (cur_token.is('[')) {

        llvm::SmallVector<ArrayTypeExpr, 8> indexes;
        llvm::SmallVector<SourceLoc, 8>     bracket_locations;

        while (cur_token.is('[')) {
            if (peek_token(0).is(Token::DOT_DOT)) {
                break;
            }

            bracket_locations.push_back(cur_token.get_location());
            next_token(); // Consuming '[' token.
            ++bracket_count;

            indexes.push_back({ .expr = parse_expr(), .has_expr = true });

            expect(']');
            --bracket_count;
        }

        if (term->is(NodeKind::IDENT_REF) || term->is(NodeKind::FUNC_CALL)) {
            if (peek_if_expr_is_type(cur_token, peek_token(0))) {
                Type* type;
                IdentRef* ref;
                if (term->is(NodeKind::IDENT_REF)) {
                    ref = static_cast<IdentRef*>(term);
                    type = UnresolvedCompositeType::create(allocator, ref->ident, ref->loc, false);
                } else {
                    auto call = static_cast<FuncCall*>(term);
                    ref = static_cast<IdentRef*>(call->site);
                    type = UnresolvedGenericCompositeType::create(context.get_allocator(),
                                                                  ref->ident,
                                                                  ref->loc,
                                                                  std::move(call->args),
                                                                  call->non_named_args_offset);
                }

                type = construct_array_type(type, indexes);
                type = parse_trailing_type_info(type);

                TypeExpr* type_expr = new_node<TypeExpr>(cur_token);
                type_expr->parsed_expr_type = type;

                SourceLoc start_loc = ref->loc;
                SourceLoc end_loc   = prev_token.get_location();

                SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                               end_loc.ptr + end_loc.length);
                type_expr->loc = type_location;
                return type_expr;
            }
        }

        size_t count = 0;
        for (auto arr_expr : indexes) {
            auto mem_access = new_node<MemoryAccess>(bracket_locations[count]);
            mem_access->site = term;
            mem_access->index = arr_expr.expr;

            term = mem_access;
            ++count;
        }

        return parse_expr_trail(term);
    }
    return term;
}

acorn::FuncCall* acorn::Parser::parse_function_call(Expr* site) {
    FuncCall* call = new_node<FuncCall>(cur_token);
    call->site = site;
    parse_function_call_args(call->args, call->non_named_args_offset);
    return call;
}

acorn::Expr* acorn::Parser::parse_generic_function_bind_call() {
    GenericBindFuncCall* call = new_node<GenericBindFuncCall>(cur_token);
    call->explicitly_binds_generics = true;
    call->ident = Identifier::get(cur_token.text());
    next_token();
    next_token(); // Consuming '!' token.
    parse_function_call_args(call->args, call->non_named_args_offset);
    return call;
}

void acorn::Parser::parse_function_call_args(llvm::SmallVector<Expr*>& args, size_t& non_named_args_offset) {

    next_token(); // Consuming '(' token.
    ++paren_count;
    if (cur_token.is_not(')')) {
        bool more_args = false, full_reported = false;
        do {
            Expr* arg;
            if (cur_token.is(Token::IDENTIFIER) && peek_token(0).is('=')) {

                auto named_arg = new_node<NamedValue>(cur_token);
                named_arg->name = Identifier::get(cur_token.text());
                arg = named_arg;
                next_token(); // Consuming identifier token.
                next_token(); // Consuming '=' token.

                named_arg->assignment = parse_expr();

                if (non_named_args_offset == -1) {
                    non_named_args_offset = args.size();
                }
            } else {
                arg = parse_expr();
            }

            if (args.size() != MAX_FUNC_PARAMS) {
                args.push_back(arg);
            } else if (!full_reported) {
                logger.begin_error(expand(arg), "Exceeded maximum number of function arguments. Max: %s",
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
    --paren_count;
    expect(')', "for function call");
}

acorn::Expr* acorn::Parser::parse_term() {
    switch (cur_token.kind) {
    case Token::INT_LITERAL:           return parse_int_literal();
    case Token::HEX_LITERAL:           return parse_hex_literal();
    case Token::BIN_LITERAL:           return parse_bin_literal();
    case Token::OCT_LITERAL:           return parse_oct_literal();
    case Token::FLOAT_LITERAL:         return parse_float_literal();
    case Token::DOUBLE_LITERAL:        return parse_double_literal();
    case Token::STRING_LITERAL:        return parse_string_literal();
    case Token::CHAR_LITERAL:          return parse_char_literal();
    case Token::INVALID_STRING_LITERAL:
    case Token::INVALID_CHAR_LITERAL:
    case Token::INVALID_NUMBER_LITERAL: {
        next_token();
        return new_node<InvalidExpr>(cur_token);
    }
    case Token::KW_TRUE: {
        Bool* b = new_node<Bool>(cur_token);
        next_token(); // Consuming 'true' token.
        b->value = true;
        b->type = context.bool_type;
        return b;
    }
    case Token::KW_FALSE: {
        Bool* b = new_node<Bool>(cur_token);
        next_token(); // Consuming 'false' token.
        b->value = false;
        b->type = context.bool_type;
        return b;
    }
    case Token::KW_NULL: {
        Null* null = new_node<Null>(cur_token);
        null->type = context.null_type;
        next_token();
        return null;
    }
    case Token::IDENTIFIER: {

        auto peek0 = peek_token(0);
        auto peek1 = peek_token(1);
        if (peek_if_expr_is_type(peek0, peek1) || peek0.is('$')) {
            return parse_type_expr();
        }

        if (peek0.is('!') && peek1.is('(')) {
            return parse_expr_trail(parse_generic_function_bind_call());
        }

        auto ident = Identifier::get(cur_token.text());
        if (!cur_generics.empty()) {
            if (Type* generic_type = find_generic_type(ident)) {
                if (peek_token(0).is_not('{')) {
                    return parse_type_expr();
                }
            }
        }

        IdentRef* ref = new_node<IdentRef>(cur_token);
        ref->ident = ident;

        next_token(); // Consuming the identifier.

        if (cur_token.is('{')) {
            return parse_struct_initializer(ref);
        }

        return ref;
    }
    case '\\': case Token::BACKSLASH_BACKSLASH: {
        bool file_local = cur_token.is('\\');
        next_token();

        Token ident_token = cur_token;

        IdentRef* ref = new_node<IdentRef>(cur_token);
        ref->ident = expect_identifier();
        ref->relative_enforcement = file_local ? IdentRef::RelativeEnforcement::FILE
            : IdentRef::RelativeEnforcement::MODULE;

        if (cur_token.is('{')) {
            return parse_struct_initializer(ref);
        }

        return ref;
    }
    // Unary operators.
    case '+': case '-': case '~': case '!':
    case '&': case Token::ADD_ADD: case Token::SUB_SUB:
    case '*': {
        Token unary_token = cur_token;
        next_token(); // Consuming the unary token.

        bool unary_on_num_literal = cur_token.is(Token::INT_LITERAL);
        Token after_op_token = cur_token;
        Expr* expr = parse_expr_trail();

        if (unary_token.kind == '+' && expr->is(NodeKind::NUMBER)) {
            return expr; // + has no effect on value.
        } else if (unary_token.kind == '-' && expr->is(NodeKind::NUMBER)) {
            Number* num = static_cast<Number*>(expr);

            switch (num->type->get_kind()) {
            case TypeKind::UINT64:                         num->value_u64 = -num->value_u64; return num;
            case TypeKind::UINT32:                         num->value_u32 = -num->value_u32; return num;
            case TypeKind::UINT16: case TypeKind::CHAR16:  num->value_u16 = -num->value_u16; return num;
            case TypeKind::UINT8:  case TypeKind::CHAR:    num->value_u8  = -num->value_u8;  return num;
            case TypeKind::INT64:                          num->value_s64 = -num->value_s64; return num;
            case TypeKind::INT32:  case TypeKind::INT:     num->value_s32 = -num->value_s32; return num;
            case TypeKind::INT16:                          num->value_s16 = -num->value_s16; return num;
            case TypeKind::INT8:                           num->value_s8  = -num->value_s8;  return num;
            case TypeKind::FLOAT:                          num->value_f32 = -num->value_f32; return num;
            case TypeKind::DOUBLE:                         num->value_f64 = -num->value_f64; return num;
            default:
                // Just let sema handle it because it is a arch. dep. type.
                break;
            }
        } else if (unary_token.kind == '~' && expr->is(NodeKind::NUMBER) &&
                   expr->type->is_integer()) {
            Number* num = static_cast<Number*>(expr);

            switch (num->type->get_kind()) {
            case TypeKind::UINT64:                        num->value_u64 = ~num->value_u64; return num;
            case TypeKind::UINT32:                        num->value_u32 = ~num->value_u32; return num;
            case TypeKind::UINT16: case TypeKind::CHAR16: num->value_u16 = ~num->value_u16; return num;
            case TypeKind::UINT8:  case TypeKind::CHAR:   num->value_u8  = ~num->value_u8;  return num;
            case TypeKind::INT64:                         num->value_s64 = ~num->value_s64; return num;
            case TypeKind::INT32:  case TypeKind::INT:    num->value_s32 = ~num->value_s32; return num;
            case TypeKind::INT16:                         num->value_s16 = ~num->value_s16; return num;
            case TypeKind::INT8:                          num->value_s8  = ~num->value_s8;  return num;
            default:
                // Just let sema handle it because it is a arch. dep. type.
                break;
            }
        }

        UnaryOp* unary_op = new_node<UnaryOp>(unary_token);
        unary_op->op = unary_token.kind;
        unary_op->expr = expr;

        return unary_op;
    }
    case '(': {
        ++paren_count;
        next_token();
        Expr* expr = parse_expr();
        expect(')');
        --paren_count;
        return expr;
    }
    case Token::KW_AS: {
        Cast* cast = new_node<Cast>(cur_token);
        ++paren_count;
        next_token();
        expect('(');
        cast->explicit_cast_type = parse_type();
        expect(')');
        --paren_count;
        cast->value = parse_expr_trail();
        return cast;
    }
    case Token::KW_BITCAST: {
        BitCast* cast = new_node<BitCast>(cur_token);
        ++paren_count;
        next_token();
        expect('(');
        cast->explicit_cast_type = parse_type();
        expect(')');
        --paren_count;
        cast->value = parse_expr_trail();
        return cast;
    }
    case Token::KW_SIZEOF: {
        SizeOf* sof = new_node<SizeOf>(cur_token);
        sof->trivially_reassignable = true;
        next_token();
        ++paren_count;
        expect('(');
        sof->value = parse_expr();
        expect(')');
        --paren_count;
        return sof;
    }
    case Token::KW_MOVEOBJ: {
        MoveObj* move_obj = new_node<MoveObj>(cur_token);
        next_token();
        ++paren_count;
        expect('(');
        move_obj->value = parse_expr();
        expect(')');
        --paren_count;
        return move_obj;
    }
    case Token::KW_THIS: {
        This* thisn = new_node<This>(cur_token);
        next_token();
        return thisn;
    }
    case Token::KW_CT_TYPEINFO: {
        Reflect* reflect = new_node<Reflect>(cur_token);
        reflect->reflect_kind = context.get_reflect_kind(cur_token.text());
        next_token();
        ++paren_count;
        expect('(');
        reflect->expr = parse_expr();
        expect(')');
        --paren_count;
        return reflect;
    }
    case TypeTokens: {
        return parse_type_expr();
    }
    case '[':
        return parse_array();
    case Token::KW_FN: {
        if (peek_token(0).is('(')) {
            return parse_type_expr();
        }
        [[fallthrough]];
    }
    default:
        error(cur_token, "Expected an expression")
            .end_error(ErrCode::ParseExpectedExpression);
        skip_recovery();
        return new_node<InvalidExpr>(cur_token);
    }
}

acorn::Number* acorn::Parser::parse_int_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    auto number = parse_int_number<10, void_table, false>(text.data(), text.end());
    char last_char = text.back();
    if (last_char == 'f') {
        number->type = context.float_type;
    } else if (last_char == 'd') {
        number->type = context.double_type;
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
    return parse_int_number<16, hex_table>(text.data() + 2, text.end());
}

acorn::Number* acorn::Parser::parse_bin_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_int_number<2, void_table, false>(text.data() + 2, text.end());
}

acorn::Number* acorn::Parser::parse_oct_literal() {
    static uint64_t void_table[256];
    const auto text = cur_token.text();
    return parse_int_number<8, void_table, false>(text.data() + 1, text.end());
}

template<uint32_t radix, uint64_t convert_table[256], bool use_table>
acorn::Number* acorn::Parser::parse_int_number(const char* start, const char* end) {

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
        ++ptr;
        bool is_signed = *ptr == 'i';
        ++ptr;

        auto report_fits_error = [this, number]() finline {
            auto err_msg = get_error_msg_for_value_not_fit_type(number->type);
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
        number->trivially_reassignable = true;

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

acorn::Number* acorn::Parser::parse_float_literal() {

    auto [value, parse_error] = parse_float32_bits(allocator, cur_token.text());

    Number* number = new_node<Number>(cur_token);
    number->value_f32 = value;
    number->type = context.float_type;

    if (parse_error != FloatParseError::None) {
        report_float_error(parse_error);
    }

    next_token();
    return number;
}

acorn::Number* acorn::Parser::parse_double_literal() {

    auto [value, parse_error] = parse_float64_bits(allocator, cur_token.text());

    Number* number = new_node<Number>(cur_token);
    number->value_f64 = value;
    number->type = context.double_type;

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

acorn::Expr* acorn::Parser::parse_string_literal() {

    auto string = new_node<String>(cur_token);
    string->type = type_table.get_ptr_type(type_table.get_const_type(context.char_type));

#define next_codepoint_digit(codepoint)                               \
codepoint = 16u * codepoint + static_cast<uint32_t>(hex_table[*ptr]); \
++ptr;

    auto text = cur_token.text();
    const char* ptr = text.data() + 1; // +1 skip the "
    while (*ptr != '"') {
        if (*ptr == '\\') {
            ++ptr;
            if (*ptr == 'u') {
                ++ptr; // skip past u

                uint32_t codepoint = 0;
                codepoint = static_cast<uint32_t>(hex_table[*ptr]);
                ++ptr;
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);

                convert_and_check_codepoint(codepoint, string->text, ptr, 6);
            } else if (*ptr == 'U') {
                ++ptr; // skip past U

                uint32_t codepoint = 0;
                codepoint = static_cast<uint32_t>(hex_table[*ptr]);
                ++ptr;
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);
                next_codepoint_digit(codepoint);

                convert_and_check_codepoint(codepoint, string->text, ptr, 10);
            } else {
                string->text += get_escape_char(*ptr);
                ++ptr;
            }
        } else {
            unsigned char ch = static_cast<unsigned char>(*ptr);
            if (ch < 0x80) {
                string->text += *ptr;
                ++ptr;
            } else {
                // dealing with multi-byte character.

                bool is_valid_utf8;
                bool is_overlong;
                size_t num_bytes = get_utf8_byte_distance(ptr, is_valid_utf8, is_overlong);

                if (!is_valid_utf8) {
                    error(cur_token, "Invalid UTF-8 characters in string")
                        .end_error(ErrCode::ParseInvalidUTF8InString);
                    ++ptr;
                    continue;
                }

                size_t byte_count = 0;
                while (byte_count < num_bytes) {
                    string->text += *ptr;
                    ++byte_count;
                    ++ptr;
                }
            }
        }
    }
    next_token();
    return string;
#undef next_codepoint_digit
}

acorn::Expr* acorn::Parser::parse_char_literal() {

    auto character = new_node<Number>(cur_token);
    character->type = context.char_type;

    auto text = cur_token.text();
    const char* ptr = text.data() + 1; // Skip the '

    if (*ptr == '\\') {
        ++ptr;
        character->value_u64 = get_escape_char(*ptr);
    } else {
        character->value_u64 = *ptr;
    }

    character->trivially_reassignable = true;
    next_token();
    return character;
}

void acorn::Parser::convert_and_check_codepoint(uint32_t codepoint,
                                                std::string& dest_string,
                                                const char* ptr,
                                                int ptr_offset) {
    bool is_valid;
    codepoint_to_utf8(codepoint, dest_string, is_valid);
    if (!is_valid) {
        auto error_loc = SourceLoc::from_ptrs(ptr - ptr_offset, ptr);
        error(error_loc, "Invalid UTF-8 codepoint")
            .end_error(ErrCode::ParseInvalidCodepoint);
    }
}

acorn::Expr* acorn::Parser::parse_array() {

    Array* arr = new_node<Array>(cur_token);

    ++bracket_count;
    next_token(); // Consuming '[' token.

    if (cur_token.is_not(']')) {
        bool more_values = false;
        do {

            bool uses_assigned_index = false;
            if (cur_token.is('[')) {
                switch (peek_token(0).kind) {
                case Token::INT_LITERAL:
                case Token::BIN_LITERAL:
                case Token::HEX_LITERAL:
                case Token::OCT_LITERAL:
                case Token::CHAR_LITERAL:
                    if (peek_token(1).is(']') && peek_token(2).is('=')) {
                        uses_assigned_index = true;

                        next_token(); // Consume [

                        Token number_token = cur_token;

                        auto number = static_cast<Number*>(parse_term());

                        next_token(); // Consume ]
                        next_token(); // Consume =
                        auto expr = parse_expr();

                        const size_t MAX_ADDED = 0xFFF;

                        if (number_token.text()[0] != '-') {

                            uint64_t index_u64 = number->value_u64;
                            bool too_many_values = index_u64 > static_cast<uint64_t>(std::numeric_limits<uint32_t>::max());
                            if (!too_many_values) {
                                size_t index = static_cast<size_t>(index_u64);
                                if (index < arr->elms.size()) {
                                    if (arr->elms[index]) {
                                        error(number_token, "Array index %s already assigned", index)
                                            .end_error(ErrCode::ParseArrayIndexAlreadyAssigned);
                                    } else {
                                        arr->elms[index] = expr;
                                    }
                                } else {
                                    size_t amount_added = index - arr->elms.size();
                                    if (amount_added > MAX_ADDED) {
                                        too_many_values = true;
                                    } else {
                                        for (size_t i = arr->elms.size(); i < index; i++) {
                                            arr->elms.push_back(nullptr);
                                        }
                                        arr->elms.push_back(expr);
                                    }
                                }
                            }

                            if (too_many_values) {
                                error(number->loc, "Array assignment index '%s' would result in over %s elements to be added",
                                      index_u64, MAX_ADDED)
                                    .end_error(ErrCode::ParseArrAssignIndexTooBig);
                            }
                        } else {
                            error(number->loc, "Array index cannot be negative")
                                .end_error(ErrCode::ParseArrayIndexCannotBeNeg);
                        }
                    }
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
    --bracket_count;

    expect(']', "for array");
    return arr;
}

acorn::Expr* acorn::Parser::parse_struct_initializer(Expr* site) {

    auto initializer = new_node<StructInitializer>(cur_token);
    initializer->site = site;

    next_token(); // Consuming '{' token.

    if (cur_token.is_not('}')) {
        bool more_values = false;
        do {
            Expr* value;
            if (cur_token.is(Token::IDENTIFIER) && peek_token(0).is('=')) {

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

acorn::Expr* acorn::Parser::parse_type_expr() {
    Token start_token = cur_token;
    TypeExpr* type_expr = new_node<TypeExpr>(cur_token);
    type_expr->parsed_expr_type = parse_type();

    SourceLoc start_loc = start_token.get_location();
    SourceLoc end_loc   = prev_token.get_location();

    SourceLoc type_location = SourceLoc::from_ptrs(start_loc.ptr,
                                                   end_loc.ptr + end_loc.length);
    type_expr->loc = type_location;
    return type_expr;
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
        cur_token = lexer.next_token();
    }
}

acorn::Token acorn::Parser::peek_token(size_t n) {
    assert(n < MAX_PEEKED_TOKENS && "Peek index exceeds the maximum number of peeked tokens.");

    // Ensure the tokens up to n are stored.
    while (peeked_size <= n) {
        peeked_tokens[peeked_size++] = lexer.next_token();
    }

    return peeked_tokens[n];
}

bool acorn::Parser::expect(TokenKind kind, const char* for_msg) {
    if (cur_token.is(kind)) {
        next_token();
        return true;
    } else {
        const std::string str_kind = token_kind_to_string(kind, context);
        const std::string arrow_msg = std::format("add '{}' here", str_kind);
        const bool is_closing = kind == ')' || kind == '}';
        const auto closing_msg = is_closing ? " closing" : "";
        auto fixed_for_msg = for_msg ? std::string{ " " } + for_msg : "";

        if (prev_token.is(Token::INVALID_STRING_LITERAL)) {
            auto text = prev_token.text();
            if (text.back() != '\"') {
                // The string was missing its ending " so checking if the
                // character we expect happens to be in the string.
                if (kind <= 128) {
                    if (static_cast<TokenKind>(text.back()) == kind) {
                        return false;
                    }
                }
            }
        }

        logger.begin_error(prev_token.get_location(), "Expected%s '%s' token%s", closing_msg, str_kind, fixed_for_msg)
              .add_arrow_msg(Logger::CaretPlacement::After, arrow_msg)
              .end_error(ErrCode::ParseExpect);
        return false;
    }
}

acorn::Type* acorn::Parser::find_generic_type(Identifier name) const {
    auto itr = std::ranges::find_if(cur_generics, [name](auto genericn) {
        return genericn->name == name;
    });
    if (itr != cur_generics.end()) {
        if (cur_var) {
            cur_var->assignment_contains_generics = true;
        }
        return (*itr)->type;
    }
    return nullptr;
}

acorn::Identifier acorn::Parser::expect_identifier(const char* for_msg) {
    if (cur_token.is(Token::IDENTIFIER)) {
        Identifier identifier = Identifier::get(cur_token.text());
        next_token();
        return identifier;
    } else {
        auto fixed_for_msg = for_msg ? std::string{" "} + for_msg : "";
        logger.begin_error(cur_token.get_location(), "Expected identifier%s", fixed_for_msg);
        if (cur_token.is_keyword()) {
            logger.add_line("Help: '%s' is a keyword", token_kind_to_string(cur_token.kind, context));
            // Eating token since it is enough like an identifier that the user likely
            // intended it to be.
            next_token();
        }
        logger.end_error(ErrCode::ParseExpectIdent);
        return Identifier();
    }
}

bool acorn::Parser::peek_if_expr_is_type(Token tok0, Token tok1) {
    if (tok0.is('*')) {
        switch (tok1.kind) {
        case ')':
        case '[':
        case ']': // end of array expression.
        case ';':
        case '*':
            return true;
        default:
            break;
        }
    } else if (tok0.is('[') && tok1.is(Token::DOT_DOT)) {
        return true;
    }
    return false;
}

void acorn::Parser::check_for_auto_ptr_with_type(Var* var, bool infers_ptr_type, Token auto_ptr_token) {
    if (infers_ptr_type) {
        if (var->parsed_type->get_kind() != TypeKind::AUTO) {
            error(auto_ptr_token, "Cannot specify pointer for inference because the variable also declares a type")
                .end_error(ErrCode::ParseVariableWithPtrAutoSpecifiesType);
        }
    }
}

void acorn::Parser::skip_recovery(bool stop_on_modifiers) {
    while (true) {
        switch (cur_token.kind) {
        case Token::EOB:
        case ';':
        case '{':
        case '}':
        case ',':
        case Token::KW_IMPORT:
        case Token::KW_IF:
        case Token::KW_RETURN:
        case Token::KW_CT_IF:
        case Token::KW_STRUCT:
        case Token::KW_ENUM:
        case Token::KW_INTERFACE:
        case Token::KW_SWITCH:
        case Token::KW_RAISE:
        case Token::KW_CT_ABORTS:
        case Token::KW_TRY:
        case Token::Token::KW_RECOVER:
        case ModifierTokens:
            if (stop_on_modifiers) {
                return;
            }
            next_token();
            break;
        case Token::KW_CONST:
            if (peek_token(0).is(Token::KW_FN)) {
                return;
            }
            next_token();
            break;
        case Token::KW_FN:
            switch (peek_token(0).kind) {
            case Token::IDENTIFIER:
            case Token::KW_NEW:
            case Token::KW_MOVEOBJ:
            case Token::KW_COPYOBJ:
            case Token::KW_DELETE:
                return;
            }
            next_token();
            break;
        case Token::IDENTIFIER:
            if (peek_token(0).is(':')) {
                return;
            }
            next_token();
            break;
        case ']': {
            if (bracket_count > 0)
                return;
            next_token();
            break;
        }
        case ')': {
            if (paren_count > 0)
                return;
            next_token();
            break;
        }
        default:
            next_token();
            break;
        }
    }
}
