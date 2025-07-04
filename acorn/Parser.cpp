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
case Token::KwFloat:   \
case Token::KwDouble:  \
case Token::KwConst:   \
case Token::KwChar:    \
case Token::KwChar16:  \
case Token::KwUSize:   \
case Token::KwISize:   \
case Token::KwBool

#define ModifierTokens   \
     Token::KwNative:    \
case Token::KwDllimport: \
case Token::KwPublic:    \
case Token::KwPrivate:   \
case Token::KwReadonly

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

    if (node->is(NodeKind::Func)) {

        auto func = static_cast<Func*>(node);

        if (func->is_constructor || func->is_destructor) {
            modl.mark_bad_scope(ScopeLocation::Global, node, logger);
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
    } else if (node->is(NodeKind::Struct) || node->is(NodeKind::Enum) || node->is(NodeKind::Interface)) {

        auto structn = static_cast<Decl*>(node);
        if (structn->name != Identifier::Invalid) {
            context.add_unchecked_decl(structn);
            file->add_composite(structn);
        }
    } else if (node->is(NodeKind::Var)) {
        process_variable(static_cast<Var*>(node));
    } else if (node->is(NodeKind::VarList)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->vars) {
            process_variable(var);
        }
        vlist->vars.clear();
    } else {
        modl.mark_bad_scope(ScopeLocation::Global, node, logger);
    }
}

void acorn::Parser::add_node_to_struct(Struct* structn, Node* node) {

    auto process_var = [structn](Var* var) finline {
        if (var->name != Identifier::Invalid) {
            structn->nspace->add_variable(var);
            var->field_idx = static_cast<uint32_t>(structn->fields.size());
            structn->fields.push_back(var);
            var->structn = structn;
        }
    };

    if (node->is(NodeKind::Var)) {
        process_var(static_cast<Var*>(node));
    } else if (node->is(NodeKind::VarList)) {
        auto vlist = static_cast<VarList*>(node);
        for (auto var : vlist->vars) {
            process_var(var);
        }
        vlist->vars.clear();
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
                structn->needs_copy_call = true;
            } else if (func->is_move_constructor) {
                if (structn->move_constructor) {
                    structn->duplicate_struct_func_infos.push_back(
                        Struct::DuplicateStructFuncInfo{ func, structn->move_constructor });
                }

                structn->move_constructor = func;
                func->structn = structn;
                structn->needs_move_call = true;
            } else if (func->is_constructor) {
                if (func->params.empty()) {
                    structn->default_constructor = func;
                }
                structn->constructors.push_back(func);
                func->structn = structn;
            } else {
                structn->nspace->add_function(func);
                func->structn = structn;
            }
            if (!func->is_generic()) {
                context.add_unchecked_decl(func);
            }
        }
    } else {
        modl.mark_bad_scope(ScopeLocation::Struct, node, logger);
    }
}

void acorn::Parser::add_node_to_interface(Interface* interfacen, Node* node) {
    if (node->is(NodeKind::Func)) {
        auto func = static_cast<Func*>(node);
        func->interfacen = interfacen;
        func->interface_idx = cur_interface->functions.size();
        cur_interface->functions.push_back(func);
    } else {
        modl.mark_bad_scope(ScopeLocation::Interface, node, logger);
    }

}

void acorn::Parser::add_node_to_scope(ScopeStmt* scope, Node* node) {
    if (!node) return;

    if (node->is(NodeKind::VarList)) {
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
    switch (cur_token.kind) {
    case ModifierTokens: {
        uint32_t modifiers = parse_modifiers();
        if (cur_token.is(Token::KwFn)) {
            return parse_function(modifiers, false);
        } else if (cur_token.is(Token::KwConst) && peek_token(0).is(Token::KwFn)) {
            next_token(); // Consuming 'const' token.
            return parse_function(modifiers, true);
        } else if (cur_token.is(Token::Identifier)) {
            return parse_variable_list(modifiers);
        } else if (Token::KwStruct) {
            return parse_struct(modifiers);
        } else if (Token::KwEnum) {
            return parse_enum(modifiers);
        } else if (Token::KwInterface) {
            return parse_interface(modifiers);
        } else {
            error(cur_token, "Expected declaration")
                .end_error(ErrCode::ParseExpectedDeclaration);
            skip_recovery();
            return nullptr;
        }
    }
    case Token::KwFn:
        return parse_function(0, false);
    case Token::KwGenerics: {
        llvm::SmallVector<Generic*> generics;
        parse_generics(generics);

        uint32_t modifiers = parse_modifiers();
        if (cur_token.is(Token::KwFn)) {
            return parse_function(modifiers, false, std::move(generics));
        } else if (cur_token.is(Token::KwConst) && peek_token(0).is(Token::KwFn)) {
            next_token(); // Consuming 'const' token.
            return parse_function(modifiers, true, std::move(generics));
        } else {
            error(cur_token, "Expected declaration")
                .end_error(ErrCode::ParseExpectedDeclaration);
            skip_recovery();
            return nullptr;
        }
    }
    case Token::KwConst: {
        if (peek_token(0).is(Token::KwFn)) {
            next_token(); // Consuming 'const' token.
            return parse_function(0, true);
        } else {
            Expr* expr = parse_assignment_and_expr();
            expect(';');
            return expr;
        }
    }
    case Token::KwStruct:
        return parse_struct(0);
    case Token::KwEnum:
        return parse_enum(0);
    case Token::KwInterface:
        return parse_interface(0);
    case Token::Identifier: {
        if (peek_token(0).is(':')) {
            Var* var = parse_variable(0);
            expect(';');
            return var;
        } else if (peek_token(0).is(',')) {
            return parse_variable_list(0);
        } else {
            Expr* expr = parse_assignment_and_expr();
            expect(';');
            return expr;
        }
    }
    case Token::KwReturn: {
        auto ret = parse_return();
        expect(';');
        return ret;
    }
    case Token::KwIf:
        return parse_if();
    case Token::KwCTIf: {
        parse_comptime_if();
        return nullptr;
    }
    case Token::KwLoop:
        return parse_loop();
    case Token::KwBreak: case Token::KwContinue:
        return parse_loop_control();
    case Token::KwSwitch:
        return parse_switch();
    case Token::KwTry: {
        auto stmt = parse_try();
        expect(';');
        return stmt;
    }
    case Token::KwRaise: {
        auto stmt = parse_raise();
        expect(';');
        return stmt;
    }
    case Token::KwRecover: {
        auto stmt = parse_recover();
        expect(';');
        return stmt;
    }
    case Token::KwCTAborts: {
        next_token(); // Consuming '#aborts' token.
        auto structn = parse_struct(0);
        structn->aborts_error = true;
        return structn;
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
    default: {
        Expr* stmt = parse_assignment_and_expr();
        expect(';');
        return stmt;
    }
    }
}

acorn::Func* acorn::Parser::parse_function(uint32_t modifiers, bool is_const, llvm::SmallVector<Generic*> generics) {

    expect(Token::KwFn);

    Func* func = new_declaration<Func, true>(modifiers, cur_token);
    func->is_constant = is_const;
    func->generics    = std::move(generics);

    // Check for constructor.
    switch (cur_token.kind) {
    case Token::KwNew:
        func->name = context.new_identifier;
        func->is_constructor = true;
        next_token();
        break;
    case Token::KwCopyobj:
        func->name = context.copyobj_identifier;
        func->is_copy_constructor = true;
        func->is_constructor = true;
        next_token();
        break;
    case Token::KwMoveobj:
        func->name = context.moveobj_identifier;
        func->is_move_constructor = true;
        func->is_constructor = true;
        next_token();
        break;
    case Token::KwDelete:
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
            if (cur_token.is(Token::DotDotDot)) {
                func->uses_native_varargs = true;
                next_token();
                break;
            }

            Var* param = parse_variable(0, true);

            param->param_idx = param_idx++;
            if (param->assignment) {
                ++num_default_params;
            } else {
                if (cur_token.is(Token::DotDotDot)) {
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
    if (cur_token.is_not('{') && cur_token.is_not(';') && cur_token.is_not('|') && cur_token.is_not(Token::KwRaises)) {
        expect(Token::Arrow);
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


    if (cur_token.is('|') || cur_token.is(Token::KwRaises)) {
        expect('|', "for raised errors");
        expect(Token::KwRaises);
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
        func->scope->end_loc = prev_token.loc;
    } else {
        expect(';');
    }

    cur_func = prev_func;
    return func;
}

acorn::VarList* acorn::Parser::parse_variable_list(uint32_t modifiers) {

    llvm::SmallVector<Token> name_tokens;
    bool more_variables = false;
    do {

        name_tokens.push_back(cur_token);
        expect_identifier("for variable declaration");

        more_variables = cur_token.is(',');
        if (more_variables) {
            next_token();
        }
    } while (more_variables);

    Type* type;
    expect(':');
    if (cur_token.is_not('=')) {
        type = parse_type();
    } else {
        type = context.auto_type;
    }

    for (Token& name_token : name_tokens) {
        Var* var = new_declaration<Var, true>(modifiers, cur_token);
        if (name_token.is(Token::Identifier)) {
            var->name = Identifier::get(name_token.text());
        }
        var->parsed_type = type;
        var_list.vars.push_back(var);
    }

    if (cur_token.is('=')) {
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


        if (assignments.size() > var_list.vars.size()) {
            if (!context.has_errors()) {
                Expr* first_extra_assignment = assignments[var_list.vars.size()];
                logger.begin_error(expand(first_extra_assignment), "Too many assignments for the number of variables")
                    .end_error(ErrCode::ParseTooManyAssignmentsForNumberOfVariables);
            }
        } else if (assignments.size() != var_list.vars.size()) {

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
            for (size_t i = 0; i < var_list.vars.size(); i++) {
                auto assignment = assignments[i];
                var_list.vars[i]->assignment = assignment;
            }
        }
    }

    return &var_list;
}

acorn::Var* acorn::Parser::parse_variable(uint32_t modifiers, bool may_parse_implicit_ptr) {

    Var* var = new_declaration<Var, true>(modifiers, cur_token);
    var->name = expect_identifier("for variable declaration");

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

    if (cur_token.is('=') || cur_token.is(':')) {
        next_token();

        if (cur_token.is(Token::SubSubSub)) {
            next_token();
            var->should_default_initialize = false;
        } else if (cur_token.is(Token::KwTry)) {
            var->assignment = parse_try();
            var->assignment->tryn->catch_recoveree = var;
        } else {
            var->assignment = parse_expr();
        }
    }

    return var;
}

acorn::Struct* acorn::Parser::parse_struct(uint32_t modifiers) {

    next_token(); // Consuming 'struct' token.

    Struct* structn = new_declaration<Struct, false>(modifiers, cur_token);
    structn->name = expect_identifier("for struct declaration");
    structn->nspace = allocator.alloc_type<Namespace>();
    new (structn->nspace) Namespace(modl, ScopeLocation::Struct);
    structn->struct_type = StructType::create(allocator, structn);

    check_composite_name_conflict_with_imports(structn->name, "struct");

    auto prev_struct = cur_struct;
    cur_struct = structn;

    // Parse interface extensions.
    if (cur_token.is(':') && peek_token(0).is(':')
        && *(cur_token.loc.ptr + 1) == ':' // Check that there is not a space because it expects it to basically be a single joined token.
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
                extension_name_token.loc,
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
        && *(cur_token.loc.ptr + 1) == ':' // Check that there is not a space because it expects it to basically be a single joined token.
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
                                   name_token.loc,
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
        case Token::KwNative: {
            if (modifiers & Modifier::Native)
                error(cur_token, "Duplicate modifier")
                    .end_error(ErrCode::ParseDuplicateModifier);
            modifiers |= Modifier::Native;

            next_token();
            if (cur_token.is('(')) {
                next_token();
                // TODO (maddie): can linknames take utf8?
                if (cur_token.is(Token::StringLiteral)) {

                    linkname = cur_token.text();
                    linkname = linkname.substr(1, linkname.size() - 2);

                    next_token();
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

void acorn::Parser::parse_generics(llvm::SmallVector<acorn::Generic*>& generics) {

    next_token(); // Consuming 'generics' token.

    expect('[');
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
    expect(']');
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

    allow_struct_initializer = false;
    if (cur_token.is(Token::Identifier) && peek_token(0).is(':')) {
        ifn->cond = parse_variable(0);
        if (cur_token.is(';')) {
            next_token();
            ifn->post_variable_cond = parse_expr();
        }
    } else {
        ifn->cond = parse_expr();
    }
    allow_struct_initializer = true;

    ifn->scope = parse_scope("for if");

    if (cur_token.is(Token::KwElIf)) {
        ifn->elseif = parse_if();
    } else if (cur_token.is(Token::KwElse)) {
        next_token(); // Consuming else token.
        if (cur_token.is(Token::KwIf)) {
            // We do not want the user to use 'else if'.
            SourceLoc error_loc = SourceLoc::from_ptrs(prev_token.loc.ptr,
                                                       cur_token.loc.ptr + cur_token.loc.length);
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
            if (stmt->is(NodeKind::VarList)) {
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
                }
                // Check if at the top of the file and parsing imports because if `add_statement`
                // is called it will just complain that the imports are not at the top of file.
                else if (parsing_import_tops && cur_token.is(Token::KwImport)) {
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
                        vlist->vars.clear();
                    }
                }
            }

            break;
        } else {
            if (cur_token.is(Token::KwCTIf)) {
                parse_comptime_if(true, takes_path);
            }
            // Check if at the top of the file and parsing imports because if `add_statement`
            // is called it will just complain that the imports are not at the top of file.
            else if (parsing_import_tops && cur_token.is(Token::KwImport)) {
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
                    vlist->vars.clear();
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
    next_token(); // Consuming 'loop' token.

    Token peek0 = peek_token(0);

    if (cur_token.is(Token::Identifier) && peek0.is(Token::KwIn)) {
        Var* var = new_declaration<Var, false>(0, cur_token);
        var->name = Identifier::get(cur_token.text());
        var->parsed_type = context.auto_type;

        next_token(); // Consuming name token.

        return parse_iterator_loop(loop_token, var, false);
    } else if (cur_token.is(Token::Identifier) && peek0.is('*') && peek_token(1).is(Token::KwIn)) {
        Var* var = new_declaration<Var, false>(0, cur_token);
        var->name = Identifier::get(cur_token.text());
        var->parsed_type = context.auto_type;

        next_token(); // Consuming '*' token.
        next_token(); // Consuming name token.

        return parse_iterator_loop(loop_token, var, true);
    } else if (cur_token.is(Token::Identifier) && peek0.is(':')) {
        Var* var = parse_variable(0);

        if (!var->assignment && cur_token.is(Token::KwIn)) {
            return parse_iterator_loop(loop_token, var, false);
        }

        return parse_range_loop(loop_token, var);

        auto loop = new_node<RangeLoopStmt>(loop_token);
        loop->init_node = var;
    } else if (cur_token.is(';')) {
        return parse_range_loop(loop_token, nullptr);
    } else {

        Expr* cond = nullptr;
        if (cur_token.is_not('{')) {
            allow_struct_initializer = false;
            cond = parse_assignment_and_expr();
            allow_struct_initializer = true;
        }

        if (cur_token.is(';')) {
            return parse_range_loop(loop_token, cond);
        }

        auto loop = new_node<PredicateLoopStmt>(loop_token);
        loop->cond = cond;
        loop->scope = parse_scope();

        return loop;
    }
}

acorn::Node* acorn::Parser::parse_iterator_loop(Token loop_token, Var* var, bool var_as_pointer) {
    next_token(); // Consuming 'in' token.

    auto loop = new_node<IteratorLoopStmt>(loop_token);
    loop->var = var;
    loop->var_auto_ptr = var_as_pointer;

    allow_struct_initializer = false;
    loop->container = parse_expr();
    allow_struct_initializer = true;

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

acorn::Node* acorn::Parser::parse_loop_control() {
    auto loop_control = new_node<LoopControlStmt>(cur_token);
    loop_control->kind = cur_token.is(Token::KwContinue) ? NodeKind::ContinueStmt : NodeKind::BreakStmt;
    next_token(); // Consuming 'break' or 'continue' token.
    return loop_control;
}

acorn::Node* acorn::Parser::parse_switch() {

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

#define ty(t) {                          \
next_token();                            \
if (is_const)                            \
    return type_table.get_const_type(t); \
return t;                                \
}

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
    case Token::KwISize:   ty(context.isize_type);
    case Token::KwUSize:   ty(context.usize_type);
    case Token::KwFloat:   ty(context.float_type);
    case Token::KwDouble:  ty(context.double_type);
    case Token::Identifier: {
        Token name_token = cur_token;
        auto name = Identifier::get(cur_token.text());
        next_token();
        if (cur_token.is_not('$')) {
            if (cur_func && cur_func->is_generic()) {
                auto itr = std::ranges::find_if(cur_func->generics, [name](auto genericn) {
                    return genericn->name == name;
                });
                if (itr != cur_func->generics.end()) {
                    Type* generic_type = (*itr)->type;
                    if (is_const) {
                        generic_type = type_table.get_const_type(generic_type);
                    }
                    return generic_type;
                }
            }

            auto unresolved_type = UnresolvedCompositeType::create(allocator, name, name_token.loc);
            if (is_const) {
                unresolved_type = type_table.get_const_type(unresolved_type);
            }
            return unresolved_type;
        } else {
            next_token();
            auto unresolved_type = UnresolvedEnumValueType::create(allocator, name, name_token.loc);
            if (is_const) {
                unresolved_type = type_table.get_const_type(unresolved_type);
            }
            return unresolved_type;
        }
    }
    case Token::KwFn: {
        next_token();

        bool uses_native_varargs = false;
        llvm::SmallVector<Type*> param_types;
        expect('(', "for function type");
        if (cur_token.is_not(')')) {
            bool more_param_types = false;
            do {

                if (cur_token.is(Token::DotDotDot)) {
                    next_token();
                    uses_native_varargs = true;
                    break;
                }

                // Allow for an optional name for the parameter.
                if (cur_token.is(Token::Identifier) && peek_token(0).is(':')) {
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
        if (cur_token.is(Token::Arrow)) {
            next_token();
            return_type = parse_type();
        } else {
            return_type = context.void_type;
        }

        llvm::SmallVector<RaisedError> raised_errors;
        if (cur_token.is('|')) {
            next_token();
            expect(Token::KwRaises);
            parse_raised_errors(raised_errors);
        }

        return type_table.get_function_type(return_type, std::move(param_types), std::move(raised_errors), uses_native_varargs);
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
            if (peek_token(0).is(Token::DotDot)) {
                next_token(); // Consuming '[' token.
                next_token(); // Consuming '..' token.
                expect(']', "for slice type");
                type = type_table.get_slice_type(type);
            } else {
                llvm::SmallVector<ArrayTypeExpr, 8> arr_lengths;

                while (cur_token.is('[') && !peek_token(0).is(Token::DotDot)) {
                    ++bracket_count;
                    next_token();

                    auto arr_expr = cur_token.is_not(']')
                        ? ArrayTypeExpr{ .expr = parse_expr()           , .has_expr = true  }
                        : ArrayTypeExpr{ .empty_elm_loc = prev_token.loc, .has_expr = false };
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

        bool resolvable = arr_expr.expr->is(NodeKind::Number);
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
        if (cur_token.is(Token::KwTry)) {
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

acorn::Expr* acorn::Parser::parse_try() {

    Try* tryn = new_node<Try>(cur_token);

    next_token(); // Consuming 'try' token.

    bool catches_error = cur_token.is(Token::Identifier) && peek_token(0).is(':');
    if (catches_error) {
        tryn->caught_var = new_declaration<Var, true>(0, cur_token);
        tryn->caught_var->name = Identifier::get(cur_token.text());

        next_token(); // Consume the name token.
        next_token(); // Consume the ':' token.
    }

    Expr* caught_expr = parse_expr();
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

acorn::Expr* acorn::Parser::new_binary_op(Token op_tok, Expr* lhs, Expr* rhs) {
    BinOp* bin_op = new_node<BinOp>(op_tok);
    bin_op->op = op_tok.kind;
    bin_op->lhs = lhs;
    bin_op->rhs = rhs;
    return bin_op;
}

std::pair<acorn::Token, acorn::Token> acorn::Parser::split_number_from_sign(Token token) {

    tokkind op_sign = *token.loc.ptr;
    Token op = Token(op_sign, SourceLoc{ token.loc.ptr, 1 });

    token.loc.ptr += 1;
    token.loc.length -= 1;

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
            static_cast<uint16_t>(e - s),
            op.loc.ptr,
            op.loc.length
        };

        lhs->trivially_reassignable = lhs->trivially_reassignable && rhs->trivially_reassignable;
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
        if (rval < 0 || rval == 0 || ((rval - 1) > (T)to_type->get_number_of_bits())) {
            // These are shift error cases handled during sema.
            return new_binary_op(op, lhs, rhs);
        }
        return calc(lval << rval);
    }
    case Token::GtGt: {
        if (rval < 0 || rval == 0 || (rval - 1) > (T)to_type->get_number_of_bits()) {
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
        case TypeKind::UInt32:
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
        return parse_expr_trail(parse_function_call(term));
    } else if (cur_token.is('.')) {
        auto dot = new_node<DotOperator>(cur_token);
        next_token(); // Consuming '.' token.

        dot->site = term;
        dot->ident = expect_identifier("for '.' operator");

        return parse_expr_trail(dot);
    } else if (cur_token.is(Token::AddAdd) || cur_token.is(Token::SubSub)) {
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
    } else if (cur_token.is('[')) {

        llvm::SmallVector<ArrayTypeExpr, 8> indexes;
        llvm::SmallVector<SourceLoc, 8>     bracket_locations;

        while (cur_token.is('[')) {
            if (peek_token(0).is(Token::DotDot)) {
                break;
            }

            bracket_locations.push_back(cur_token.loc);
            next_token(); // Consuming '[' token.
            ++bracket_count;

            indexes.push_back({ .expr = parse_expr(), .has_expr = true });

            expect(']');
            --bracket_count;
        }

        if (term->is(NodeKind::IdentRef)) {
            if (peek_if_expr_is_type(cur_token, peek_token(0))) {
                auto ref = static_cast<IdentRef*>(term);

                auto type = UnresolvedCompositeType::create(allocator, ref->ident, ref->loc, false);

                type = construct_array_type(type, indexes);
                type = parse_trailing_type_info(type);

                TypeExpr* type_expr = new_node<TypeExpr>(cur_token);
                type_expr->parsed_expr_type = type;

                SourceLoc start_loc = ref->loc;
                SourceLoc end_loc   = prev_token.loc;

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

acorn::Expr* acorn::Parser::parse_function_call(Expr* site) {
    FuncCall* call = new_node<FuncCall>(cur_token);
    call->site = site;
    parse_function_call_args(call->args, call->non_named_args_offset);
    return call;
}

acorn::Expr* acorn::Parser::parse_generic_function_bind_call() {
    GenericBindFuncCall* call = new_node<GenericBindFuncCall>(cur_token);
    call->binds_generics = true;
    call->ident = Identifier::get(cur_token.text());
    next_token();
    next_token(); // Consuming '!' token.
    parse_function_call_args(call->args, call->non_named_args_offset);
    return call;
}

void acorn::Parser::parse_function_call_args(llvm::SmallVector<Expr*>& args, size_t& non_named_args_offset) {

    next_token(); // Consuming '(' token.
    bool prev_allow_struct_initializer = allow_struct_initializer;
    allow_struct_initializer = true;
    ++paren_count;
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
    allow_struct_initializer = prev_allow_struct_initializer;
    expect(')', "for function call");
}

acorn::Expr* acorn::Parser::parse_term() {
    switch (cur_token.kind) {
    case Token::IntLiteral:           return parse_int_literal();
    case Token::HexLiteral:           return parse_hex_literal();
    case Token::BinLiteral:           return parse_bin_literal();
    case Token::OctLiteral:           return parse_oct_literal();
    case Token::FloatLiteral:         return parse_float_literal();
    case Token::DoubleLiteral:        return parse_double_literal();
    case Token::StringLiteral:        return parse_string_literal();
    case Token::CharLiteral:          return parse_char_literal();
    case Token::InvalidStringLiteral:
    case Token::InvalidCharLiteral:
    case Token::InvalidNumberLiteral: {
        next_token();
        return new_node<InvalidExpr>(cur_token);
    }
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
    case Token::Identifier: {

        auto peek0 = peek_token(0);
        auto peek1 = peek_token(1);
        if (peek_if_expr_is_type(peek0, peek1)) {
            return parse_type_expr();
        }

        if (peek0.is('!') && peek1.is('(')) {
            return parse_expr_trail(parse_generic_function_bind_call());
        }

        IdentRef* ref = new_node<IdentRef>(cur_token);
        ref->ident = Identifier::get(cur_token.text());

        next_token(); // Consuming the identifier.

        if (allow_struct_initializer && cur_token.is('{')) {
            return parse_struct_initializer(ref);
        }

        return ref;
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
    // Unary operators.
    case '+': case '-': case '~': case '!':
    case '&': case Token::AddAdd: case Token::SubSub:
    case '*': {
        Token unary_token = cur_token;
        next_token(); // Consuming the unary token.

        bool unary_on_num_literal = cur_token.is(Token::IntLiteral);
        Token after_op_token = cur_token;
        Expr* expr = parse_expr_trail();

        if (unary_token.kind == '+' && expr->is(NodeKind::Number)) {
            return expr; // + has no effect on value.
        } else if (unary_token.kind == '-' && expr->is(NodeKind::Number)) {
            Number* num = static_cast<Number*>(expr);

            switch (num->type->get_kind()) {
            case TypeKind::UInt64:                         num->value_u64 = -num->value_u64; return num;
            case TypeKind::UInt32:                         num->value_u32 = -num->value_u32; return num;
            case TypeKind::UInt16: case TypeKind::Char16:  num->value_u16 = -num->value_u16; return num;
            case TypeKind::UInt8:  case TypeKind::Char:    num->value_u8  = -num->value_u8;  return num;
            case TypeKind::Int64:                          num->value_s64 = -num->value_s64; return num;
            case TypeKind::Int32:  case TypeKind::Int:     num->value_s32 = -num->value_s32; return num;
            case TypeKind::Int16:                          num->value_s16 = -num->value_s16; return num;
            case TypeKind::Int8:                           num->value_s8  = -num->value_s8;  return num;
            case TypeKind::Float:                          num->value_f32 = -num->value_f32; return num;
            case TypeKind::Double:                         num->value_f64 = -num->value_f64; return num;
            }
        } else if (unary_token.kind == '~' && expr->is(NodeKind::Number) &&
                   expr->type->is_integer()) {
            Number* num = static_cast<Number*>(expr);

            switch (num->type->get_kind()) {
            case TypeKind::UInt64:                        num->value_u64 = ~num->value_u64; return num;
            case TypeKind::UInt32:                        num->value_u32 = ~num->value_u32; return num;
            case TypeKind::UInt16: case TypeKind::Char16: num->value_u16 = ~num->value_u16; return num;
            case TypeKind::UInt8:  case TypeKind::Char:   num->value_u8  = ~num->value_u8;  return num;
            case TypeKind::Int64:                         num->value_s64 = ~num->value_s64; return num;
            case TypeKind::Int32:  case TypeKind::Int:    num->value_s32 = ~num->value_s32; return num;
            case TypeKind::Int16:                         num->value_s16 = ~num->value_s16; return num;
            case TypeKind::Int8:                          num->value_s8  = ~num->value_s8;  return num;
            }
        }

        UnaryOp* unary_op = new_node<UnaryOp>(unary_token);
        unary_op->op = unary_token.kind;
        unary_op->expr = expr;

        return unary_op;
    }
    case '(': {
        ++paren_count;
        bool prev_allow_struct_initializer = allow_struct_initializer;
        allow_struct_initializer = true;
        next_token();
        Expr* expr = parse_expr();
        expect(')');
        --paren_count;
        allow_struct_initializer = prev_allow_struct_initializer;
        return expr;
    }
    case Token::KwAs: {
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
    case Token::KwSizeof: {
        SizeOf* sof = new_node<SizeOf>(cur_token);
        sof->trivially_reassignable = true;
        next_token();
        ++paren_count;
        bool prev_allow_struct_initializer = allow_struct_initializer;
        allow_struct_initializer = true;
        expect('(');
        sof->value = parse_expr();
        expect(')');
        --paren_count;
        allow_struct_initializer = prev_allow_struct_initializer;
        return sof;
    }
    case Token::KwMoveobj: {
        MoveObj* move_obj = new_node<MoveObj>(cur_token);
        next_token();
        ++paren_count;
        bool prev_allow_struct_initializer = allow_struct_initializer;
        allow_struct_initializer = true;
        expect('(');
        move_obj->value = parse_expr();
        expect(')');
        --paren_count;
        allow_struct_initializer = prev_allow_struct_initializer;
        return move_obj;
    }
    case Token::KwThis: {
        This* thisn = new_node<This>(cur_token);
        next_token();
        return thisn;
    }
    case Token::KwCTTypeInfo: {
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
    case Token::KwFn: {
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
                case Token::IntLiteral:
                case Token::BinLiteral:
                case Token::HexLiteral:
                case Token::OctLiteral:
                case Token::CharLiteral:
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

acorn::Expr* acorn::Parser::parse_struct_initializer(IdentRef* ref) {

    auto initializer = new_node<StructInitializer>(cur_token);
    initializer->ref = ref;

    next_token(); // Consuming '{' token.

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

acorn::Expr* acorn::Parser::parse_type_expr() {
    Token start_token = cur_token;
    TypeExpr* type_expr = new_node<TypeExpr>(cur_token);
    type_expr->parsed_expr_type = parse_type();

    SourceLoc start_loc = start_token.loc;
    SourceLoc end_loc   = prev_token.loc;

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

        if (prev_token.is(Token::InvalidStringLiteral)) {
            auto text = prev_token.text();
            if (text.back() != '\"') {
                // The string was missing its ending " so checking if the
                // character we expect happens to be in the string.
                if (kind <= 128) {
                    if (static_cast<tokkind>(text.back()) == kind) {
                        return false;
                    }
                }
            }
        }

        logger.begin_error(prev_token.loc, "Expected%s '%s' token%s", closing_msg, str_kind, fixed_for_msg)
              .add_arrow_msg(Logger::CaretPlacement::After, arrow_msg)
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
        case '$':
            return true;
        default:
            break;
        }
    } else if (tok0.is('[') && tok1.is(Token::DotDot)) {
        return true;
    }
    return false;
}

void acorn::Parser::skip_recovery(bool stop_on_modifiers) {
    while (true) {
        switch (cur_token.kind) {
        case Token::EOB:
        case ';':
        case '{':
        case '}':
        case ',':
        case Token::KwImport:
        case Token::KwIf:
        case Token::KwReturn:
        case Token::KwCTIf:
        case Token::KwStruct:
        case Token::KwEnum:
        case Token::KwInterface:
        case Token::KwSwitch:
        case Token::KwRaise:
        case Token::KwCTAborts:
        case Token::KwTry:
        case Token::Token::KwRecover:
        case ModifierTokens:
            if (stop_on_modifiers) {
                return;
            }
            next_token();
            break;
        case Token::KwConst:
            if (peek_token(0).is(Token::KwFn)) {
                return;
            }
            next_token();
            break;
        case Token::KwFn:
            switch (peek_token(0).kind) {
            case Token::Identifier:
            case Token::KwNew:
            case Token::KwMoveobj:
            case Token::KwCopyobj:
            case Token::KwDelete:
                return;
            }
            next_token();
            break;
        case Token::Identifier:
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
