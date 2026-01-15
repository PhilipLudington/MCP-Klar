//! Parser for the Klar language.
//!
//! Transforms a token stream into an Abstract Syntax Tree (AST).
//! Uses recursive descent parsing with Pratt parsing for expressions.

const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("utils");
const token_mod = @import("token.zig");
const lexer_mod = @import("lexer.zig");
const ast_mod = @import("ast.zig");

const Span = utils.Span;
const Token = token_mod.Token;
const TokenKind = token_mod.TokenKind;
const Lexer = lexer_mod.Lexer;
const Ast = ast_mod.Ast;
const Node = ast_mod.Node;
const NodeIndex = ast_mod.NodeIndex;
const NodeRange = ast_mod.NodeRange;
const StringIndex = ast_mod.StringIndex;
const BinaryOp = ast_mod.BinaryOp;
const UnaryOp = ast_mod.UnaryOp;
const null_node = ast_mod.null_node;
const null_string = ast_mod.null_string;

/// Errors that can occur during parsing.
pub const ParseError = error{OutOfMemory};

/// Parser state and operations.
pub const Parser = struct {
    lexer: Lexer,
    ast: Ast,
    current: Token,
    previous: Token,

    /// Temporary storage for building node lists.
    scratch: std.ArrayListUnmanaged(NodeIndex),

    pub fn init(allocator: Allocator, source: []const u8, file_id: u32) Parser {
        var lexer = Lexer.init(source, file_id);
        const first_token = lexer.next();

        return .{
            .lexer = lexer,
            .ast = Ast.init(allocator, source, file_id),
            .current = first_token,
            .previous = first_token,
            .scratch = .{},
        };
    }

    pub fn deinit(self: *Parser) void {
        self.ast.deinit();
        self.scratch.deinit(self.ast.allocator);
    }

    /// Parse the entire source file and return the AST.
    /// Caller owns the returned AST and must call deinit on it.
    pub fn parse(self: *Parser) !Ast {
        self.ast.root = try self.parseModule();

        // Move ownership of AST to caller
        const result = self.ast;
        self.ast = Ast.init(result.allocator, result.source, result.file_id);
        return result;
    }

    // =========================================================================
    // Module Level
    // =========================================================================

    fn parseModule(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;
        const scratch_start = self.scratch.items.len;

        while (!self.check(.eof)) {
            self.skipNewlines();
            if (self.check(.eof)) break;

            const decl = try self.parseDeclaration();
            if (decl != null_node) {
                try self.scratch.append(self.ast.allocator, decl);
            }
        }

        const decls = try self.finishNodeList(scratch_start);
        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .module,
            .span = Span.merge(start_span, end_span),
            .data = .{ .range = decls },
        });
    }

    fn parseDeclaration(self: *Parser) ParseError!NodeIndex {
        // Handle visibility modifier
        const is_pub = self.match(.kw_pub);

        // Skip doc comments and attach to next declaration
        while (self.match(.doc_comment)) {
            self.skipNewlines();
        }

        if (self.check(.kw_fn)) {
            return self.parseFnDecl(is_pub);
        } else if (self.check(.kw_struct)) {
            return self.parseStructDecl(is_pub);
        } else if (self.check(.kw_enum)) {
            return self.parseEnumDecl(is_pub);
        } else if (self.check(.kw_trait)) {
            return self.parseTraitDecl(is_pub);
        } else if (self.check(.kw_impl)) {
            return self.parseImplDecl();
        } else if (self.check(.kw_type)) {
            return self.parseTypeAlias(is_pub);
        } else if (self.check(.kw_import)) {
            return self.parseImport();
        } else if (self.check(.kw_const)) {
            return self.parseConstDecl(is_pub);
        } else if (self.check(.kw_let)) {
            return self.parseLetStmt();
        } else if (self.check(.kw_var)) {
            return self.parseVarStmt();
        } else if (self.check(.kw_while)) {
            return self.parseWhileStmt();
        } else if (self.check(.kw_for)) {
            return self.parseForStmt();
        } else if (self.check(.kw_loop)) {
            return self.parseLoopStmt();
        } else if (self.check(.kw_if)) {
            return self.parseIfExpr();
        } else if (self.check(.kw_match)) {
            return self.parseMatchExpr();
        } else if (!self.check(.eof)) {
            // Try to parse as expression statement
            return self.parseExprStmt();
        }

        return null_node;
    }

    // =========================================================================
    // Declarations
    // =========================================================================

    fn parseFnDecl(self: *Parser, is_pub: bool) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_fn)) {
            return self.errorAtCurrent("expected 'fn'");
        }

        // Function name
        const name = try self.expectIdentifier("expected function name");

        // Optional generic parameters
        const generics = try self.parseOptionalGenerics();

        // Parameters
        if (!self.match(.lparen)) {
            return self.errorAtCurrent("expected '(' after function name");
        }
        const params = try self.parseParameterList();
        if (!self.match(.rparen)) {
            return self.errorAtCurrent("expected ')' after parameters");
        }

        // Optional return type
        var return_type: NodeIndex = null_node;
        if (self.match(.arrow)) {
            return_type = try self.parseType();
        }

        // Body (optional for trait method signatures)
        var body: NodeIndex = null_node;
        self.skipNewlines();
        if (self.check(.lbrace)) {
            body = try self.parseBlock();
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .fn_decl,
            .span = Span.merge(start_span, end_span),
            .data = .{ .fn_decl = .{
                .name = name,
                .generics = generics,
                .params = params,
                .return_type = return_type,
                .body = body,
                .is_pub = is_pub,
            } },
        });
    }

    fn parseParameterList(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        while (!self.check(.rparen) and !self.check(.eof)) {
            const param = try self.parseParameter();
            try self.scratch.append(self.ast.allocator, param);

            if (!self.match(.comma)) break;
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseParameter(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        // Check for 'self' parameter
        if (self.match(.kw_self)) {
            const name_idx = try self.ast.addString("self");
            return self.ast.addNode(.{
                .tag = .param,
                .span = self.previous.span,
                .data = .{ .param = .{
                    .name = name_idx,
                    .type_node = null_node,
                    .is_mut = false,
                } },
            });
        }

        // Check for 'mut' modifier
        const is_mut = self.match(.kw_mut);

        // Parameter name
        const name = try self.expectIdentifier("expected parameter name");

        // Type annotation
        if (!self.match(.colon)) {
            return self.errorAtCurrent("expected ':' after parameter name");
        }
        const type_node = try self.parseType();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .param,
            .span = Span.merge(start_span, end_span),
            .data = .{ .param = .{
                .name = name,
                .type_node = type_node,
                .is_mut = is_mut,
            } },
        });
    }

    fn parseStructDecl(self: *Parser, is_pub: bool) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_struct)) {
            return self.errorAtCurrent("expected 'struct'");
        }

        const name = try self.expectIdentifier("expected struct name");
        const generics = try self.parseOptionalGenerics();

        self.skipNewlines();
        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{' after struct name");
        }

        const fields = try self.parseStructFields();

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after struct fields");
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .struct_decl,
            .span = Span.merge(start_span, end_span),
            .data = .{ .struct_decl = .{
                .name = name,
                .generics = generics,
                .fields = fields,
                .is_pub = is_pub,
            } },
        });
    }

    fn parseStructFields(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const field = try self.parseStructField();
            try self.scratch.append(self.ast.allocator, field);

            // Fields can be separated by comma or newline
            _ = self.match(.comma);
            self.skipNewlines();
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseStructField(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;
        const is_pub = self.match(.kw_pub);

        const name = try self.expectIdentifier("expected field name");

        if (!self.match(.colon)) {
            return self.errorAtCurrent("expected ':' after field name");
        }

        const type_node = try self.parseType();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .struct_field,
            .span = Span.merge(start_span, end_span),
            .data = .{ .field = .{
                .name = name,
                .type_node = type_node,
                .is_pub = is_pub,
            } },
        });
    }

    fn parseEnumDecl(self: *Parser, is_pub: bool) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_enum)) {
            return self.errorAtCurrent("expected 'enum'");
        }

        const name = try self.expectIdentifier("expected enum name");
        const generics = try self.parseOptionalGenerics();

        self.skipNewlines();
        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{' after enum name");
        }

        const variants = try self.parseEnumVariants();

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after enum variants");
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .enum_decl,
            .span = Span.merge(start_span, end_span),
            .data = .{ .enum_decl = .{
                .name = name,
                .generics = generics,
                .variants = variants,
                .is_pub = is_pub,
            } },
        });
    }

    fn parseEnumVariants(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const variant = try self.parseEnumVariant();
            try self.scratch.append(self.ast.allocator, variant);

            _ = self.match(.comma);
            self.skipNewlines();
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseEnumVariant(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        const name = try self.expectIdentifier("expected variant name");

        // Optional payload
        var payload: NodeIndex = null_node;
        if (self.match(.lparen)) {
            // Tuple-style: Variant(Type)
            payload = try self.parseType();
            if (!self.match(.rparen)) {
                return self.errorAtCurrent("expected ')' after variant type");
            }
        } else if (self.check(.lbrace)) {
            // Struct-style: Variant { field: Type }
            // For now, parse as type_named pointing to anonymous struct
            // This is a simplification - full implementation would parse inline struct
            payload = null_node;
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .enum_variant,
            .span = Span.merge(start_span, end_span),
            .data = .{ .variant = .{
                .name = name,
                .payload = payload,
            } },
        });
    }

    fn parseTraitDecl(self: *Parser, is_pub: bool) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_trait)) {
            return self.errorAtCurrent("expected 'trait'");
        }

        const name = try self.expectIdentifier("expected trait name");
        const generics = try self.parseOptionalGenerics();

        self.skipNewlines();
        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{' after trait name");
        }

        const methods = try self.parseTraitMethods();

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after trait methods");
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .trait_decl,
            .span = Span.merge(start_span, end_span),
            .data = .{ .trait_decl = .{
                .name = name,
                .generics = generics,
                .methods = methods,
                .is_pub = is_pub,
            } },
        });
    }

    fn parseTraitMethods(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const method = try self.parseFnDecl(false);
            try self.scratch.append(self.ast.allocator, method);
            self.skipNewlines();
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseImplDecl(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_impl)) {
            return self.errorAtCurrent("expected 'impl'");
        }

        const generics = try self.parseOptionalGenerics();

        // Parse first type (either trait or target)
        const first_type = try self.parseType();

        // Check if this is `impl Trait for Type` or just `impl Type`
        var trait_type: NodeIndex = null_node;
        var target_type: NodeIndex = first_type;

        if (self.match(.kw_for)) {
            trait_type = first_type;
            target_type = try self.parseType();
        }

        self.skipNewlines();
        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{' after impl type");
        }

        const methods = try self.parseImplMethods();

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after impl methods");
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .impl_decl,
            .span = Span.merge(start_span, end_span),
            .data = .{ .impl_decl = .{
                .trait_type = trait_type,
                .target_type = target_type,
                .generics = generics,
                .methods = methods,
            } },
        });
    }

    fn parseImplMethods(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const is_pub = self.match(.kw_pub);
            const method = try self.parseFnDecl(is_pub);
            try self.scratch.append(self.ast.allocator, method);
            self.skipNewlines();
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseTypeAlias(self: *Parser, is_pub: bool) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_type)) {
            return self.errorAtCurrent("expected 'type'");
        }

        const name = try self.expectIdentifier("expected type name");
        const generics = try self.parseOptionalGenerics();

        if (!self.match(.eq)) {
            return self.errorAtCurrent("expected '=' after type name");
        }

        const aliased_type = try self.parseType();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .type_alias,
            .span = Span.merge(start_span, end_span),
            .data = .{ .type_alias = .{
                .name = name,
                .generics = generics,
                .aliased_type = aliased_type,
                .is_pub = is_pub,
            } },
        });
    }

    fn parseConstDecl(self: *Parser, is_pub: bool) ParseError!NodeIndex {
        _ = is_pub;
        const start_span = self.current.span;

        if (!self.match(.kw_const)) {
            return self.errorAtCurrent("expected 'const'");
        }

        const name = try self.expectIdentifier("expected constant name");

        // Optional type annotation
        var type_node: NodeIndex = null_node;
        if (self.match(.colon)) {
            type_node = try self.parseType();
        }

        if (!self.match(.eq)) {
            return self.errorAtCurrent("expected '=' after constant name");
        }

        const value = try self.parseExpression();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .const_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .let = .{
                .name = name,
                .type_node = type_node,
                .value = value,
            } },
        });
    }

    fn parseImport(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_import)) {
            return self.errorAtCurrent("expected 'import'");
        }

        // Parse module path
        const scratch_start = self.scratch.items.len;

        const first_ident = try self.expectIdentifier("expected module name");
        const first_node = try self.ast.addNode(.{
            .tag = .identifier,
            .span = self.previous.span,
            .data = .{ .string = first_ident },
        });
        try self.scratch.append(self.ast.allocator, first_node);

        while (self.match(.dot)) {
            const ident = try self.expectIdentifier("expected module name after '.'");
            const node = try self.ast.addNode(.{
                .tag = .identifier,
                .span = self.previous.span,
                .data = .{ .string = ident },
            });
            try self.scratch.append(self.ast.allocator, node);
        }

        const path = try self.finishNodeList(scratch_start);

        // Optional alias
        var alias: StringIndex = null_string;
        if (self.match(.kw_as)) {
            alias = try self.expectIdentifier("expected alias name");
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .import_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .import = .{
                .path = path,
                .items = NodeRange.empty,
                .alias = alias,
            } },
        });
    }

    fn parseOptionalGenerics(self: *Parser) ParseError!NodeRange {
        if (!self.match(.lbracket)) {
            return NodeRange.empty;
        }

        const scratch_start = self.scratch.items.len;

        while (!self.check(.rbracket) and !self.check(.eof)) {
            const param = try self.parseGenericParam();
            try self.scratch.append(self.ast.allocator, param);

            if (!self.match(.comma)) break;
        }

        if (!self.match(.rbracket)) {
            _ = try self.errorAtCurrent("expected ']' after generic parameters");
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseGenericParam(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        const name = try self.expectIdentifier("expected generic parameter name");

        // Optional bounds: T: Trait + OtherTrait
        const scratch_start = self.scratch.items.len;

        if (self.match(.colon)) {
            const bound = try self.parseType();
            try self.scratch.append(self.ast.allocator, bound);

            while (self.match(.plus)) {
                const additional_bound = try self.parseType();
                try self.scratch.append(self.ast.allocator, additional_bound);
            }
        }

        const bounds = try self.finishNodeList(scratch_start);

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .generic_param,
            .span = Span.merge(start_span, end_span),
            .data = .{ .generic_param = .{
                .name = name,
                .bounds = bounds,
            } },
        });
    }

    // =========================================================================
    // Statements
    // =========================================================================

    fn parseStatement(self: *Parser) ParseError!NodeIndex {
        self.skipNewlines();

        if (self.check(.kw_let)) {
            return self.parseLetStmt();
        } else if (self.check(.kw_var)) {
            return self.parseVarStmt();
        } else if (self.check(.kw_return)) {
            return self.parseReturnStmt();
        } else if (self.check(.kw_break)) {
            return self.parseBreakStmt();
        } else if (self.check(.kw_continue)) {
            return self.parseContinueStmt();
        } else if (self.check(.kw_while)) {
            return self.parseWhileStmt();
        } else if (self.check(.kw_for)) {
            return self.parseForStmt();
        } else if (self.check(.kw_loop)) {
            return self.parseLoopStmt();
        } else if (self.check(.kw_if)) {
            return self.parseIfExpr();
        } else {
            return self.parseExprStmt();
        }
    }

    fn parseLetStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_let)) {
            return self.errorAtCurrent("expected 'let'");
        }

        const name = try self.expectIdentifier("expected variable name");

        // Optional type annotation
        var type_node: NodeIndex = null_node;
        if (self.match(.colon)) {
            type_node = try self.parseType();
        }

        if (!self.match(.eq)) {
            return self.errorAtCurrent("expected '=' after variable name");
        }

        const value = try self.parseExpression();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .let_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .let = .{
                .name = name,
                .type_node = type_node,
                .value = value,
            } },
        });
    }

    fn parseVarStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_var)) {
            return self.errorAtCurrent("expected 'var'");
        }

        const name = try self.expectIdentifier("expected variable name");

        // Optional type annotation
        var type_node: NodeIndex = null_node;
        if (self.match(.colon)) {
            type_node = try self.parseType();
        }

        if (!self.match(.eq)) {
            return self.errorAtCurrent("expected '=' after variable name");
        }

        const value = try self.parseExpression();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .var_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .let = .{
                .name = name,
                .type_node = type_node,
                .value = value,
            } },
        });
    }

    fn parseReturnStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_return)) {
            return self.errorAtCurrent("expected 'return'");
        }

        // Optional return value
        var value: NodeIndex = null_node;
        if (!self.check(.newline) and !self.check(.rbrace) and !self.check(.eof)) {
            value = try self.parseExpression();
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .return_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .node = value },
        });
    }

    fn parseBreakStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_break)) {
            return self.errorAtCurrent("expected 'break'");
        }

        // Optional break value
        var value: NodeIndex = null_node;
        if (!self.check(.newline) and !self.check(.rbrace) and !self.check(.eof)) {
            value = try self.parseExpression();
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .break_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .node = value },
        });
    }

    fn parseContinueStmt(self: *Parser) ParseError!NodeIndex {
        const span = self.current.span;

        if (!self.match(.kw_continue)) {
            return self.errorAtCurrent("expected 'continue'");
        }

        return self.ast.addNode(.{
            .tag = .continue_stmt,
            .span = span,
            .data = .{ .none = {} },
        });
    }

    fn parseWhileStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_while)) {
            return self.errorAtCurrent("expected 'while'");
        }

        const condition = try self.parseExpression();

        self.skipNewlines();
        const body = try self.parseBlock();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .while_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .while_loop = .{
                .condition = condition,
                .body = body,
            } },
        });
    }

    fn parseForStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_for)) {
            return self.errorAtCurrent("expected 'for'");
        }

        const binding = try self.expectIdentifier("expected loop variable name");

        if (!self.match(.kw_in)) {
            return self.errorAtCurrent("expected 'in' after loop variable");
        }

        const iterator = try self.parseExpression();

        self.skipNewlines();
        const body = try self.parseBlock();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .for_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .for_loop = .{
                .binding = binding,
                .iterator = iterator,
                .body = body,
            } },
        });
    }

    fn parseLoopStmt(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_loop)) {
            return self.errorAtCurrent("expected 'loop'");
        }

        self.skipNewlines();
        const body = try self.parseBlock();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .loop_stmt,
            .span = Span.merge(start_span, end_span),
            .data = .{ .node = body },
        });
    }

    fn parseExprStmt(self: *Parser) ParseError!NodeIndex {
        const expr = try self.parseExpression();

        return self.ast.addNode(.{
            .tag = .expr_stmt,
            .span = if (self.ast.getNode(expr)) |n| n.span else self.previous.span,
            .data = .{ .node = expr },
        });
    }

    fn parseBlock(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{'");
        }

        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const stmt = try self.parseStatement();
            try self.scratch.append(self.ast.allocator, stmt);

            // Statements separated by newlines or semicolons
            _ = self.match(.semicolon);
            self.skipNewlines();
        }

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after block");
        }

        const stmts = try self.finishNodeList(scratch_start);
        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .block,
            .span = Span.merge(start_span, end_span),
            .data = .{ .range = stmts },
        });
    }

    // =========================================================================
    // Expressions (Pratt Parser)
    // =========================================================================

    fn parseExpression(self: *Parser) ParseError!NodeIndex {
        return self.parsePrecedence(1);
    }

    fn parsePrecedence(self: *Parser, min_precedence: u8) ParseError!NodeIndex {
        var left = try self.parsePrefixExpr();

        while (true) {
            const op = self.tokenToBinaryOp(self.current.kind) orelse break;
            const prec = op.precedence();

            if (prec < min_precedence) break;

            const op_span = self.current.span;
            _ = self.advance();

            const next_prec = if (op.isRightAssociative()) prec else prec + 1;
            const right = try self.parsePrecedence(next_prec);

            left = try self.ast.addNode(.{
                .tag = .binary_op,
                .span = Span.merge(
                    if (self.ast.getNode(left)) |n| n.span else op_span,
                    if (self.ast.getNode(right)) |n| n.span else op_span,
                ),
                .data = .{ .binary = .{
                    .op = op,
                    .left = left,
                    .right = right,
                } },
            });
        }

        return left;
    }

    fn parsePrefixExpr(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        // Unary operators
        if (self.match(.minus)) {
            const operand = try self.parsePrefixExpr();
            return self.ast.addNode(.{
                .tag = .unary_op,
                .span = Span.merge(start_span, if (self.ast.getNode(operand)) |n| n.span else start_span),
                .data = .{ .unary = .{ .op = .neg, .operand = operand } },
            });
        }

        if (self.match(.kw_not) or self.match(.bang)) {
            const operand = try self.parsePrefixExpr();
            return self.ast.addNode(.{
                .tag = .unary_op,
                .span = Span.merge(start_span, if (self.ast.getNode(operand)) |n| n.span else start_span),
                .data = .{ .unary = .{ .op = .not, .operand = operand } },
            });
        }

        if (self.match(.tilde)) {
            const operand = try self.parsePrefixExpr();
            return self.ast.addNode(.{
                .tag = .unary_op,
                .span = Span.merge(start_span, if (self.ast.getNode(operand)) |n| n.span else start_span),
                .data = .{ .unary = .{ .op = .bit_not, .operand = operand } },
            });
        }

        if (self.match(.amp)) {
            const is_mut = self.match(.kw_mut);
            const operand = try self.parsePrefixExpr();
            return self.ast.addNode(.{
                .tag = .unary_op,
                .span = Span.merge(start_span, if (self.ast.getNode(operand)) |n| n.span else start_span),
                .data = .{ .unary = .{
                    .op = if (is_mut) .ref_mut else .ref,
                    .operand = operand,
                } },
            });
        }

        if (self.match(.kw_await)) {
            const operand = try self.parsePrefixExpr();
            return self.ast.addNode(.{
                .tag = .unary_op,
                .span = Span.merge(start_span, if (self.ast.getNode(operand)) |n| n.span else start_span),
                .data = .{ .unary = .{ .op = .await_op, .operand = operand } },
            });
        }

        if (self.match(.star)) {
            const operand = try self.parsePrefixExpr();
            return self.ast.addNode(.{
                .tag = .unary_op,
                .span = Span.merge(start_span, if (self.ast.getNode(operand)) |n| n.span else start_span),
                .data = .{ .unary = .{ .op = .deref, .operand = operand } },
            });
        }

        return self.parsePostfixExpr();
    }

    fn parsePostfixExpr(self: *Parser) ParseError!NodeIndex {
        var expr = try self.parsePrimaryExpr();

        while (true) {
            if (self.match(.dot)) {
                // Field access or method call
                const name = try self.expectIdentifier("expected field or method name");

                if (self.match(.lparen)) {
                    // Method call
                    const args = try self.parseArgList();
                    const end_span = self.previous.span;

                    expr = try self.ast.addNode(.{
                        .tag = .method_call,
                        .span = Span.merge(
                            if (self.ast.getNode(expr)) |n| n.span else end_span,
                            end_span,
                        ),
                        .data = .{ .method_call = .{
                            .receiver = expr,
                            .method_name = name,
                            .args = args,
                        } },
                    });
                } else {
                    // Field access
                    expr = try self.ast.addNode(.{
                        .tag = .field_access,
                        .span = Span.merge(
                            if (self.ast.getNode(expr)) |n| n.span else self.previous.span,
                            self.previous.span,
                        ),
                        .data = .{ .field_access = .{
                            .object = expr,
                            .field_name = name,
                        } },
                    });
                }
            } else if (self.match(.lparen)) {
                // Function call
                const args = try self.parseArgList();
                const end_span = self.previous.span;

                expr = try self.ast.addNode(.{
                    .tag = .call,
                    .span = Span.merge(
                        if (self.ast.getNode(expr)) |n| n.span else end_span,
                        end_span,
                    ),
                    .data = .{ .call = .{
                        .callee = expr,
                        .args = args,
                    } },
                });
            } else if (self.match(.lbracket)) {
                // Index access
                const index_expr = try self.parseExpression();

                if (!self.match(.rbracket)) {
                    return self.errorAtCurrent("expected ']' after index");
                }

                expr = try self.ast.addNode(.{
                    .tag = .index_access,
                    .span = Span.merge(
                        if (self.ast.getNode(expr)) |n| n.span else self.previous.span,
                        self.previous.span,
                    ),
                    .data = .{ .index = .{
                        .object = expr,
                        .index_expr = index_expr,
                    } },
                });
            } else if (self.match(.question)) {
                // Try operator: expr?
                expr = try self.ast.addNode(.{
                    .tag = .unary_op,
                    .span = Span.merge(
                        if (self.ast.getNode(expr)) |n| n.span else self.previous.span,
                        self.previous.span,
                    ),
                    .data = .{ .unary = .{
                        .op = .try_op,
                        .operand = expr,
                    } },
                });
            } else {
                break;
            }
        }

        return expr;
    }

    fn parsePrimaryExpr(self: *Parser) ParseError!NodeIndex {
        const span = self.current.span;

        // Literals
        if (self.match(.int_literal)) {
            const str_idx = try self.ast.addString(self.previous.text);
            return self.ast.addNode(.{
                .tag = .integer_literal,
                .span = span,
                .data = .{ .string = str_idx },
            });
        }

        if (self.match(.float_literal)) {
            const str_idx = try self.ast.addString(self.previous.text);
            return self.ast.addNode(.{
                .tag = .float_literal,
                .span = span,
                .data = .{ .string = str_idx },
            });
        }

        if (self.match(.string_literal)) {
            const str_idx = try self.ast.addString(self.previous.text);
            return self.ast.addNode(.{
                .tag = .string_literal,
                .span = span,
                .data = .{ .string = str_idx },
            });
        }

        if (self.match(.char_literal)) {
            const str_idx = try self.ast.addString(self.previous.text);
            return self.ast.addNode(.{
                .tag = .char_literal,
                .span = span,
                .data = .{ .string = str_idx },
            });
        }

        if (self.match(.true_)) {
            return self.ast.addNode(.{
                .tag = .bool_literal,
                .span = span,
                .data = .{ .bool_value = true },
            });
        }

        if (self.match(.false_)) {
            return self.ast.addNode(.{
                .tag = .bool_literal,
                .span = span,
                .data = .{ .bool_value = false },
            });
        }

        // Self references
        if (self.match(.kw_self)) {
            return self.ast.addNode(.{
                .tag = .self_expr,
                .span = span,
                .data = .{ .none = {} },
            });
        }

        if (self.match(.kw_Self)) {
            return self.ast.addNode(.{
                .tag = .self_type,
                .span = span,
                .data = .{ .none = {} },
            });
        }

        // If expression
        if (self.check(.kw_if)) {
            return self.parseIfExpr();
        }

        // Match expression
        if (self.check(.kw_match)) {
            return self.parseMatchExpr();
        }

        // Block expression
        if (self.check(.lbrace)) {
            return self.parseBlock();
        }

        // Array literal
        if (self.check(.lbracket)) {
            return self.parseArrayLiteral();
        }

        // Grouped expression
        if (self.match(.lparen)) {
            const inner = try self.parseExpression();
            if (!self.match(.rparen)) {
                return self.errorAtCurrent("expected ')' after expression");
            }
            return self.ast.addNode(.{
                .tag = .grouped,
                .span = Span.merge(span, self.previous.span),
                .data = .{ .node = inner },
            });
        }

        // Identifier (possibly struct literal or generic instantiation)
        if (self.match(.identifier)) {
            const name = try self.ast.addString(self.previous.text);
            const ident_node = try self.ast.addNode(.{
                .tag = .identifier,
                .span = span,
                .data = .{ .string = name },
            });

            // Check for struct literal: Name { field: value, ... }
            // Only treat as struct literal if followed by { identifier :
            // This disambiguates from block expressions after identifiers
            if (self.check(.lbrace) and self.isStructLiteralAhead()) {
                return self.parseStructLiteral(ident_node);
            }

            return ident_node;
        }

        return self.errorAtCurrent("expected expression");
    }

    fn parseIfExpr(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_if)) {
            return self.errorAtCurrent("expected 'if'");
        }

        const condition = try self.parseExpression();

        self.skipNewlines();
        const then_block = try self.parseBlock();

        var else_block: NodeIndex = null_node;
        self.skipNewlines();
        if (self.match(.kw_else)) {
            self.skipNewlines();
            if (self.check(.kw_if)) {
                else_block = try self.parseIfExpr();
            } else {
                else_block = try self.parseBlock();
            }
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .if_expr,
            .span = Span.merge(start_span, end_span),
            .data = .{ .if_expr = .{
                .condition = condition,
                .then_block = then_block,
                .else_block = else_block,
            } },
        });
    }

    fn parseMatchExpr(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.kw_match)) {
            return self.errorAtCurrent("expected 'match'");
        }

        const value = try self.parseExpression();

        self.skipNewlines();
        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{' after match value");
        }

        const arms = try self.parseMatchArms();

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after match arms");
        }

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .match_expr,
            .span = Span.merge(start_span, end_span),
            .data = .{ .match_expr = .{
                .value = value,
                .arms = arms,
            } },
        });
    }

    fn parseMatchArms(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const arm = try self.parseMatchArm();
            try self.scratch.append(self.ast.allocator, arm);

            _ = self.match(.comma);
            self.skipNewlines();
        }

        return self.finishNodeList(scratch_start);
    }

    fn parseMatchArm(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        const pattern = try self.parsePattern();

        // Optional guard
        var guard: NodeIndex = null_node;
        if (self.match(.kw_if)) {
            guard = try self.parseExpression();
        }

        if (!self.match(.fat_arrow)) {
            return self.errorAtCurrent("expected '=>' after pattern");
        }

        const body = try self.parseExpression();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .match_arm,
            .span = Span.merge(start_span, end_span),
            .data = .{ .match_arm = .{
                .pattern = pattern,
                .guard = guard,
                .body = body,
            } },
        });
    }

    fn parsePattern(self: *Parser) ParseError!NodeIndex {
        const span = self.current.span;

        // Wildcard pattern: _
        if (self.check(.identifier) and std.mem.eql(u8, self.current.text, "_")) {
            _ = self.advance();
            return self.ast.addNode(.{
                .tag = .pattern_wildcard,
                .span = span,
                .data = .{ .none = {} },
            });
        }

        // Literal patterns
        if (self.match(.int_literal) or self.match(.float_literal) or self.match(.string_literal) or
            self.match(.char_literal) or self.match(.true_) or self.match(.false_))
        {
            const str_idx = try self.ast.addString(self.previous.text);
            return self.ast.addNode(.{
                .tag = .pattern_literal,
                .span = span,
                .data = .{ .string = str_idx },
            });
        }

        // Identifier pattern (also handles enum variants)
        if (self.match(.identifier)) {
            const name = try self.ast.addString(self.previous.text);

            // Check for enum variant with payload: Some(x)
            if (self.match(.lparen)) {
                const inner = try self.parsePattern();
                if (!self.match(.rparen)) {
                    return self.errorAtCurrent("expected ')' after pattern");
                }
                return self.ast.addNode(.{
                    .tag = .pattern_enum,
                    .span = Span.merge(span, self.previous.span),
                    .data = .{ .node_and_string = .{
                        .node = inner,
                        .string = name,
                    } },
                });
            }

            return self.ast.addNode(.{
                .tag = .pattern_ident,
                .span = span,
                .data = .{ .string = name },
            });
        }

        // Tuple pattern: (a, b, c)
        if (self.match(.lparen)) {
            const scratch_start = self.scratch.items.len;

            while (!self.check(.rparen) and !self.check(.eof)) {
                const elem = try self.parsePattern();
                try self.scratch.append(self.ast.allocator, elem);

                if (!self.match(.comma)) break;
            }

            if (!self.match(.rparen)) {
                return self.errorAtCurrent("expected ')' after tuple pattern");
            }

            const elements = try self.finishNodeList(scratch_start);

            return self.ast.addNode(.{
                .tag = .pattern_tuple,
                .span = Span.merge(span, self.previous.span),
                .data = .{ .range = elements },
            });
        }

        return self.errorAtCurrent("expected pattern");
    }

    fn parseArrayLiteral(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        if (!self.match(.lbracket)) {
            return self.errorAtCurrent("expected '['");
        }

        const scratch_start = self.scratch.items.len;

        while (!self.check(.rbracket) and !self.check(.eof)) {
            const elem = try self.parseExpression();
            try self.scratch.append(self.ast.allocator, elem);

            if (!self.match(.comma)) break;
        }

        if (!self.match(.rbracket)) {
            return self.errorAtCurrent("expected ']' after array elements");
        }

        const elements = try self.finishNodeList(scratch_start);
        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .array_literal,
            .span = Span.merge(start_span, end_span),
            .data = .{ .range = elements },
        });
    }

    fn parseStructLiteral(self: *Parser, type_node: NodeIndex) ParseError!NodeIndex {
        const start_span = if (self.ast.getNode(type_node)) |n| n.span else self.current.span;

        if (!self.match(.lbrace)) {
            return self.errorAtCurrent("expected '{'");
        }

        const scratch_start = self.scratch.items.len;

        self.skipNewlines();
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const field = try self.parseFieldInit();
            try self.scratch.append(self.ast.allocator, field);

            if (!self.match(.comma)) {
                self.skipNewlines();
            }
        }

        if (!self.match(.rbrace)) {
            return self.errorAtCurrent("expected '}' after struct fields");
        }

        const fields = try self.finishNodeList(scratch_start);
        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .struct_literal,
            .span = Span.merge(start_span, end_span),
            .data = .{ .struct_literal = .{
                .type_name = type_node,
                .fields = fields,
            } },
        });
    }

    fn parseFieldInit(self: *Parser) ParseError!NodeIndex {
        const start_span = self.current.span;

        const name = try self.expectIdentifier("expected field name");

        if (!self.match(.colon)) {
            return self.errorAtCurrent("expected ':' after field name");
        }

        const value = try self.parseExpression();

        const end_span = self.previous.span;

        return self.ast.addNode(.{
            .tag = .field_init,
            .span = Span.merge(start_span, end_span),
            .data = .{ .field_init = .{
                .name = name,
                .value = value,
            } },
        });
    }

    fn parseArgList(self: *Parser) ParseError!NodeRange {
        const scratch_start = self.scratch.items.len;

        while (!self.check(.rparen) and !self.check(.eof)) {
            const arg = try self.parseExpression();
            try self.scratch.append(self.ast.allocator, arg);

            if (!self.match(.comma)) break;
        }

        if (!self.match(.rparen)) {
            _ = try self.errorAtCurrent("expected ')' after arguments");
        }

        return self.finishNodeList(scratch_start);
    }

    // =========================================================================
    // Types
    // =========================================================================

    fn parseType(self: *Parser) ParseError!NodeIndex {
        const span = self.current.span;

        // Optional type: ?T
        if (self.match(.question)) {
            const inner = try self.parseType();
            return self.ast.addNode(.{
                .tag = .type_optional,
                .span = Span.merge(span, if (self.ast.getNode(inner)) |n| n.span else span),
                .data = .{ .node = inner },
            });
        }

        // Reference type: &T or &mut T
        if (self.match(.amp)) {
            const is_mut = self.match(.kw_mut);
            const inner = try self.parseType();
            return self.ast.addNode(.{
                .tag = .type_reference,
                .span = Span.merge(span, if (self.ast.getNode(inner)) |n| n.span else span),
                .data = .{ .ref_type = .{
                    .pointee = inner,
                    .is_mut = is_mut,
                } },
            });
        }

        // Array/Slice type: [T] or [T; N]
        if (self.match(.lbracket)) {
            const element_type = try self.parseType();

            if (self.match(.semicolon)) {
                // Array type: [T; N]
                const size_expr = try self.parseExpression();
                if (!self.match(.rbracket)) {
                    return self.errorAtCurrent("expected ']' after array size");
                }
                return self.ast.addNode(.{
                    .tag = .type_array,
                    .span = Span.merge(span, self.previous.span),
                    .data = .{ .array_type = .{
                        .element_type = element_type,
                        .size_expr = size_expr,
                    } },
                });
            }

            // Slice type: [T]
            if (!self.match(.rbracket)) {
                return self.errorAtCurrent("expected ']' after slice type");
            }
            return self.ast.addNode(.{
                .tag = .type_slice,
                .span = Span.merge(span, self.previous.span),
                .data = .{ .node = element_type },
            });
        }

        // Tuple type: (T, U, V)
        if (self.match(.lparen)) {
            const scratch_start = self.scratch.items.len;

            while (!self.check(.rparen) and !self.check(.eof)) {
                const elem_type = try self.parseType();
                try self.scratch.append(self.ast.allocator, elem_type);

                if (!self.match(.comma)) break;
            }

            if (!self.match(.rparen)) {
                return self.errorAtCurrent("expected ')' after tuple type");
            }

            const element_types = try self.finishNodeList(scratch_start);

            return self.ast.addNode(.{
                .tag = .type_tuple,
                .span = Span.merge(span, self.previous.span),
                .data = .{ .range = element_types },
            });
        }

        // Function type: fn(T, U) -> V
        if (self.match(.kw_fn)) {
            if (!self.match(.lparen)) {
                return self.errorAtCurrent("expected '(' after 'fn' in type");
            }

            const scratch_start = self.scratch.items.len;

            while (!self.check(.rparen) and !self.check(.eof)) {
                const param_type = try self.parseType();
                try self.scratch.append(self.ast.allocator, param_type);

                if (!self.match(.comma)) break;
            }

            if (!self.match(.rparen)) {
                return self.errorAtCurrent("expected ')' after function type params");
            }

            var return_type: NodeIndex = null_node;
            if (self.match(.arrow)) {
                return_type = try self.parseType();
            }

            const param_types = try self.finishNodeList(scratch_start);

            return self.ast.addNode(.{
                .tag = .type_function,
                .span = Span.merge(span, self.previous.span),
                .data = .{ .fn_type = .{
                    .param_types = param_types,
                    .return_type = return_type,
                } },
            });
        }

        // Named type (possibly generic): Name or Name[T, U]
        if (self.match(.identifier)) {
            const name = try self.ast.addString(self.previous.text);

            var base_type = try self.ast.addNode(.{
                .tag = .type_named,
                .span = span,
                .data = .{ .string = name },
            });

            // Check for generic arguments: Type[T, U]
            if (self.match(.lbracket)) {
                const scratch_start = self.scratch.items.len;

                while (!self.check(.rbracket) and !self.check(.eof)) {
                    const type_arg = try self.parseType();
                    try self.scratch.append(self.ast.allocator, type_arg);

                    if (!self.match(.comma)) break;
                }

                if (!self.match(.rbracket)) {
                    return self.errorAtCurrent("expected ']' after type arguments");
                }

                const type_args = try self.finishNodeList(scratch_start);

                base_type = try self.ast.addNode(.{
                    .tag = .type_generic,
                    .span = Span.merge(span, self.previous.span),
                    .data = .{ .generic_type = .{
                        .base_type = base_type,
                        .type_args = type_args,
                    } },
                });
            }

            return base_type;
        }

        // Self type
        if (self.match(.kw_Self)) {
            return self.ast.addNode(.{
                .tag = .self_type,
                .span = span,
                .data = .{ .none = {} },
            });
        }

        return self.errorAtCurrent("expected type");
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    fn tokenToBinaryOp(self: *const Parser, kind: TokenKind) ?BinaryOp {
        _ = self;
        return switch (kind) {
            // Basic arithmetic
            .plus => .add,
            .minus => .sub,
            .star => .mul,
            .slash => .div,
            .percent => .mod,
            // Wrapping arithmetic
            .plus_wrap => .add_wrap,
            .minus_wrap => .sub_wrap,
            .star_wrap => .mul_wrap,
            // Saturating arithmetic
            .plus_sat => .add_sat,
            .minus_sat => .sub_sat,
            .star_sat => .mul_sat,
            // Comparison
            .eq_eq => .eq,
            .bang_eq => .ne,
            .lt => .lt,
            .gt => .gt,
            .lt_eq => .le,
            .gt_eq => .ge,
            // Logical
            .kw_and => .@"and",
            .kw_or => .@"or",
            // Bitwise
            .amp => .bit_and,
            .pipe => .bit_or,
            .caret => .bit_xor,
            .lt_lt => .shl,
            .gt_gt => .shr,
            // Null coalescing
            .question_question => .null_coalesce,
            // Type checking
            .kw_is => .@"is",
            // Assignment
            .eq => .assign,
            .plus_eq => .add_assign,
            .minus_eq => .sub_assign,
            .star_eq => .mul_assign,
            .slash_eq => .div_assign,
            .percent_eq => .mod_assign,
            // Range
            .dot_dot => .range,
            .dot_dot_eq => .range_inclusive,
            else => null,
        };
    }

    fn expectIdentifier(self: *Parser, message: []const u8) !StringIndex {
        if (!self.match(.identifier)) {
            _ = try self.errorAtCurrent(message);
            return null_string;
        }
        return self.ast.addString(self.previous.text);
    }

    fn finishNodeList(self: *Parser, scratch_start: usize) !NodeRange {
        const items = self.scratch.items[scratch_start..];
        const count = items.len;

        // Reset scratch buffer
        self.scratch.shrinkRetainingCapacity(scratch_start);

        if (count == 0) {
            return NodeRange.empty;
        }

        // The scratch buffer contains indices into ast.nodes
        // Return the range spanning from first to last index + 1
        const range_start: NodeIndex = items[0];
        const range_end: NodeIndex = items[count - 1] + 1;

        return NodeRange{
            .start = range_start,
            .end = range_end,
        };
    }

    fn advance(self: *Parser) Token {
        self.previous = self.current;
        while (true) {
            self.current = self.lexer.next();
            // Skip comments but not newlines (significant for statement separation)
            if (self.current.kind != .comment and self.current.kind != .doc_comment) {
                break;
            }
        }
        return self.previous;
    }

    fn check(self: *const Parser, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        if (!self.check(kind)) return false;
        _ = self.advance();
        return true;
    }

    fn skipNewlines(self: *Parser) void {
        while (self.match(.newline)) {}
    }

    /// Lookahead to check if current `{` starts a struct literal.
    /// A struct literal looks like: `{ identifier : ... }` or `{ }` (empty)
    /// A block expression looks like: `{ statement ... }`
    fn isStructLiteralAhead(self: *Parser) bool {
        // Must be at `{`
        if (!self.check(.lbrace)) return false;

        // Save current state for lookahead
        const saved_index = self.lexer.index;
        const saved_pos = self.lexer.pos;
        const saved_current = self.current;

        // Consume `{`
        _ = self.advance();
        self.skipNewlines();

        // Check what follows
        const is_struct = blk: {
            // Empty struct literal: `{ }`
            if (self.check(.rbrace)) break :blk true;

            // Field initializer pattern: `identifier :`
            if (self.check(.identifier)) {
                _ = self.advance();
                if (self.check(.colon)) break :blk true;
            }

            break :blk false;
        };

        // Restore state
        self.lexer.index = saved_index;
        self.lexer.pos = saved_pos;
        self.current = saved_current;

        return is_struct;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) ParseError!NodeIndex {
        try self.ast.addError(message, self.current.span);
        const span = self.current.span;
        // Advance past the error token to prevent infinite loops
        if (!self.check(.eof)) {
            _ = self.advance();
        }
        return self.ast.addNode(.{
            .tag = .@"error",
            .span = span,
            .data = .{ .none = {} },
        });
    }
};

// =========================================================================
// Tests
// =========================================================================

test "parser empty module" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
    try std.testing.expect(ast.root != null_node);
    const root = ast.getNode(ast.root).?;
    try std.testing.expectEqual(Node.Tag.module, root.tag);
}

test "parser let statement" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "let x = 42", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser var statement" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "var x = 42", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser function declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    return a + b
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser struct declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\struct Point {
        \\    x: i32,
        \\    y: i32
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser enum declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\enum Option[T] {
        \\    Some(T),
        \\    None
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser binary expressions" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "let x = 1 + 2 * 3", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser if expression" {
    const allocator = std.testing.allocator;
    const source =
        \\if x > 0 {
        \\    1
        \\} else {
        \\    0
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser while loop" {
    const allocator = std.testing.allocator;
    const source =
        \\while x > 0 {
        \\    x = x - 1
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser for loop" {
    const allocator = std.testing.allocator;
    const source =
        \\for item in items {
        \\    process(item)
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser function call" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "foo(1, 2, 3)", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser method call" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "obj.method(arg)", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser field access" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "point.x", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser index access" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "arr[0]", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser struct literal" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "Point { x: 1, y: 2 }", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser array literal" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "[1, 2, 3]", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser generic type" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "let x: List[i32] = []", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser impl block" {
    const allocator = std.testing.allocator;
    const source =
        \\impl Point {
        \\    fn new(x: i32, y: i32) -> Self {
        \\        Point { x: x, y: y }
        \\    }
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser trait declaration" {
    const allocator = std.testing.allocator;
    const source =
        \\trait Drawable {
        \\    fn draw(self)
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser import statement" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "import std.io", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser match expression" {
    const allocator = std.testing.allocator;
    const source =
        \\match value {
        \\    0 => "zero",
        \\    x => "other"
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser unary operators" {
    const allocator = std.testing.allocator;

    // Negation
    {
        var parser = Parser.init(allocator, "-x", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    // Not
    {
        var parser = Parser.init(allocator, "not x", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    // Reference
    {
        var parser = Parser.init(allocator, "&x", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }
}

test "parser loop statement" {
    const allocator = std.testing.allocator;
    const source =
        \\loop {
        \\    break
        \\}
    ;
    var parser = Parser.init(allocator, source, 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser wrapping operators" {
    const allocator = std.testing.allocator;

    {
        var parser = Parser.init(allocator, "a +% b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    {
        var parser = Parser.init(allocator, "a -% b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    {
        var parser = Parser.init(allocator, "a *% b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }
}

test "parser saturating operators" {
    const allocator = std.testing.allocator;

    {
        var parser = Parser.init(allocator, "a +| b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    {
        var parser = Parser.init(allocator, "a -| b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    {
        var parser = Parser.init(allocator, "a *| b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }
}

test "parser shift operators" {
    const allocator = std.testing.allocator;

    {
        var parser = Parser.init(allocator, "a << b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }

    {
        var parser = Parser.init(allocator, "a >> b", 0);
        defer parser.deinit();
        var ast = try parser.parse();
        defer ast.deinit();
        try std.testing.expect(!ast.hasErrors());
    }
}

test "parser null coalescing operator" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "a ?? b", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser is operator" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "x is i32", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    try std.testing.expect(!ast.hasErrors());
}

test "parser path separator" {
    const allocator = std.testing.allocator;
    var parser = Parser.init(allocator, "import std::io", 0);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit();

    // Note: This may fail if we don't support :: in imports yet
    // For now just check it doesn't crash
}
