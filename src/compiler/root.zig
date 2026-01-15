//! Klar compiler frontend.
//!
//! Provides lexer, parser, and type checker for the Klar language.

pub const token = @import("token.zig");
pub const lexer = @import("lexer.zig");
pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");
pub const types = @import("types.zig");
pub const checker = @import("checker.zig");

pub const Token = token.Token;
pub const TokenKind = token.TokenKind;
pub const Lexer = lexer.Lexer;

pub const Ast = ast.Ast;
pub const Node = ast.Node;
pub const NodeIndex = ast.NodeIndex;
pub const BinaryOp = ast.BinaryOp;
pub const UnaryOp = ast.UnaryOp;

pub const Parser = parser.Parser;

pub const Type = types.Type;
pub const TypeId = types.TypeId;
pub const TypePool = types.TypePool;
pub const Primitive = types.Primitive;
pub const BuiltinTypes = types.BuiltinTypes;

pub const Checker = checker.Checker;
pub const Symbol = checker.Symbol;
pub const Scope = checker.Scope;

test {
    _ = token;
    _ = lexer;
    _ = ast;
    _ = parser;
    _ = types;
    _ = checker;
}
