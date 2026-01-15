//! Klar compiler frontend.
//!
//! Provides lexer, parser, and type checker for the Klar language.

pub const token = @import("token.zig");
pub const lexer = @import("lexer.zig");
pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");

pub const Token = token.Token;
pub const TokenKind = token.TokenKind;
pub const Lexer = lexer.Lexer;

pub const Ast = ast.Ast;
pub const Node = ast.Node;
pub const NodeIndex = ast.NodeIndex;
pub const BinaryOp = ast.BinaryOp;
pub const UnaryOp = ast.UnaryOp;

pub const Parser = parser.Parser;

test {
    _ = token;
    _ = lexer;
    _ = ast;
    _ = parser;
}
