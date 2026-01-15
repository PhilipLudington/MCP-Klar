//! Klar compiler frontend.
//!
//! Provides lexer, parser, and type checker for the Klar language.

pub const token = @import("token.zig");
pub const lexer = @import("lexer.zig");

pub const Token = token.Token;
pub const TokenKind = token.TokenKind;
pub const Lexer = lexer.Lexer;

test {
    _ = token;
    _ = lexer;
}
