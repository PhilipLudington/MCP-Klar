//! Token types for the Klar lexer.

const std = @import("std");
const utils = @import("utils");

const Span = utils.Span;

/// Token kinds for the Klar language.
pub const TokenKind = enum {
    // Literals
    integer,
    float,
    string,
    char,
    true_,
    false_,

    // Identifiers and keywords
    identifier,
    kw_fn,
    kw_let,
    kw_mut,
    kw_const,
    kw_if,
    kw_else,
    kw_while,
    kw_for,
    kw_in,
    kw_return,
    kw_break,
    kw_continue,
    kw_struct,
    kw_enum,
    kw_trait,
    kw_impl,
    kw_type,
    kw_pub,
    kw_import,
    kw_as,
    kw_self,
    kw_Self,
    kw_match,
    kw_and,
    kw_or,
    kw_not,

    // Punctuation
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    comma,
    dot,
    colon,
    semicolon,
    arrow,
    fat_arrow,
    question,
    at,
    hash,

    // Operators
    plus,
    minus,
    star,
    slash,
    percent,
    ampersand,
    pipe,
    caret,
    tilde,
    bang,
    eq,
    lt,
    gt,
    plus_eq,
    minus_eq,
    star_eq,
    slash_eq,
    eq_eq,
    bang_eq,
    lt_eq,
    gt_eq,
    ampersand_ampersand,
    pipe_pipe,
    dot_dot,
    dot_dot_eq,

    // Special
    newline,
    comment,
    doc_comment,
    eof,
    invalid,

    pub fn isKeyword(self: TokenKind) bool {
        return @intFromEnum(self) >= @intFromEnum(TokenKind.kw_fn) and
            @intFromEnum(self) <= @intFromEnum(TokenKind.kw_not);
    }

    pub fn isOperator(self: TokenKind) bool {
        return @intFromEnum(self) >= @intFromEnum(TokenKind.plus) and
            @intFromEnum(self) <= @intFromEnum(TokenKind.dot_dot_eq);
    }
};

/// A lexical token with its span and optional value.
pub const Token = struct {
    kind: TokenKind,
    span: Span,
    /// The text of the token (for identifiers, literals, etc.)
    text: []const u8,

    pub fn init(kind: TokenKind, span: Span, text: []const u8) Token {
        return .{
            .kind = kind,
            .span = span,
            .text = text,
        };
    }

    pub fn eof(span: Span) Token {
        return .{
            .kind = .eof,
            .span = span,
            .text = "",
        };
    }
};

/// Keyword lookup table.
pub const keywords = std.StaticStringMap(TokenKind).initComptime(.{
    .{ "fn", .kw_fn },
    .{ "let", .kw_let },
    .{ "mut", .kw_mut },
    .{ "const", .kw_const },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "while", .kw_while },
    .{ "for", .kw_for },
    .{ "in", .kw_in },
    .{ "return", .kw_return },
    .{ "break", .kw_break },
    .{ "continue", .kw_continue },
    .{ "struct", .kw_struct },
    .{ "enum", .kw_enum },
    .{ "trait", .kw_trait },
    .{ "impl", .kw_impl },
    .{ "type", .kw_type },
    .{ "pub", .kw_pub },
    .{ "import", .kw_import },
    .{ "as", .kw_as },
    .{ "self", .kw_self },
    .{ "Self", .kw_Self },
    .{ "match", .kw_match },
    .{ "and", .kw_and },
    .{ "or", .kw_or },
    .{ "not", .kw_not },
    .{ "true", .true_ },
    .{ "false", .false_ },
});

test "keywords lookup" {
    try std.testing.expectEqual(TokenKind.kw_fn, keywords.get("fn").?);
    try std.testing.expectEqual(TokenKind.kw_let, keywords.get("let").?);
    try std.testing.expectEqual(TokenKind.true_, keywords.get("true").?);
    try std.testing.expect(keywords.get("notakeyword") == null);
}

test "TokenKind.isKeyword" {
    try std.testing.expect(TokenKind.kw_fn.isKeyword());
    try std.testing.expect(TokenKind.kw_return.isKeyword());
    try std.testing.expect(!TokenKind.plus.isKeyword());
    try std.testing.expect(!TokenKind.identifier.isKeyword());
}
