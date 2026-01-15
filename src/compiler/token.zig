//! Token types for the Klar lexer.
//!
//! Matches the Klar language specification from the reference implementation.

const std = @import("std");
const utils = @import("utils");

const Span = utils.Span;

/// Token kinds for the Klar language.
pub const TokenKind = enum {
    // Literals
    int_literal,
    float_literal,
    string_literal,
    char_literal,
    true_,
    false_,

    // Identifier
    identifier,

    // Keywords
    kw_fn,
    kw_let,
    kw_var,
    kw_struct,
    kw_enum,
    kw_trait,
    kw_impl,
    kw_if,
    kw_else,
    kw_match,
    kw_for,
    kw_while,
    kw_loop,
    kw_return,
    kw_break,
    kw_continue,
    kw_pub,
    kw_mut,
    kw_async,
    kw_await,
    kw_unsafe,
    kw_import,
    kw_module,
    kw_as,
    kw_in,
    kw_is,
    kw_and,
    kw_or,
    kw_not,
    kw_comptime,
    kw_where,
    kw_dyn,
    kw_type,
    kw_const,
    kw_static,
    kw_self,
    kw_Self,

    // Operators
    plus, // +
    minus, // -
    star, // *
    slash, // /
    percent, // %
    plus_wrap, // +%
    minus_wrap, // -%
    star_wrap, // *%
    plus_sat, // +|
    minus_sat, // -|
    star_sat, // *|
    eq, // =
    eq_eq, // ==
    bang_eq, // !=
    lt, // <
    gt, // >
    lt_eq, // <=
    gt_eq, // >=
    amp, // &
    pipe, // |
    caret, // ^
    tilde, // ~
    lt_lt, // <<
    gt_gt, // >>
    question, // ?
    bang, // !
    question_question, // ??
    dot, // .
    dot_dot, // ..
    dot_dot_eq, // ..=
    arrow, // ->
    fat_arrow, // =>
    colon, // :
    colon_colon, // ::

    // Compound assignment
    plus_eq, // +=
    minus_eq, // -=
    star_eq, // *=
    slash_eq, // /=
    percent_eq, // %=

    // Delimiters
    lparen, // (
    rparen, // )
    lbracket, // [
    rbracket, // ]
    lbrace, // {
    rbrace, // }
    comma, // ,
    semicolon, // ;
    at, // @
    hash, // #

    // Special
    newline,
    comment,
    doc_comment,
    eof,
    invalid,

    pub fn lexeme(self: TokenKind) ?[]const u8 {
        return switch (self) {
            .kw_fn => "fn",
            .kw_let => "let",
            .kw_var => "var",
            .kw_struct => "struct",
            .kw_enum => "enum",
            .kw_trait => "trait",
            .kw_impl => "impl",
            .kw_if => "if",
            .kw_else => "else",
            .kw_match => "match",
            .kw_for => "for",
            .kw_while => "while",
            .kw_loop => "loop",
            .kw_return => "return",
            .kw_break => "break",
            .kw_continue => "continue",
            .kw_pub => "pub",
            .kw_mut => "mut",
            .kw_async => "async",
            .kw_await => "await",
            .kw_unsafe => "unsafe",
            .kw_import => "import",
            .kw_module => "module",
            .kw_as => "as",
            .kw_in => "in",
            .kw_is => "is",
            .kw_and => "and",
            .kw_or => "or",
            .kw_not => "not",
            .true_ => "true",
            .false_ => "false",
            .kw_comptime => "comptime",
            .kw_where => "where",
            .kw_dyn => "dyn",
            .kw_type => "type",
            .kw_const => "const",
            .kw_static => "static",
            .kw_self => "self",
            .kw_Self => "Self",
            .plus => "+",
            .minus => "-",
            .star => "*",
            .slash => "/",
            .percent => "%",
            .plus_wrap => "+%",
            .minus_wrap => "-%",
            .star_wrap => "*%",
            .plus_sat => "+|",
            .minus_sat => "-|",
            .star_sat => "*|",
            .eq => "=",
            .eq_eq => "==",
            .bang_eq => "!=",
            .lt => "<",
            .gt => ">",
            .lt_eq => "<=",
            .gt_eq => ">=",
            .amp => "&",
            .pipe => "|",
            .caret => "^",
            .tilde => "~",
            .lt_lt => "<<",
            .gt_gt => ">>",
            .question => "?",
            .bang => "!",
            .question_question => "??",
            .dot => ".",
            .dot_dot => "..",
            .dot_dot_eq => "..=",
            .arrow => "->",
            .fat_arrow => "=>",
            .colon => ":",
            .colon_colon => "::",
            .plus_eq => "+=",
            .minus_eq => "-=",
            .star_eq => "*=",
            .slash_eq => "/=",
            .percent_eq => "%=",
            .lparen => "(",
            .rparen => ")",
            .lbracket => "[",
            .rbracket => "]",
            .lbrace => "{",
            .rbrace => "}",
            .comma => ",",
            .semicolon => ";",
            .at => "@",
            .hash => "#",
            .newline => "\\n",
            .eof => "EOF",
            .comment, .doc_comment, .invalid => null,
            .int_literal, .float_literal, .string_literal, .char_literal, .identifier => null,
        };
    }

    pub fn isKeyword(self: TokenKind) bool {
        return switch (self) {
            .kw_fn,
            .kw_let,
            .kw_var,
            .kw_struct,
            .kw_enum,
            .kw_trait,
            .kw_impl,
            .kw_if,
            .kw_else,
            .kw_match,
            .kw_for,
            .kw_while,
            .kw_loop,
            .kw_return,
            .kw_break,
            .kw_continue,
            .kw_pub,
            .kw_mut,
            .kw_async,
            .kw_await,
            .kw_unsafe,
            .kw_import,
            .kw_module,
            .kw_as,
            .kw_in,
            .kw_is,
            .kw_and,
            .kw_or,
            .kw_not,
            .true_,
            .false_,
            .kw_comptime,
            .kw_where,
            .kw_dyn,
            .kw_type,
            .kw_const,
            .kw_static,
            .kw_self,
            .kw_Self,
            => true,
            else => false,
        };
    }

    pub fn isOperator(self: TokenKind) bool {
        return switch (self) {
            .plus,
            .minus,
            .star,
            .slash,
            .percent,
            .plus_wrap,
            .minus_wrap,
            .star_wrap,
            .plus_sat,
            .minus_sat,
            .star_sat,
            .eq,
            .eq_eq,
            .bang_eq,
            .lt,
            .gt,
            .lt_eq,
            .gt_eq,
            .amp,
            .pipe,
            .caret,
            .tilde,
            .lt_lt,
            .gt_gt,
            .question,
            .bang,
            .question_question,
            .dot,
            .dot_dot,
            .dot_dot_eq,
            .arrow,
            .fat_arrow,
            .colon,
            .colon_colon,
            .plus_eq,
            .minus_eq,
            .star_eq,
            .slash_eq,
            .percent_eq,
            => true,
            else => false,
        };
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
    .{ "var", .kw_var },
    .{ "struct", .kw_struct },
    .{ "enum", .kw_enum },
    .{ "trait", .kw_trait },
    .{ "impl", .kw_impl },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "match", .kw_match },
    .{ "for", .kw_for },
    .{ "while", .kw_while },
    .{ "loop", .kw_loop },
    .{ "return", .kw_return },
    .{ "break", .kw_break },
    .{ "continue", .kw_continue },
    .{ "pub", .kw_pub },
    .{ "mut", .kw_mut },
    .{ "async", .kw_async },
    .{ "await", .kw_await },
    .{ "unsafe", .kw_unsafe },
    .{ "import", .kw_import },
    .{ "module", .kw_module },
    .{ "as", .kw_as },
    .{ "in", .kw_in },
    .{ "is", .kw_is },
    .{ "and", .kw_and },
    .{ "or", .kw_or },
    .{ "not", .kw_not },
    .{ "comptime", .kw_comptime },
    .{ "where", .kw_where },
    .{ "dyn", .kw_dyn },
    .{ "type", .kw_type },
    .{ "const", .kw_const },
    .{ "static", .kw_static },
    .{ "self", .kw_self },
    .{ "Self", .kw_Self },
    .{ "true", .true_ },
    .{ "false", .false_ },
});

test "keywords lookup" {
    try std.testing.expectEqual(TokenKind.kw_fn, keywords.get("fn").?);
    try std.testing.expectEqual(TokenKind.kw_let, keywords.get("let").?);
    try std.testing.expectEqual(TokenKind.kw_var, keywords.get("var").?);
    try std.testing.expectEqual(TokenKind.kw_loop, keywords.get("loop").?);
    try std.testing.expectEqual(TokenKind.true_, keywords.get("true").?);
    try std.testing.expect(keywords.get("notakeyword") == null);
}

test "TokenKind.isKeyword" {
    try std.testing.expect(TokenKind.kw_fn.isKeyword());
    try std.testing.expect(TokenKind.kw_return.isKeyword());
    try std.testing.expect(TokenKind.kw_var.isKeyword());
    try std.testing.expect(TokenKind.kw_loop.isKeyword());
    try std.testing.expect(!TokenKind.plus.isKeyword());
    try std.testing.expect(!TokenKind.identifier.isKeyword());
}

test "TokenKind.isOperator" {
    try std.testing.expect(TokenKind.plus.isOperator());
    try std.testing.expect(TokenKind.plus_wrap.isOperator());
    try std.testing.expect(TokenKind.lt_lt.isOperator());
    try std.testing.expect(TokenKind.question_question.isOperator());
    try std.testing.expect(TokenKind.colon_colon.isOperator());
    try std.testing.expect(!TokenKind.kw_fn.isOperator());
    try std.testing.expect(!TokenKind.identifier.isOperator());
}
