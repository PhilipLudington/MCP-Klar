//! Lexer for the Klar language.
//!
//! Tokenizes Klar source code into a stream of tokens.

const std = @import("std");
const utils = @import("utils");
const token = @import("token.zig");

const Position = utils.Position;
const Span = utils.Span;
const Token = token.Token;
const TokenKind = token.TokenKind;
const keywords = token.keywords;

/// Lexer for tokenizing Klar source code.
pub const Lexer = struct {
    source: []const u8,
    pos: Position,
    file_id: u32,
    index: usize,

    pub fn init(source: []const u8, file_id: u32) Lexer {
        return .{
            .source = source,
            .pos = Position.start(),
            .file_id = file_id,
            .index = 0,
        };
    }

    /// Returns the next token.
    pub fn next(self: *Lexer) Token {
        self.skipWhitespace();

        if (self.isAtEnd()) {
            return Token.eof(Span.point(self.pos, self.file_id));
        }

        const start_pos = self.pos;
        const start_index = self.index;
        const c = self.advance();

        // Single-character tokens
        const kind: TokenKind = switch (c) {
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lbrace,
            '}' => .rbrace,
            '[' => .lbracket,
            ']' => .rbracket,
            ',' => .comma,
            ';' => .semicolon,
            '?' => .question,
            '@' => .at,
            '#' => .hash,
            '~' => .tilde,
            '\n' => .newline,

            ':' => .colon,
            '.' => blk: {
                if (self.match('.')) {
                    if (self.match('=')) break :blk .dot_dot_eq;
                    break :blk .dot_dot;
                }
                break :blk .dot;
            },

            '+' => if (self.match('=')) .plus_eq else .plus,
            '-' => blk: {
                if (self.match('>')) break :blk .arrow;
                if (self.match('=')) break :blk .minus_eq;
                break :blk .minus;
            },
            '*' => if (self.match('=')) .star_eq else .star,
            '%' => .percent,
            '^' => .caret,

            '/' => blk: {
                if (self.match('/')) {
                    // Comment
                    if (self.match('/')) {
                        // Doc comment
                        return self.docComment(start_pos, start_index);
                    }
                    return self.lineComment(start_pos, start_index);
                }
                if (self.match('=')) break :blk .slash_eq;
                break :blk .slash;
            },

            '=' => blk: {
                if (self.match('=')) break :blk .eq_eq;
                if (self.match('>')) break :blk .fat_arrow;
                break :blk .eq;
            },
            '!' => if (self.match('=')) .bang_eq else .bang,
            '<' => if (self.match('=')) .lt_eq else .lt,
            '>' => if (self.match('=')) .gt_eq else .gt,

            '&' => if (self.match('&')) .ampersand_ampersand else .ampersand,
            '|' => if (self.match('|')) .pipe_pipe else .pipe,

            '"' => return self.string(start_pos, start_index),
            '\'' => return self.char(start_pos, start_index),

            else => blk: {
                if (isDigit(c)) {
                    return self.number(start_pos, start_index);
                }
                if (isAlpha(c)) {
                    return self.identifier(start_pos, start_index);
                }
                break :blk .invalid;
            },
        };

        const text = self.source[start_index..self.index];
        return Token.init(kind, self.makeSpan(start_pos), text);
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => _ = self.advance(),
                else => return,
            }
        }
    }

    fn lineComment(self: *Lexer, start_pos: Position, start_index: usize) Token {
        while (!self.isAtEnd() and self.peek() != '\n') {
            _ = self.advance();
        }
        const text = self.source[start_index..self.index];
        return Token.init(.comment, self.makeSpan(start_pos), text);
    }

    fn docComment(self: *Lexer, start_pos: Position, start_index: usize) Token {
        while (!self.isAtEnd() and self.peek() != '\n') {
            _ = self.advance();
        }
        const text = self.source[start_index..self.index];
        return Token.init(.doc_comment, self.makeSpan(start_pos), text);
    }

    fn string(self: *Lexer, start_pos: Position, start_index: usize) Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\\') {
                _ = self.advance(); // Skip backslash
                if (!self.isAtEnd()) {
                    _ = self.advance(); // Skip escaped char
                }
            } else {
                _ = self.advance();
            }
        }

        if (self.isAtEnd()) {
            // Unterminated string
            const text = self.source[start_index..self.index];
            return Token.init(.invalid, self.makeSpan(start_pos), text);
        }

        _ = self.advance(); // Closing quote
        const text = self.source[start_index..self.index];
        return Token.init(.string, self.makeSpan(start_pos), text);
    }

    fn char(self: *Lexer, start_pos: Position, start_index: usize) Token {
        if (!self.isAtEnd() and self.peek() == '\\') {
            _ = self.advance();
            if (!self.isAtEnd()) {
                _ = self.advance();
            }
        } else if (!self.isAtEnd()) {
            _ = self.advance();
        }

        if (self.isAtEnd() or self.peek() != '\'') {
            const text = self.source[start_index..self.index];
            return Token.init(.invalid, self.makeSpan(start_pos), text);
        }

        _ = self.advance(); // Closing quote
        const text = self.source[start_index..self.index];
        return Token.init(.char, self.makeSpan(start_pos), text);
    }

    fn number(self: *Lexer, start_pos: Position, start_index: usize) Token {
        while (!self.isAtEnd() and isDigit(self.peek())) {
            _ = self.advance();
        }

        var kind: TokenKind = .integer;

        // Check for float
        if (!self.isAtEnd() and self.peek() == '.' and
            self.index + 1 < self.source.len and isDigit(self.source[self.index + 1]))
        {
            kind = .float;
            _ = self.advance(); // Consume '.'
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Check for exponent
        if (!self.isAtEnd() and (self.peek() == 'e' or self.peek() == 'E')) {
            kind = .float;
            _ = self.advance();
            if (!self.isAtEnd() and (self.peek() == '+' or self.peek() == '-')) {
                _ = self.advance();
            }
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        const text = self.source[start_index..self.index];
        return Token.init(kind, self.makeSpan(start_pos), text);
    }

    fn identifier(self: *Lexer, start_pos: Position, start_index: usize) Token {
        while (!self.isAtEnd() and (isAlphaNumeric(self.peek()))) {
            _ = self.advance();
        }

        const text = self.source[start_index..self.index];
        const kind = keywords.get(text) orelse .identifier;
        return Token.init(kind, self.makeSpan(start_pos), text);
    }

    fn makeSpan(self: *const Lexer, start: Position) Span {
        return .{
            .start = start,
            .end = self.pos,
            .file_id = self.file_id,
        };
    }

    fn peek(self: *const Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.index];
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.index];
        self.index += 1;
        self.pos = self.pos.advance(c);
        return c;
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.index] != expected) return false;
        _ = self.advance();
        return true;
    }

    fn isAtEnd(self: *const Lexer) bool {
        return self.index >= self.source.len;
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

test "lexer basic tokens" {
    var lexer_inst = Lexer.init("( ) { } [ ]", 0);

    try std.testing.expectEqual(TokenKind.lparen, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.rparen, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.lbrace, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.rbrace, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.lbracket, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.rbracket, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.eof, lexer_inst.next().kind);
}

test "lexer keywords" {
    var lexer_inst = Lexer.init("fn let if else return", 0);

    try std.testing.expectEqual(TokenKind.kw_fn, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_let, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_if, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_else, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_return, lexer_inst.next().kind);
}

test "lexer identifiers" {
    var lexer_inst = Lexer.init("foo bar_baz _private", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.identifier, tok.kind);
    try std.testing.expectEqualStrings("foo", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.identifier, tok.kind);
    try std.testing.expectEqualStrings("bar_baz", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.identifier, tok.kind);
    try std.testing.expectEqualStrings("_private", tok.text);
}

test "lexer numbers" {
    var lexer_inst = Lexer.init("42 3.14 1e10 2.5e-3", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.integer, tok.kind);
    try std.testing.expectEqualStrings("42", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float, tok.kind);
    try std.testing.expectEqualStrings("3.14", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float, tok.kind);
    try std.testing.expectEqualStrings("1e10", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float, tok.kind);
    try std.testing.expectEqualStrings("2.5e-3", tok.text);
}

test "lexer strings" {
    var lexer_inst = Lexer.init("\"hello\" \"with\\nescapes\"", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.string, tok.kind);
    try std.testing.expectEqualStrings("\"hello\"", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.string, tok.kind);
    try std.testing.expectEqualStrings("\"with\\nescapes\"", tok.text);
}

test "lexer operators" {
    var lexer_inst = Lexer.init("+ - * / == != <= >= -> =>", 0);

    try std.testing.expectEqual(TokenKind.plus, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.minus, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.star, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.slash, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.eq_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.bang_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.lt_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.gt_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.arrow, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.fat_arrow, lexer_inst.next().kind);
}

test "lexer comments" {
    var lexer_inst = Lexer.init("// comment\n/// doc comment\nfoo", 0);

    try std.testing.expectEqual(TokenKind.comment, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.newline, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.doc_comment, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.newline, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.identifier, lexer_inst.next().kind);
}
