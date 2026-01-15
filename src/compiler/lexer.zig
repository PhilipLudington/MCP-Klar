//! Lexer for the Klar language.
//!
//! Tokenizes Klar source code into a stream of tokens.
//! Matches the Klar language specification from the reference implementation.

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
            '@' => .at,
            '#' => .hash,
            '~' => .tilde,
            '\n' => .newline,

            ':' => if (self.match(':')) .colon_colon else .colon,

            '.' => blk: {
                if (self.match('.')) {
                    if (self.match('=')) break :blk .dot_dot_eq;
                    break :blk .dot_dot;
                }
                break :blk .dot;
            },

            '+' => self.handlePlus(),
            '-' => self.handleMinus(),
            '*' => self.handleStar(),
            '%' => if (self.match('=')) .percent_eq else .percent,
            '^' => .caret,

            '/' => blk: {
                if (self.match('/')) {
                    // Line comment
                    if (self.match('/')) {
                        // Doc comment
                        return self.docComment(start_pos, start_index);
                    }
                    return self.lineComment(start_pos, start_index);
                }
                if (self.match('*')) {
                    // Block comment - skip it
                    self.skipBlockComment();
                    return self.next(); // Recurse to get next real token
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

            '<' => blk: {
                if (self.match('=')) break :blk .lt_eq;
                if (self.match('<')) break :blk .lt_lt;
                break :blk .lt;
            },
            '>' => blk: {
                if (self.match('=')) break :blk .gt_eq;
                if (self.match('>')) break :blk .gt_gt;
                break :blk .gt;
            },

            '&' => .amp,
            '|' => .pipe,
            '?' => if (self.match('?')) .question_question else .question,

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

    fn handlePlus(self: *Lexer) TokenKind {
        if (self.match('%')) return .plus_wrap;
        if (self.match('|')) return .plus_sat;
        if (self.match('=')) return .plus_eq;
        return .plus;
    }

    fn handleMinus(self: *Lexer) TokenKind {
        if (self.match('>')) return .arrow;
        if (self.match('%')) return .minus_wrap;
        if (self.match('|')) return .minus_sat;
        if (self.match('=')) return .minus_eq;
        return .minus;
    }

    fn handleStar(self: *Lexer) TokenKind {
        if (self.match('%')) return .star_wrap;
        if (self.match('|')) return .star_sat;
        if (self.match('=')) return .star_eq;
        return .star;
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

    fn skipBlockComment(self: *Lexer) void {
        while (!self.isAtEnd()) {
            if (self.peek() == '*' and self.peekNext() == '/') {
                _ = self.advance();
                _ = self.advance();
                return;
            }
            _ = self.advance();
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
        // Check for multi-line string """
        if (self.peek() == '"' and self.peekNext() == '"') {
            _ = self.advance(); // second "
            _ = self.advance(); // third "
            return self.multiLineString(start_pos, start_index);
        }

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
        return Token.init(.string_literal, self.makeSpan(start_pos), text);
    }

    fn multiLineString(self: *Lexer, start_pos: Position, start_index: usize) Token {
        while (!self.isAtEnd()) {
            if (self.peek() == '"' and self.peekNext() == '"' and self.peekN(2) == '"') {
                _ = self.advance();
                _ = self.advance();
                _ = self.advance();
                const text = self.source[start_index..self.index];
                return Token.init(.string_literal, self.makeSpan(start_pos), text);
            }
            _ = self.advance();
        }
        const text = self.source[start_index..self.index];
        return Token.init(.invalid, self.makeSpan(start_pos), text);
    }

    fn char(self: *Lexer, start_pos: Position, start_index: usize) Token {
        if (!self.isAtEnd() and self.peek() == '\\') {
            _ = self.advance(); // backslash
            if (!self.isAtEnd()) {
                // Handle unicode escapes \u{...}
                if (self.peek() == 'u' and self.peekNext() == '{') {
                    _ = self.advance(); // u
                    _ = self.advance(); // {
                    while (!self.isAtEnd() and self.peek() != '}') {
                        _ = self.advance();
                    }
                    if (!self.isAtEnd()) _ = self.advance(); // }
                } else {
                    _ = self.advance(); // escape sequence char
                }
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
        return Token.init(.char_literal, self.makeSpan(start_pos), text);
    }

    fn number(self: *Lexer, start_pos: Position, start_index: usize) Token {
        // Handle hex, binary, octal prefixes
        if (self.source[start_index] == '0' and !self.isAtEnd()) {
            switch (self.peek()) {
                'x', 'X' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (isHexDigit(self.peek()) or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.finishNumberWithSuffix(start_pos, start_index, false);
                },
                'b', 'B' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and (self.peek() == '0' or self.peek() == '1' or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.finishNumberWithSuffix(start_pos, start_index, false);
                },
                'o', 'O' => {
                    _ = self.advance();
                    while (!self.isAtEnd() and ((self.peek() >= '0' and self.peek() <= '7') or self.peek() == '_')) {
                        _ = self.advance();
                    }
                    return self.finishNumberWithSuffix(start_pos, start_index, false);
                },
                else => {},
            }
        }

        // Decimal number
        while (!self.isAtEnd() and (isDigit(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }

        var is_float = false;

        // Check for float
        if (!self.isAtEnd() and self.peek() == '.' and
            self.index + 1 < self.source.len and isDigit(self.source[self.index + 1]))
        {
            is_float = true;
            _ = self.advance(); // Consume '.'
            while (!self.isAtEnd() and (isDigit(self.peek()) or self.peek() == '_')) {
                _ = self.advance();
            }
        }

        // Check for exponent
        if (!self.isAtEnd() and (self.peek() == 'e' or self.peek() == 'E')) {
            is_float = true;
            _ = self.advance();
            if (!self.isAtEnd() and (self.peek() == '+' or self.peek() == '-')) {
                _ = self.advance();
            }
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.finishNumberWithSuffix(start_pos, start_index, is_float);
    }

    fn finishNumberWithSuffix(self: *Lexer, start_pos: Position, start_index: usize, is_float: bool) Token {
        // Check for type suffix (i32, u64, f32, etc.)
        if (!self.isAtEnd() and isAlpha(self.peek())) {
            while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
                _ = self.advance();
            }
        }

        const text = self.source[start_index..self.index];
        const kind: TokenKind = if (is_float) .float_literal else .int_literal;
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

    fn peekNext(self: *const Lexer) u8 {
        if (self.index + 1 >= self.source.len) return 0;
        return self.source[self.index + 1];
    }

    fn peekN(self: *const Lexer, n: usize) u8 {
        if (self.index + n >= self.source.len) return 0;
        return self.source[self.index + n];
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

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
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
    var lexer_inst = Lexer.init("fn let var if else return loop", 0);

    try std.testing.expectEqual(TokenKind.kw_fn, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_let, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_var, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_if, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_else, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_return, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.kw_loop, lexer_inst.next().kind);
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
    var lexer_inst = Lexer.init("42 3.14 1e10 2.5e-3 0xff 0b1010 0o755", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.int_literal, tok.kind);
    try std.testing.expectEqualStrings("42", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float_literal, tok.kind);
    try std.testing.expectEqualStrings("3.14", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float_literal, tok.kind);
    try std.testing.expectEqualStrings("1e10", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float_literal, tok.kind);
    try std.testing.expectEqualStrings("2.5e-3", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.int_literal, tok.kind);
    try std.testing.expectEqualStrings("0xff", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.int_literal, tok.kind);
    try std.testing.expectEqualStrings("0b1010", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.int_literal, tok.kind);
    try std.testing.expectEqualStrings("0o755", tok.text);
}

test "lexer strings" {
    var lexer_inst = Lexer.init("\"hello\" \"with\\nescapes\"", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.string_literal, tok.kind);
    try std.testing.expectEqualStrings("\"hello\"", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.string_literal, tok.kind);
    try std.testing.expectEqualStrings("\"with\\nescapes\"", tok.text);
}

test "lexer basic operators" {
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

test "lexer wrapping and saturating operators" {
    var lexer_inst = Lexer.init("+% -% *% +| -| *|", 0);

    try std.testing.expectEqual(TokenKind.plus_wrap, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.minus_wrap, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.star_wrap, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.plus_sat, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.minus_sat, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.star_sat, lexer_inst.next().kind);
}

test "lexer shift operators" {
    var lexer_inst = Lexer.init("<< >>", 0);

    try std.testing.expectEqual(TokenKind.lt_lt, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.gt_gt, lexer_inst.next().kind);
}

test "lexer null coalescing and path separator" {
    var lexer_inst = Lexer.init("?? ::", 0);

    try std.testing.expectEqual(TokenKind.question_question, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.colon_colon, lexer_inst.next().kind);
}

test "lexer compound assignment" {
    var lexer_inst = Lexer.init("+= -= *= /= %=", 0);

    try std.testing.expectEqual(TokenKind.plus_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.minus_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.star_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.slash_eq, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.percent_eq, lexer_inst.next().kind);
}

test "lexer line comments" {
    var lexer_inst = Lexer.init("// comment\n/// doc comment\nfoo", 0);

    try std.testing.expectEqual(TokenKind.comment, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.newline, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.doc_comment, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.newline, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.identifier, lexer_inst.next().kind);
}

test "lexer block comments" {
    var lexer_inst = Lexer.init("a /* block comment */ b", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.identifier, tok.kind);
    try std.testing.expectEqualStrings("a", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.identifier, tok.kind);
    try std.testing.expectEqualStrings("b", tok.text);
}

test "lexer ranges" {
    var lexer_inst = Lexer.init(".. ..=", 0);

    try std.testing.expectEqual(TokenKind.dot_dot, lexer_inst.next().kind);
    try std.testing.expectEqual(TokenKind.dot_dot_eq, lexer_inst.next().kind);
}

test "lexer number with type suffix" {
    var lexer_inst = Lexer.init("42i64 1.5f32", 0);

    var tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.int_literal, tok.kind);
    try std.testing.expectEqualStrings("42i64", tok.text);

    tok = lexer_inst.next();
    try std.testing.expectEqual(TokenKind.float_literal, tok.kind);
    try std.testing.expectEqualStrings("1.5f32", tok.text);
}
