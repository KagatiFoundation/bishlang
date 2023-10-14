//
// This file is part of Bish.
//
// Bish is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Bish is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Bish.  If not, see <https://www.gnu.org/licenses/>.
//

const std = @import("std");
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

pub const TokenType = enum { TOKEN_DEKHAU, TOKEN_GHUMAU, TOKEN_BARABAR, TOKEN_MA, TOKEN_THULO, TOKEN_SANO, TOKEN_CHHAINA, TOKEN_RAKHA, TOKEN_ERROR, TOKEN_STRING, TOKEN_NONE, TOKEN_PLUS, TOKEN_INT, TOKEN_FLOAT, TOKEN_EOF };

var KEYWORDS = std.StringHashMap(TokenType).init(std.heap.page_allocator);
fn init_keywords() !bool {
    _ = try KEYWORDS.put("dekhau", TokenType.TOKEN_DEKHAU);
    _ = try KEYWORDS.put("ghumau", TokenType.TOKEN_GHUMAU);
    _ = try KEYWORDS.put("ma", TokenType.TOKEN_MA);
    _ = try KEYWORDS.put("sano", TokenType.TOKEN_SANO);
    _ = try KEYWORDS.put("thulo", TokenType.TOKEN_THULO);
    _ = try KEYWORDS.put("chhaina", TokenType.TOKEN_CHHAINA);
    _ = try KEYWORDS.put("rakha", TokenType.TOKEN_RAKHA);
    _ = try KEYWORDS.put("barabar", TokenType.TOKEN_BARABAR);
    return true;
}

pub const Token = struct { token_type: TokenType, lexeme: []const u8, literal: []const u8, line: usize, column: usize };

pub fn Scanner(comptime source: []const u8) type {
    const allocator = arena.allocator();

    return struct {
        source: []const u8 = source,
        current: usize = 0,
        start: usize = 0,
        line: usize = 1,
        column: usize = 1,
        tokens: std.ArrayList(Token) = std.ArrayList(Token).init(allocator),
        const Self = @This();

        pub fn scanTokens(self: *Self) !std.ArrayList(Token) {
            // var source_lines = std.mem.splitAny([]const u8, self.source, '\n');
            // while (source_lines.next()) |line| {
            //     self.start = self.current;
            //     self.column = 0;
            //     var tokens = self.extractTokens(line);
            //     _ = tokens;
            // }

            while (!self.isAtEnd()) {
                self.start = self.current;
                const token = self.scanToken();
                if (token.token_type != TokenType.TOKEN_NONE) {
                    _ = try self.tokens.append(token);
                }
            }
            var eof = Token{ .token_type = TokenType.TOKEN_EOF, .lexeme = "", .literal = "", .line = self.line, .column = self.column };
            _ = try self.tokens.append(eof);
            return self.tokens;
        }

        fn scanToken(self: *Self) Token {
            const char = self.advance();
            if (std.ascii.isDigit(char)) {
                while (true) {
                    var peek_value: u8 = self.peek();
                    if (std.ascii.isDigit(peek_value) and !self.isAtEnd()) {
                        _ = self.advance();
                    } else {
                        break;
                    }
                }
                const literal = self.source[self.start..self.current];
                _ = self.advance();
                return Token{ .token_type = TokenType.TOKEN_INT, .lexeme = literal, .literal = literal, .line = self.line, .column = self.column };
            } else if (char == '"' or char == '\'') {
                while (true) {
                    var peek_value: u8 = self.peek();
                    var end: bool = self.isAtEnd();
                    // if (peek_value == single_quote
                    //         or peek_value == double_quote
                    //          or scanner is at end)
                    if (peek_value == char or end) {
                        break;
                    } else {
                        _ = self.advance();
                    }
                }
                if (self.isAtEnd()) {
                    std.debug.print("SyntaxError: Unterminated string literal (detected at line {d}:{d})\n", .{ self.line, self.column });
                    return Token{ .token_type = TokenType.TOKEN_ERROR, .lexeme = "", .literal = "", .line = self.line, .column = self.column };
                }
                const lexeme = self.source[self.start + 1 .. self.current];
                _ = self.advance();
                return Token{ .token_type = TokenType.TOKEN_STRING, .lexeme = lexeme, .literal = lexeme, .line = self.line, .column = self.column };
            } else {
                if (char == 10) {
                    self.column = 0;
                }
                return Token{ .token_type = TokenType.TOKEN_NONE, .lexeme = "", .literal = "", .line = self.line, .column = self.column };
            }
        }

        fn peek(self: *Self) u8 {
            if (self.isAtEnd()) {
                return 0;
            }
            return self.source[self.current];
        }

        fn advance(self: *Self) u8 {
            if (self.current < self.source.len) {
                const now: usize = self.current;
                self.current += 1;
                self.column += 1;
                return self.source[now];
            } else {
                return 0;
            }
        }

        fn isAtEnd(self: *Self) bool {
            return self.current >= self.source.len;
        }

        pub fn destroy(self: *Self) void {
            _ = self;
            arena.deinit();
            KEYWORDS.deinit();
        }
    };
}

pub fn init() void {
    if (init_keywords()) |status| {
        _ = status;
    } else |err| {
        std.debug.print("Some error while initializing the scanner: {}\n", .{err});
    }
}

pub fn token_dump(token: *const Token) void {
    std.debug.print("Token[ {}, {s}, {s} ]\n", .{ token.token_type, token.lexeme, token.literal });
}
