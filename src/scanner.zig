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
var allocator = arena.allocator();

pub const TokenType = enum { TOKEN_DEKHAU, TOKEN_GHUMAU, TOKEN_BARABAR, TOKEN_MA, TOKEN_THULO, TOKEN_SANO, TOKEN_CHHAINA, TOKEN_RAKHA, TOKEN_ERROR, TOKEN_STRING, TOKEN_NONE, TOKEN_PLUS, TOKEN_EOF };

const KEYWORDS = std.StringHashMap(TokenType).init(std.heap.page_allocator);
fn init_keywords() !void {
    KEYWORDS.put("dekhau", TokenType.TOKEN_DEKHAU);
    KEYWORDS.put("ghumau", TokenType.TOKEN_GHUMAU);
    KEYWORDS.put("ma", TokenType.TOKEN_MA);
    KEYWORDS.put("SANO", TokenType.TOKEN_SANO);
    KEYWORDS.put("THULO", TokenType.TOKEN_THULO);
    KEYWORDS.put("CHHAINA", TokenType.TOKEN_CHHAINA);
    KEYWORDS.put("rakha", TokenType.TOKEN_RAKHA);
    KEYWORDS.put("barabar", TokenType.TOKEN_BARABAR);
}

pub const Token = struct { token_type: TokenType, lexeme: []const u8, literal: []const u8, line: usize, column: usize };

pub fn token_new(token_type: TokenType, lexeme: []const u8, literal: []const u8, line: usize, column: usize) Token {
    return Token{ .token_type = token_type, .lexeme = lexeme, .literal = literal, .line = line, .column = column };
}

// scanner to scan through the source code
pub const Scanner = struct { source: []const u8, tokens: std.ArrayList(Token), current: usize, start: usize, line: usize, column: usize };

pub fn scanner_new(source: []const u8) Scanner {
    return Scanner{ .source = source, .tokens = std.ArrayList(Token).init(allocator), .current = 0, .start = 0, .line = 1, .column = 1 };
}

pub fn scanner_scan_tokens(scanner: *Scanner) !std.ArrayList(Token) {
    while (!scanner_is_at_end(scanner)) {
        scanner.start = scanner.current;
        const token = scanner_scan_token(scanner);
        if (token.token_type != TokenType.TOKEN_NONE) {
            _ = try scanner.tokens.append(token);
        }
    }
    var endOfSource = Token{ .token_type = TokenType.TOKEN_EOF, .lexeme = "", .literal = "", .line = scanner.line, .column = scanner.column };
    _ = try scanner.tokens.append(endOfSource);
    return scanner.tokens;
}

fn scanner_scan_token(scanner: *Scanner) Token {
    const char = scanner_advance(scanner);
    if (char == '"') {
        while (true) {
            var peek_value: u8 = scanner_peek(scanner);
            var end: bool = scanner_is_at_end(scanner);
            if (peek_value == '"' or end) {
                break;
            } else {
                _ = scanner_advance(scanner);
            }
        }
        if (scanner_is_at_end(scanner)) {
            std.debug.print("SyntaxError: Unterminated string literal (detected at line {d})\n", .{scanner.line});
            return token_new(TokenType.TOKEN_ERROR, "", "", scanner.line, scanner.line);
        }
        _ = scanner_advance(scanner);
        const lexeme = scanner.source[scanner.start + 1 .. scanner.current - 1];
        return token_new(TokenType.TOKEN_STRING, lexeme, lexeme, scanner.line, scanner.column);
    } else {
        if (char == 10 or char == ' ') {
            return token_new(TokenType.TOKEN_NONE, "", "", scanner.line, scanner.column);
        }
        return token_new(TokenType.TOKEN_ERROR, "", "", scanner.line, scanner.column);
    }
}

fn scanner_peek(scanner: *Scanner) u8 {
    if (scanner_is_at_end(scanner)) {
        return 0;
    }
    return scanner.source[scanner.current];
}

fn scanner_advance(scanner: *Scanner) u8 {
    var now = scanner.current;
    scanner.current += 1;
    return scanner.source[now];
}

fn scanner_is_at_end(scanner: *Scanner) bool {
    return scanner.current >= scanner.source.len;
}

pub fn token_dump(token: *const Token) void {
    std.debug.print("Token[ {}, {s}, {s} ]\n", .{ token.token_type, token.lexeme, token.literal });
}

pub fn scanner_destroy() void {
    arena.deinit();
}
