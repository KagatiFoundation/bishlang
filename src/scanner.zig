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
// var pointers_to_be_freed = std.ArrayList(type);

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

//fn SyntaxError(msg: []const u8, token: Token) type {
//return struct {
//msg: []const u8 = msg,
//token: Token = token,
//const Self = @This();

//fn dump(self: *Self) void {
//_ = self;
//}

//fn to_string(self: *Self) []u8 {
//return std.fmt.allocPrint(std.heap.page_allocator, "SyntaxError: {s}", .{self.msg});
//}
//};
//}

pub const Token = struct { token_type: TokenType, lexeme: []const u8, literal: []const u8, line: usize, column: usize };

pub fn Scanner(comptime source: []const u8) type {
    const allocator = arena.allocator();
    return struct {
        source: []const u8 = source,
        line: usize = 1,
        tokens: std.ArrayList(Token) = std.ArrayList(Token).init(allocator),
        const Self = @This();

        pub fn scanTokens(self: *Self) !std.ArrayList(Token) {
            var source_lines = std.mem.split(u8, self.source, "\n");
            while (source_lines.next()) |line| {
                if (line.len == 0) {
                    continue;
                }
                var tokens = try self.scanTokensLine(line);
                for (tokens.items) |item| {
                    _ = try self.tokens.append(item);
                }
            }
            const eof: Token = Token{ .token_type = TokenType.TOKEN_EOF, .literal = "", .lexeme = "", .line = 0, .column = 0 };
            _ = try self.tokens.append(eof);
            return self.tokens;
        }

        fn scanTokensLine(self: *Self, line: []const u8) !std.ArrayList(Token) {
            var pos: usize = 0;
            var start: usize = 0;
            var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(allocator);
            const len: usize = line.len;
            while (pos < len) {
                start = pos;
                if (std.ascii.isDigit(line[pos])) {
                    pos = self.parseInt(pos, line);
                    const literal = line[start..pos];
                    const token = Token{ .token_type = TokenType.TOKEN_INT, .lexeme = literal, .literal = literal, .line = self.line, .column = start };
                    _ = try tokens.append(token);
                } else if (line[pos] == '"' or line[pos] == '\'') {
                    var tmpPos = self.parseStr(pos, line);
                    if (tmpPos == 0xFFFFFFFF) {
                        _ = try tokens.append(Token{ .token_type = TokenType.TOKEN_ERROR, .lexeme = "", .literal = "", .line = self.line, .column = start });
                    }
                    pos = tmpPos;
                    const literal = line[start + 1 .. pos];
                    const token = Token{ .token_type = TokenType.TOKEN_STRING, .lexeme = literal, .literal = literal, .line = self.line, .column = start };
                    _ = try tokens.append(token);
                    pos += 1; // advance past the end quotation
                } else {
                    _ = try tokens.append(Token{ .token_type = TokenType.TOKEN_NONE, .lexeme = "", .literal = "", .line = self.line, .column = 0 });
                }
            }
            return tokens;
        }

        fn parseInt(self: *Self, pos: usize, line: []const u8) usize {
            _ = self;
            var mutablePos: usize = pos;
            const len = line.len;
            mutablePos += 1;
            if (mutablePos < len) {
                const char = line[mutablePos];
                while (mutablePos < len and std.ascii.isDigit(char)) {
                    mutablePos += 1;
                }
            }
            return mutablePos;
        }

        fn parseStr(self: *Self, pos: usize, line: []const u8) usize {
            var mutablePos: usize = pos;
            const quoteStyle = line[mutablePos];
            const len = line.len;
            mutablePos += 1;
            if (mutablePos < len) {
                while (true) {
                    var isEnd: bool = mutablePos >= len;
                    var _peek: u8 = line[mutablePos];
                    if (_peek == quoteStyle or isEnd) {
                        break;
                    }
                    mutablePos += 1;
                }
                if (mutablePos >= len) {
                    std.debug.print("SyntaxError: Unterminated string literal (detected at line {d}:{d})\n", .{ self.line, mutablePos });
                    return 0xFFFFFFFF;
                }
            }
            return mutablePos;
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
