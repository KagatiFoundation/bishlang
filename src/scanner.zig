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

const ScannerError = struct {
    pub const E = enum { SYNTAX_ERROR, UNTERMINATED_STR, INVALID_NUM_VALUE };
    err_type: E,
    err_subtype: E,
    token: Token,
    const Self = @This();
    pub var errors = std.AutoHashMap(E, []const u8).init(std.heap.page_allocator);

    fn init_errors() !bool {
        _ = try errors.put(E.SYNTAX_ERROR, "SyntaxError");
        _ = try errors.put(E.UNTERMINATED_STR, "Unterminated string literal");
        _ = try errors.put(E.INVALID_NUM_VALUE, "Invalid numeric literal");
        return true;
    }

    pub fn dump(self: Self) void {
        var errt: []const u8 = undefined;
        if (errors.get(self.err_type)) |value| {
            errt = value;
        }
        var errst: []const u8 = undefined;
        if (errors.get(self.err_subtype)) |value| {
            errst = value;
        }
        std.debug.print("{s}: {s} (detected at line {d}:{d})\n", .{ errt, errst, self.token.line, self.token.column });
    }
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: []const u8,
    line: usize,
    column: usize,
    const Self = @This();

    pub fn dump(self: Self) void {
        std.debug.print("Token[ {?}, {s}, {s}, {d}:{d} ]\n", .{ self.token_type, self.lexeme, self.literal, self.line, self.column });
    }
};

pub fn Scanner(comptime source: []const u8) type {
    const allocator = arena.allocator();
    return struct {
        source: []const u8 = source,
        line: usize = 1,
        tokens: std.ArrayList(Token) = std.ArrayList(Token).init(allocator),
        errors: std.ArrayList(ScannerError) = std.ArrayList(ScannerError).init(allocator),
        has_error: bool = false,
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

            // dumping errors(if any)
            if (self.has_error) {
                for (self.errors.items) |item| {
                    item.dump();
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
                    const token = Token{ .token_type = TokenType.TOKEN_INT, .lexeme = literal, .literal = literal, .line = self.line, .column = start + 1 };
                    _ = try tokens.append(token);
                } else if (line[pos] == '"' or line[pos] == '\'') {
                    var tmpPos = self.parseStr(pos, line);
                    if (tmpPos == 0xFFFFFFFF) {
                        return tokens;
                    }
                    pos = tmpPos;
                    const literal = line[start + 1 .. pos];
                    const token = Token{ .token_type = TokenType.TOKEN_STRING, .lexeme = literal, .literal = literal, .line = self.line, .column = start + 1 };
                    _ = try tokens.append(token);
                    pos += 1; // advance past the end quotation
                } else {
                    _ = try tokens.append(Token{ .token_type = TokenType.TOKEN_NONE, .lexeme = "", .literal = "", .line = 0, .column = 0 });
                    pos += 1;
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
                    if (!isEnd) {
                        var _peek: u8 = line[mutablePos];
                        if (_peek == quoteStyle) {
                            break;
                        }
                        mutablePos += 1;
                    } else {
                        const errTok = Token{ .token_type = TokenType.TOKEN_ERROR, .lexeme = "", .literal = "", .line = self.line, .column = pos + 1 };
                        var err = ScannerError{ .err_type = ScannerError.E.SYNTAX_ERROR, .err_subtype = ScannerError.E.UNTERMINATED_STR, .token = errTok };
                        self.errors.append(err) catch |_err| {
                            std.debug.print("{?}", .{_err});
                        };
                        self.has_error = true;
                        return 0xFFFFFFFF;
                    }
                }
            }
            return mutablePos;
        }

        fn isAtEnd(self: *Self) bool {
            return self.current >= self.source.len;
        }
    };
}

pub fn init() void {
    if (init_keywords()) |status| {
        _ = status;
    } else |err| {
        std.debug.print("Some error while initializing the scanner: {?}\n", .{err});
        return;
    }

    if (ScannerError.init_errors()) |status| {
        _ = status;
    } else |err| {
        std.debug.print("Some error while initializing the scanner: {?}\n", .{err});
        return;
    }
}

pub fn deinit() void {
    arena.deinit();
    KEYWORDS.deinit();
    ScannerError.errors.deinit();
}
