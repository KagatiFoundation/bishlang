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

pub const TokenType = enum {
    TOKEN_DEKHAU, // dekhau = "print"
    TOKEN_GHUMAU, // ghumau = "loop"
    TOKEN_BARABAR, // barabar = "=="
    TOKEN_MA, // ma = "="
    TOKEN_THULO, // thulo = ">"
    TOKEN_SANO, // sano = "<"
    TOKEN_CHHAINA, // chhaina = "!"
    TOKEN_RAKHA, // rakha = "var" | "let" | "const"
    KW_PATAK, //
    KW_YADI,
    KW_NATRA,
    KW_SURU,
    KW_ANTYA,
    KW_JABASAMMA,
    KW_YA, // || operator
    KW_RA, // && operator
    KW_CHHA, //
    KW_KARYA, // keyword to declare function
    KW_GARA, // keyword to call a function
    TOKEN_COMMA, // ,
    KW_FARKAU, // 'return' in other mainstream languages
    KW_NIL, // 'null'
    TOKEN_PIPE,
    TOKEN_ERROR,
    TOKEN_STRING,
    TOKEN_NONE,
    TOKEN_PLUS,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_IDENTIFIER,
    TOKEN_SEMICOLON,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_MINUS,
    TOKEN_PERCENTAGE,
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_GALAT, // galat = "false"
    TOKEN_SAHI, // sahi = "true"
    TOKEN_POWER, // power = "**"
    TOKEN_EOF,
};

var KEYWORDS = std.StringHashMap(TokenType).init(std.heap.page_allocator);
fn init_keywords() !bool {
    _ = try KEYWORDS.put("dekhau", TokenType.TOKEN_DEKHAU);
    _ = try KEYWORDS.put("ghumau", TokenType.TOKEN_GHUMAU);
    _ = try KEYWORDS.put("ma", TokenType.TOKEN_MA);
    _ = try KEYWORDS.put("sano", TokenType.TOKEN_SANO);
    _ = try KEYWORDS.put("thulo", TokenType.TOKEN_THULO);
    _ = try KEYWORDS.put("chhaina", TokenType.TOKEN_CHHAINA);
    _ = try KEYWORDS.put("chha", TokenType.KW_CHHA);
    _ = try KEYWORDS.put("rakha", TokenType.TOKEN_RAKHA);
    _ = try KEYWORDS.put("barabar", TokenType.TOKEN_BARABAR);
    _ = try KEYWORDS.put("galat", TokenType.TOKEN_GALAT);
    _ = try KEYWORDS.put("sahi", TokenType.TOKEN_SAHI);
    _ = try KEYWORDS.put("yadi", TokenType.KW_YADI);
    _ = try KEYWORDS.put("natra", TokenType.KW_NATRA);
    _ = try KEYWORDS.put("suru", TokenType.KW_SURU);
    _ = try KEYWORDS.put("antya", TokenType.KW_ANTYA);
    _ = try KEYWORDS.put("ra", TokenType.KW_RA);
    _ = try KEYWORDS.put("ya", TokenType.KW_YA);
    _ = try KEYWORDS.put("patak", TokenType.KW_PATAK);
    _ = try KEYWORDS.put("karya", TokenType.KW_KARYA);
    _ = try KEYWORDS.put("gara", TokenType.KW_GARA);
    _ = try KEYWORDS.put("farkau", TokenType.KW_FARKAU);
    return true;
}

const ScannerError = struct {
    pub const E = enum { SYNTAX_ERROR, UNTERMINATED_STR, INVALID_NUM_VALUE };
    err_type: E,
    err_subtype: E,
    src_line: []const u8,
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
        std.debug.print("    {s}\n", .{self.src_line});
        for (0..self.token.column - 1) |_| {
            std.debug.print(" ", .{});
        }
        std.debug.print("    ^\n", .{});
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

pub const Scanner = struct {
    source: []const u8,
    line: usize = 0,
    tokens: std.ArrayList(Token),
    errors: std.ArrayList(ScannerError),
    has_error: bool,
    allocator: std.mem.Allocator,
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        if (init_keywords()) |status| {
            _ = status;
        } else |err| {
            std.debug.panic("Some error while initializing the scanner: {?}\n", .{err});
        }
        if (ScannerError.init_errors()) |status| {
            _ = status;
        } else |err| {
            std.debug.panic("Some error while initializing the scanner: {?}\n", .{err});
        }
        return Self{
            .source = source,
            .line = 0,
            .tokens = std.ArrayList(Token).init(allocator),
            .errors = std.ArrayList(ScannerError).init(allocator),
            .has_error = false,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.errors.deinit();
        KEYWORDS.deinit();
        ScannerError.errors.deinit();
    }

    pub fn scanTokens(self: *Self) !std.ArrayList(Token) {
        var source_lines = std.mem.split(u8, self.source, "\n");
        while (source_lines.next()) |line| {
            self.line += 1; // increment line count by one
            if (line.len == 0) {
                continue;
            }
            var tokens = try self.scanTokensLine(line);
            for (tokens.items) |item| {
                _ = try self.tokens.append(item);
            }
            tokens.deinit();
        }
        // dumping errors(if any)
        if (self.has_error) {
            for (self.errors.items) |item| {
                item.dump();
            }
        }
        const last_token: Token = self.tokens.getLast();
        const eof: Token = Token{ .token_type = TokenType.TOKEN_EOF, .literal = "", .lexeme = "", .line = self.line, .column = (last_token.column + last_token.lexeme.len) };
        _ = try self.tokens.append(eof);
        return self.tokens;
    }

    fn scanTokensLine(self: *Self, line: []const u8) !std.ArrayList(Token) {
        var pos: usize = 0;
        var start: usize = 0;
        var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(self.allocator);
        const len: usize = line.len;
        while (pos < len) {
            start = pos;
            const char = line[pos];
            if (std.ascii.isDigit(char)) {
                pos = self.parseInt(pos, line);
                const literal = line[start..pos];
                const token = Token{ .token_type = TokenType.TOKEN_FLOAT, .lexeme = literal, .literal = literal, .line = self.line, .column = start + 1 };
                _ = try tokens.append(token);
            } else if (char == '"' or char == '\'') {
                var tmpPos = self.parseStr(pos, line);
                if (tmpPos == 0xFFFFFFFF) {
                    return tokens;
                }
                pos = tmpPos;
                const literal = line[start + 1 .. pos];
                const token = Token{ .token_type = TokenType.TOKEN_STRING, .lexeme = literal, .literal = literal, .line = self.line, .column = start + 1 };
                _ = try tokens.append(token);
                pos += 1; // advance past the end quotation
            } else if (std.ascii.isAlphabetic(char) or char == '_') {
                pos = self.parseKeywordOrIdentifier(pos, line);
                const lexeme = line[start..pos];
                var tok = self._nonLiteralToken(TokenType.TOKEN_IDENTIFIER, lexeme, start + 1);
                if (KEYWORDS.get(lexeme)) |keywordTokType| {
                    tok.token_type = keywordTokType;
                }
                _ = try tokens.append(tok);
            } else {
                if (char == '+') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_PLUS, "+", start + 1));
                } else if (char == '-') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_MINUS, "-", start + 1));
                } else if (char == '*') {
                    if ((pos + 1) < len) {
                        if (line[pos + 1] == '*') {
                            _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_POWER, "**", start + 1));
                            pos += 2;
                            continue;
                        }
                    }
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_STAR, "*", start + 1));
                } else if (char == '/') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_SLASH, "/", start + 1));
                } else if (char == ';') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_SEMICOLON, ";", start + 1));
                } else if (char == '(') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_LEFT_PAREN, "(", start + 1));
                } else if (char == ')') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_RIGHT_PAREN, ")", start + 1));
                } else if (char == '|') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_PIPE, "|", start + 1));
                } else if (char == '%') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_PERCENTAGE, "%", start + 1));
                } else if (char == ',') {
                    _ = try tokens.append(self._nonLiteralToken(TokenType.TOKEN_COMMA, ",", start + 1));
                }
                pos += 1;
            }
        }
        return tokens;
    }

    fn parseKeywordOrIdentifier(self: *Self, pos: usize, line: []const u8) usize {
        _ = self;
        var mutablePos: usize = pos;
        mutablePos += 1; // skip past the first character
        const len = line.len;
        if (mutablePos < len) {
            var char = line[mutablePos];
            while (char == '_' or std.ascii.isAlphabetic(char) or std.ascii.isDigit(char)) {
                mutablePos += 1;
                if (mutablePos >= len) {
                    break;
                }
                char = line[mutablePos];
            }
        }
        return mutablePos;
    }

    fn parseInt(self: *Self, pos: usize, line: []const u8) usize {
        _ = self;
        var mutablePos: usize = pos;
        const len = line.len;
        mutablePos += 1;
        if (mutablePos < len) {
            var char = line[mutablePos];
            while (mutablePos < len and std.ascii.isDigit(char)) {
                mutablePos += 1;
                if (mutablePos >= len) {
                    break;
                }
                char = line[mutablePos];
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
                    self.genUnterminatedStrErr(pos, line);
                    return 0xFFFFFFFF;
                }
            }
        } else {
            self.genUnterminatedStrErr(pos, line);
            return 0xFFFFFFFF;
        }
        return mutablePos;
    }

    fn genUnterminatedStrErr(self: *Self, pos: usize, line: []const u8) void {
        const errTok = Token{ .token_type = TokenType.TOKEN_ERROR, .lexeme = "", .literal = "", .line = self.line, .column = pos + 1 };
        var err = ScannerError{ .err_type = ScannerError.E.SYNTAX_ERROR, .err_subtype = ScannerError.E.UNTERMINATED_STR, .token = errTok, .src_line = line };
        self.errors.append(err) catch |_err| {
            std.debug.print("{?}", .{_err});
        };
        self.has_error = true;
    }

    fn _nonLiteralToken(self: *Self, tok_type: TokenType, lexeme: []const u8, column: usize) Token {
        return Token{ .token_type = tok_type, .lexeme = lexeme, .literal = lexeme, .line = self.line, .column = column };
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }
};
