const std = @import("std");

const TokenType = enum {
    TOKEN_DEKHAU,
    TOKEN_GHUMAU,
    TOKEN_BARABAR,
    TOKEN_MA,
    TOKEN_THULO,
    TOKEN_SANO,
    TOKEN_CHHAINA,
    TOKEN_RAKHA,
};

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

const Token = struct { token_type: TokenType, lexeme: []u8, literal: []u8, line: i32, column: i32 };

fn token_new(token_type: TokenType, lexeme: []u8, literal: []u8, line: i32, column: i32) Token {
    return Token{ .token_type = token_type, .lexeme = lexeme, .literal = literal, .line = line, .column = column };
}

// scanner to scan through the source code
const Scanner = struct { source: []u8, tokens: std.ArrayList(Token), current: i32, start: i32, line: i32 };

fn scanner_new(source: []u8) Scanner {
    return Scanner{ .source = source, .tokens = std.ArrayList(Token).init(std.heap.page_allocator), .current = 0, .start = 0, .line = 0 };
}
