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
const scanner = @import("./scanner.zig");
const ast = @import("./ast.zig");
const errorutil = @import("./utils//errorutil.zig");

pub const Parser = struct {
    source_lines: std.mem.SplitIterator(u8, .sequence),
    tokens: std.ArrayList(scanner.Token),
    current: usize,
    tokens_len: usize,
    const Self = @This();

    pub fn init(source: []const u8, tokens: std.ArrayList(scanner.Token)) Parser {
        errorutil.initErrorEngine(source);
        return .{
            .source_lines = std.mem.split(u8, source, "\n"),
            .tokens = tokens,
            .current = 0,
            .tokens_len = tokens.items.len,
        };
    }

    pub fn parse(self: *Self) !std.ArrayList(ast.Stmt) {
        var stmts: std.ArrayList(ast.Stmt) = std.ArrayList(ast.Stmt).init(std.heap.page_allocator);
        while (!self.isAtEnd()) {
            var stmtOrNull: ?ast.Stmt = self.parseStmt();
            if (stmtOrNull) |stmt| {
                _ = try stmts.append(stmt);
            }
        }
        return stmts;
    }

    fn parseStmt(self: *Self) ?ast.Stmt {
        var now: scanner.Token = self.peek();
        switch (now.token_type) {
            .TOKEN_DEKHAU => {
                self.current += 1; // step past 'dekhau'
                var dekhauStmt = self.parseDekhauStmt();
                return dekhauStmt;
            },
            .TOKEN_RAKHA => {
                self.current += 1;
                var rakhaStmt = self.parseRakhaStmt();
                return rakhaStmt;
            },
            else => {
                self.current += 1;
                return null;
            },
        }
    }

    fn parseRakhaStmt(self: *Self) ?ast.Stmt {
        var var_name: scanner.Token = self.peek();
        if (var_name.token_type != scanner.TokenType.TOKEN_IDENTIFIER) {
            errorutil.reportErrorFatal(var_name, "'rakha' pacchi variable ko naam dinus", "yaha variable ko naam huna parchha");
            return null;
        }
        self.current += 1; // skip past identifier name

        var now: scanner.Token = self.peek(); // 'ma' keyword
        if (now.token_type != scanner.TokenType.TOKEN_MA) {
            errorutil.reportErrorFatal(now, "variable ko naam pachhi 'ma' lekhnus", "variable ma value store garna 'ma' keyword lekhnu parne hunchha");
            return null;
        }
        self.current += 1; // skip past 'ma' keyword

        now = self.peek();
        if (now.token_type == scanner.TokenType.TOKEN_SEMICOLON or now.token_type == scanner.TokenType.TOKEN_EOF) {
            errorutil.reportErrorFatal(now, "yaha tapaile variable ko lagi value dina parne hunchha", null);
            return null;
        }

        var expr: ast.Expr = undefined;
        if (self.parsePrimary()) |_expr| {
            expr = _expr;
        }

        now = self.peek();
        if (now.token_type != scanner.TokenType.TOKEN_SEMICOLON) {
            errorutil.reportErrorFatal(now, "variable ma value rakhi sakepachhi ';' lekhnus", "yaha ';' lekhnus");
            return null;
        }
        self.current += 1; // skip past ';'
        return ast.Stmt{
            .RakhaStmt = .{
                .var_name = var_name.lexeme,
                .expr = expr,
            },
        };
    }

    fn parseDekhauStmt(self: *Self) ?ast.Stmt {
        var expr: ?ast.Expr = self.parsePrimary();
        if (expr) |exp| {
            // DEKHAU <expression>;
            // expecting ';' after expression
            var now: scanner.Token = self.peek();
            if (now.token_type != scanner.TokenType.TOKEN_SEMICOLON) {
                if (now.token_type == scanner.TokenType.TOKEN_EOF) {
                    errorutil.reportErrorFatal(now, "expected ';' after expression but program reached end of file", null);
                } else {
                    var msgFmt = "chaiyeko ';' tara vetiyo '{s}'";
                    var errorMsg: [msgFmt.len + 100]u8 = undefined; // easter egg - do not enter characters more than 100
                    @memset(&errorMsg, 0);
                    _ = std.fmt.bufPrint(&errorMsg, "chaiyeko ';' tara vetiyo '{s}'", .{now.lexeme}) catch |err| {
                        std.debug.panic("{any}\n", .{err});
                    };
                    errorutil.reportErrorFatal(now, &errorMsg, "yeslai hatayera ';' rakhnus");
                }
                return null;
            }
            self.current += 1; // skip ';' token
            return ast.Stmt{
                .DekhauStmt = .{
                    .expr = exp,
                },
            };
        } else {
            std.debug.print("error: expected expression but found '{any}'\n", .{self.peek().lexeme});
            return null;
        }
    }

    fn parsePrimary(self: *Self) ?ast.Expr {
        var now: scanner.Token = self.peek();
        self.current += 1; // skip past primary token
        if (now.token_type == scanner.TokenType.TOKEN_SAHI) {
            return Parser.createLiteralExpr(.{ .Boolean = true });
        } else if (now.token_type == scanner.TokenType.TOKEN_GALAT) {
            return Parser.createLiteralExpr(.{ .Boolean = true });
        } else if (now.token_type == scanner.TokenType.TOKEN_INT) {
            return Parser.createLiteralExpr(.{ .Integer = std.fmt.parseInt(i32, now.literal, 10) catch |err| {
                std.debug.panic("error parsing int: {any}\n", .{err});
            } });
        } else if (now.token_type == scanner.TokenType.TOKEN_STRING) {
            return Parser.createLiteralExpr(.{ .String = now.literal });
        } else if (now.token_type == scanner.TokenType.TOKEN_IDENTIFIER) {
            return ast.Expr{
                .VariableExpr = .{ .var_name = now.lexeme },
            };
        }
        self.current -= 1; // go back to whatever token was there before
        return null;
    }

    fn createLiteralExpr(lit_val: ast.LiteralValueType) ast.Expr {
        return ast.Expr{
            .LiteralExpr = .{
                .value = lit_val,
            },
        };
    }

    fn expect(self: *Self, token: scanner.Token, msg: []const u8) bool {
        var now: scanner.Token = self.peek();
        if (now.token_type == token.token_type) {
            return true;
        } else {
            errorutil.reportUnexpectedTokenError(token, msg);
        }
    }

    fn peek(self: *Self) scanner.Token {
        if (!self.isAtEnd()) {
            return self.tokens.items[self.current];
        }
        return scanner.Token{
            .token_type = scanner.TokenType.TOKEN_NONE,
            .column = 0,
            .line = 0,
            .literal = "",
            .lexeme = "",
        };
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.tokens_len;
    }
};
