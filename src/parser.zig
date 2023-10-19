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

pub const Parser = struct {
    source: []const u8,
    tokens: std.ArrayList(scanner.Token),
    current: usize,
    tokens_len: usize,
    const Self = @This();

    pub fn init(source: []const u8, tokens: std.ArrayList(scanner.Token)) Parser {
        return .{
            .source = source,
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
                self.current += 1;
                var dekhauStmt = self.parseDekhauStmt();
                return dekhauStmt;
            },
            else => {
                self.current += 1;
                return null;
            },
        }
    }

    fn parseDekhauStmt(self: *Self) ?ast.Stmt {
        var expr: ?ast.Expr = self.parsePrimary();
        if (expr) |exp| {
            return ast.Stmt{
                .DekhauStmt = .{
                    .expr = exp,
                },
            };
        } else {
            std.debug.print("error: expected expression but found '{any}'\n", .{self.peek().lexeme});
            return null;
        }
        self.current += 1;
        return null;
    }

    fn parsePrimary(self: *Self) ?ast.Expr {
        var now: scanner.Token = self.peek();
        if (now.token_type == scanner.TokenType.TOKEN_SAHI) {
            self.current += 1;
            return Parser.createLiteralExpr(.{ .Boolean = true });
        } else if (now.token_type == scanner.TokenType.TOKEN_GALAT) {
            self.current += 1;
            return Parser.createLiteralExpr(.{ .Boolean = true });
        } else if (now.token_type == scanner.TokenType.TOKEN_INT) {
            self.current += 1;
            return Parser.createLiteralExpr(.{ .Integer = std.fmt.parseInt(i32, now.literal, 10) catch |err| {
                std.debug.panic("error parsing int: {any}\n", .{err});
            } });
        } else if (now.token_type == scanner.TokenType.TOKEN_STRING) {
            self.current += 1;
            return Parser.createLiteralExpr(.{ .String = now.literal });
        }
        return null;
    }

    fn createLiteralExpr(lit_val: ast.LiteralValueType) ast.Expr {
        return ast.Expr{
            .LiteralExpr = .{
                .value = lit_val,
            },
        };
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
