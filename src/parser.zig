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

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator: std.heap.ArenaAllocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var exprs_allocated = std.ArrayList(*ast.Expr).init(allocator.allocator());
    const Self: type = @This();

    pub fn init(source: []const u8, tokens: std.ArrayList(scanner.Token)) Parser {
        errorutil.initErrorEngine(source);
        return .{
            .source_lines = std.mem.split(u8, source, "\n"),
            .tokens = tokens,
            .current = 0,
            .tokens_len = tokens.items.len,
        };
    }

    pub fn deinit() void {
        var expr_dealloc: std.mem.Allocator = allocator.allocator();
        for (exprs_allocated.items) |expr| {
            expr_dealloc.destroy(expr);
        }
        _ = allocator.deinit();

        switch (gpa.deinit()) {
            .leak => {
                std.debug.panic("There was memory leak in this program!!!", .{});
            },
            else => {},
        }
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
                var dekhau_stmt = self.parseDekhauStmt();
                return dekhau_stmt;
            },
            .TOKEN_RAKHA => {
                self.current += 1;
                var rakha_stmt = self.parseRakhaStmt();
                return rakha_stmt;
            },
            else => {
                self.current += 1;
                var expr_stmt = self.parseExprStmt();
                return expr_stmt;
            },
        }
    }

    fn parseExprStmt(self: *Self) ?ast.Stmt {
        var expr = self.parseExpr();
        if (expr) |_expr| {
            return ast.Stmt{ .ExprStmt = .{ .expr = _expr } };
        }
        return null;
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
        var prev: scanner.Token = now; // if in case we reached the end of program/line and didn't find semicolon
        if (now.token_type == scanner.TokenType.TOKEN_SEMICOLON or now.token_type == scanner.TokenType.TOKEN_EOF) {
            errorutil.reportErrorFatal(now, "yaha tapaile variable ko lagi value dina parne hunchha", null);
            return null;
        }

        var expr: ast.Expr = undefined;
        if (self.parseExpr()) |_expr| {
            expr = _expr;
        } else {
            return null;
        }

        now = self.peek();
        if (now.token_type != scanner.TokenType.TOKEN_SEMICOLON) {
            if (now.token_type != scanner.TokenType.TOKEN_EOF) {
                errorutil.reportUnexpectedTokenError(now, ";");
            } else {
                errorutil.reportErrorFatal(prev, "variable ma value rakhi sakepachhi ';' lekhnus", "yo pachhadi ';' lekhnus");
            }
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
        var expr: ?ast.Expr = self.parseExpr();
        if (expr) |exp| {
            // DEKHAU <expression>;
            // expecting ';' after expression
            var now: scanner.Token = self.peek();
            if (now.token_type != scanner.TokenType.TOKEN_SEMICOLON) {
                if (now.token_type == scanner.TokenType.TOKEN_EOF) {
                    errorutil.reportErrorFatal(now, "'expression' lekhi sakepachhi ';' lekhnu parne huncha tara program antim ma pugi sakechha", null);
                } else {
                    var arena: std.mem.Allocator = Parser.allocator.allocator();
                    var msg: []u8 = std.fmt.allocPrint(arena, "chaiyeko ';' tara vetiyo '{s}'", .{now.lexeme}) catch |_err| {
                        std.debug.panic("Error: {any}\n", .{_err});
                    };
                    errorutil.reportErrorFatal(now, msg, null);
                    arena.free(msg);
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
            errorutil.reportErrorFatal(self.peek(), "yaha 'dekhau' statement ko lagi 'expression' dinuhos", null);
            self.current += 1; // skip the erronous token
            return null;
        }
    }

    fn parseExpr(self: *Self) ?ast.Expr {
        return self.parseAddition();
    }

    fn parseAddition(self: *Self) ?ast.Expr {
        if (self.parseFactor()) |left_side_expr| {
            var now: scanner.Token = self.peek();
            if (now.token_type == scanner.TokenType.TOKEN_PLUS or now.token_type == scanner.TokenType.TOKEN_MINUS) {
                const operator: []const u8 = now.lexeme;
                self.current += 1; // skip past '+' or '-'
                if (self.parseFactor()) |right_side_expr| {
                    return Parser.createBinaryExpr(left_side_expr, right_side_expr, operator);
                } else {
                    var arena: std.mem.Allocator = Parser.allocator.allocator();
                    var msg: []u8 = std.fmt.allocPrint(arena, "'{s}' yaha aasha gariyeko thiyiyena", .{now.lexeme}) catch |_err| {
                        std.debug.panic("Error: {any}\n", .{_err});
                    };
                    errorutil.reportErrorFatal(now, msg, "yeslai hataunu hos");
                    arena.free(msg);
                    return null;
                }
            }
            return left_side_expr;
        }
        // show unexpected token error here
        return null;
    }

    fn parseFactor(self: *Self) ?ast.Expr {
        if (self.parsePrimary()) |left_side_expr| {
            var now: scanner.Token = self.peek();
            if (now.token_type == scanner.TokenType.TOKEN_SLASH or now.token_type == scanner.TokenType.TOKEN_STAR) {
                const operator: []const u8 = now.lexeme;
                self.current += 1;
                if (self.parsePrimary()) |right_side_expr| {
                    return Parser.createBinaryExpr(left_side_expr, right_side_expr, operator);
                } else {
                    var arena: std.mem.Allocator = Parser.allocator.allocator();
                    var msg: []u8 = std.fmt.allocPrint(arena, "'{s}' yaha aasha gariyeko thiyiyena", .{now.lexeme}) catch |_err| {
                        std.debug.panic("Error: {any}\n", .{_err});
                    };
                    errorutil.reportErrorFatal(now, msg, "yeslai hataunu hos");
                    arena.free(msg);
                    return null;
                }
            }
            return left_side_expr;
        }
        // show unexpected token error here
        return null;
    }

    fn parsePrimary(self: *Self) ?ast.Expr {
        var now: scanner.Token = self.peek();
        self.current += 1; // skip past primary token
        if (now.token_type == scanner.TokenType.TOKEN_SAHI) {
            return Parser.createLiteralExpr(.{ .Boolean = true });
        } else if (now.token_type == scanner.TokenType.TOKEN_GALAT) {
            return Parser.createLiteralExpr(.{ .Boolean = false });
        } else if (now.token_type == scanner.TokenType.TOKEN_INT) {
            return Parser.createLiteralExpr(.{ .Integer = std.fmt.parseInt(i32, now.literal, 10) catch |err| {
                std.debug.panic("error parsing int: {any}\n", .{err});
            } });
        } else if (now.token_type == scanner.TokenType.TOKEN_STRING) {
            return Parser.createLiteralExpr(.{ .String = now.literal });
        } else if (now.token_type == scanner.TokenType.TOKEN_IDENTIFIER) {
            return .{ .VariableExpr = .{ .var_name = now.lexeme } };
        } else {
            self.current -= 1; // go back to whatever token was there before
            return null;
        }
    }

    fn createBinaryExpr(expr1: ast.Expr, expr2: ast.Expr, operator: []const u8) ast.Expr {
        var tmp_alloc: std.mem.Allocator = Parser.allocator.allocator();
        var leftt = tmp_alloc.create(ast.Expr) catch |errr| {
            std.debug.panic("Error: {any}\n", .{errr});
        };
        leftt.* = expr1;
        Parser.exprs_allocated.append(leftt) catch |app_err| {
            std.debug.panic("Error: {any}\n", .{app_err});
        };

        var rightt = tmp_alloc.create(ast.Expr) catch |errr| {
            std.debug.panic("Error: {any}\n", .{errr});
        };
        rightt.* = expr2;
        Parser.exprs_allocated.append(rightt) catch |app_err| {
            std.debug.panic("Error: {any}\n", .{app_err});
        };
        return ast.Expr{
            .BinaryExpr = .{
                .left = leftt,
                .operator = operator,
                .right = rightt,
            },
        };
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

    fn previous(self: *Self) ?scanner.Token {
        if (self.current > 0) {
            return self.tokens.items[self.current - 1];
        }
        return null;
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
