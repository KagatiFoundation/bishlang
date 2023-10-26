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

pub var has_error: bool = false;

// errors which may show up during parsing tokens
const ParseError = error{
    ParenNotClosed,
    UnexpectedToken,
};

// result will be one of Error(ErrorTokenType, ErrorToken) or Success(Expr) after parsing statements
const ParseResult = union(enum) {
    Success: ast.Expr,
    Error: struct {
        err_token: scanner.Token,
        err_type: ParseError,
    },
};

pub const Parser = struct {
    source_lines: std.mem.SplitIterator(u8, .sequence),
    tokens: std.ArrayList(scanner.Token),
    current: usize,
    tokens_len: usize,

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator: std.heap.ArenaAllocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var exprs_allocated = std.ArrayList(*ast.Expr).init(allocator.allocator());
    var stmts_allocated = std.ArrayList(*ast.Stmt).init(allocator.allocator());
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
        var _dealloc: std.mem.Allocator = allocator.allocator();
        for (exprs_allocated.items) |expr| {
            _dealloc.destroy(expr);
        }

        for (stmts_allocated.items) |stmt| {
            _dealloc.destroy(stmt);
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
            if (self.parseStmt()) |stmt| {
                _ = try stmts.append(stmt);
            }
        }
        return stmts;
    }

    fn parseStmt(self: *Self) ?ast.Stmt {
        var now: scanner.Token = self.peek();
        switch (now.token_type) {
            .TOKEN_DEKHAU => {
                self.current += 1; // step 'dekhau'
                return self.parseDekhauStmt();
            },
            .TOKEN_RAKHA => {
                self.current += 1; // skip 'rakha'
                return self.parseRakhaStmt();
            },
            .KW_YADI => {
                self.current += 1; // skip 'yadi'
                return self.parseYadiStmt();
            },
            .KW_SURU => {
                self.current += 1; // skip 'suru'
                return self.parseBlockStmt();
            },
            .TOKEN_EOF => {
                self.current += 1;
                return null;
            },
            else => return self.parseExprStmt(),
        }
    }

    fn parseExprStmt(self: *Self) ?ast.Stmt {
        var possible_expr_tokens: [6]scanner.TokenType = [6]scanner.TokenType{
            scanner.TokenType.TOKEN_IDENTIFIER,
            scanner.TokenType.TOKEN_INT,
            scanner.TokenType.TOKEN_STRING,
            scanner.TokenType.TOKEN_SAHI,
            scanner.TokenType.TOKEN_GALAT,
            scanner.TokenType.TOKEN_FLOAT,
        };
        var ok: bool = false;
        var now: scanner.Token = self.peek();
        for (possible_expr_tokens) |tt| {
            if (now.token_type == tt) {
                ok = true; // token is parsable
                break;
            }
        }

        if (ok) {
            if (self.parseExpr()) |expr| {
                if (!self.expectToken(scanner.TokenType.TOKEN_SEMICOLON, ";")) {
                    self.hopToNextStmt();
                    return null;
                }
                self.current += 1; // skip ';' or 'EOF' token
                return ast.Stmt{ .ExprStmt = .{ .expr = expr } };
            }
        } else {
            var arena: std.mem.Allocator = Parser.allocator.allocator();
            var msg: []u8 = std.fmt.allocPrint(arena, "'{s}' yaha aasha gariyeko thiyiyena", .{now.lexeme}) catch |_err| {
                std.debug.panic("Error: {any}\n", .{_err});
            };
            errorutil.reportErrorFatal(now, msg, null);
            arena.free(msg);
        }
        self.hopToNextStmt();
        return null;
    }

    fn parseYadiStmt(self: *Self) ?ast.Stmt {
        var tmp_alloc: std.mem.Allocator = Parser.allocator.allocator();
        if (self.parseExpr()) |expr| {
            var yadi_sahi: *ast.Stmt = undefined;
            if (self.parseStmt()) |ys| {
                yadi_sahi = tmp_alloc.create(ast.Stmt) catch |errr| {
                    std.debug.panic("Error: {any}\n", .{errr});
                };
                yadi_sahi.* = ys;
                Parser.stmts_allocated.append(yadi_sahi) catch |app_err| {
                    std.debug.panic("Error: {any}\n", .{app_err});
                };
            } else return null;

            var yadi_galat: *?ast.Stmt = undefined;
            var now: scanner.Token = self.peek();
            if (now.token_type == scanner.TokenType.KW_NATRA) {
                self.current += 1;
                if (self.parseStmt()) |yd| {
                    yadi_galat = tmp_alloc.create(?ast.Stmt) catch |errr| {
                        std.debug.panic("Error: {any}\n", .{errr});
                    };
                    yadi_galat.* = yd;
                    Parser.stmts_allocated.append(&yadi_galat.*.?) catch |app_err| {
                        std.debug.panic("Error: {any}\n", .{app_err});
                    };
                } else return null;
            }
            return ast.Stmt{
                .YadiNatraStmt = .{
                    .condition = expr,
                    .yadi_sahi = yadi_sahi,
                    .yadi_galat = yadi_galat,
                },
            };
        } else {
            self.hopToNextStmt();
            return null;
        }
    }

    fn parseBlockStmt(self: *Self) ?ast.Stmt {
        var tmp_alloc: std.mem.Allocator = Parser.allocator.allocator();
        var block_start_token: scanner.Token = self.tokens.items[self.current - 1]; // 'suru' token
        var stmts: std.ArrayList(*ast.Stmt) = std.ArrayList(*ast.Stmt).init(Parser.allocator.allocator());
        while (true) {
            var now: scanner.Token = self.peek();
            if (now.token_type == scanner.TokenType.KW_ANTYA or now.token_type == scanner.TokenType.TOKEN_EOF) {
                break;
            }
            if (self.parseStmt()) |stmt| {
                var tmp_stmt: *ast.Stmt = tmp_alloc.create(ast.Stmt) catch |err| {
                    std.debug.panic("Error: {any}\n", .{err});
                };
                tmp_stmt.* = stmt;
                Parser.stmts_allocated.append(tmp_stmt) catch |err| {
                    std.debug.panic("Error: {any}\n", .{err});
                };
                stmts.append(tmp_stmt) catch |err| {
                    std.debug.panic("Error: {any}\n", .{err});
                };
            } else {
                stmts.deinit();
                self.hopToNextStmt();
                break;
            }
        }

        if (self.peek().token_type != scanner.TokenType.KW_ANTYA) {
            has_error = true;
            stmts.deinit();
            errorutil.reportErrorFatal(block_start_token, "'suru' garisakepachhi 'antya' pani garnus", null);
            return null;
        }
        self.current += 1; // skip 'antya'
        return ast.Stmt{ .BlockStmt = .{ .stmts = stmts } };
    }

    fn parseRakhaStmt(self: *Self) ?ast.Stmt {
        var var_name: scanner.Token = self.peek();
        if (var_name.token_type != scanner.TokenType.TOKEN_IDENTIFIER) {
            has_error = true;
            self.hopToNextStmt();
            errorutil.reportErrorFatal(var_name, "'rakha' pacchi variable ko naam dinus", "yaha variable ko naam huna parchha");
            return null;
        }
        self.current += 1; // skip past identifier name

        var now: scanner.Token = self.peek(); // 'ma' keyword
        if (now.token_type != scanner.TokenType.TOKEN_MA) {
            has_error = true;
            self.hopToNextStmt();
            errorutil.reportErrorFatal(now, "variable ko naam pachhi 'ma' lekhnus", "variable ma value store garna 'ma' keyword lekhnu parne hunchha");
            return null;
        }
        self.current += 1; // skip past 'ma' keyword

        if (self.parseExpr()) |expr| {
            if (!self.expectToken(scanner.TokenType.TOKEN_SEMICOLON, ";")) {
                self.hopToNextStmt();
                return null;
            }
            self.current += 1; // skip ';' or 'EOF' token
            return ast.Stmt{
                .RakhaStmt = .{
                    .var_name = var_name.lexeme,
                    .expr = expr,
                },
            };
        } else {
            self.hopToNextStmt();
            return null;
        }
    }

    fn parseDekhauStmt(self: *Self) ?ast.Stmt {
        if (self.parseExpr()) |exp| {
            // DEKHAU <expression>;
            // expecting ';' after expression
            if (!self.expectToken(scanner.TokenType.TOKEN_SEMICOLON, ";")) {
                self.hopToNextStmt();
                return null;
            }
            self.current += 1; // skip ';' or 'EOF' token
            return ast.Stmt{
                .DekhauStmt = .{
                    .expr = exp,
                },
            };
        } else {
            self.hopToNextStmt();
            return null;
        }
    }

    fn parseExpr(self: *Self) ?ast.Expr {
        switch (self.parseAddition()) {
            .Success => |expr| return expr,
            .Error => |err| {
                has_error = true;
                switch (err.err_type) {
                    ParseError.ParenNotClosed => {
                        errorutil.reportErrorFatal(err.err_token, "'(' suru garisakepacchi banda pani garnuhos", null);
                    },
                    ParseError.UnexpectedToken => {
                        var arena: std.mem.Allocator = Parser.allocator.allocator();
                        var msg: []u8 = std.fmt.allocPrint(arena, "'{s}' yaha aasha gariyeko thiyiyena", .{err.err_token.lexeme}) catch |_err| {
                            std.debug.panic("Error: {any}\n", .{_err});
                        };
                        errorutil.reportErrorFatal(err.err_token, msg, "yeslai hataunu hos");
                        arena.free(msg);
                    },
                }
                return null;
            },
        }
    }

    fn parseAddition(self: *Self) ParseResult {
        var left_expr_res: ParseResult = self.parseFactor();
        switch (left_expr_res) {
            .Success => |left_side_expr| {
                var now: scanner.Token = self.peek();
                if (now.token_type == scanner.TokenType.TOKEN_PLUS or now.token_type == scanner.TokenType.TOKEN_MINUS) {
                    const operator: []const u8 = now.lexeme;
                    self.current += 1; // skip '+' or '-' token
                    var right_expr_res = self.parseAddition();
                    return switch (right_expr_res) {
                        .Success => |right_side_expr| ParseResult{
                            .Success = Parser.createBinaryExpr(left_side_expr, right_side_expr, operator),
                        },
                        .Error => |_| right_expr_res,
                    };
                }
                return ParseResult{ .Success = left_side_expr };
            },
            else => return left_expr_res,
        }
        return Parser.unexpectedTokenErr(self.peek());
    }

    fn parseFactor(self: *Self) ParseResult {
        var left_expr_res: ParseResult = self.parsePrimary();
        switch (left_expr_res) {
            .Success => |left_side_expr| {
                self.current += 1;
                var now: scanner.Token = self.peek();
                if (now.token_type == scanner.TokenType.TOKEN_SLASH or now.token_type == scanner.TokenType.TOKEN_STAR) {
                    const operator: []const u8 = now.lexeme;
                    self.current += 1; // skip '*' or '/' token
                    var right_expr_res = self.parseAddition();
                    return switch (right_expr_res) {
                        .Success => |right_side_expr| ParseResult{
                            .Success = Parser.createBinaryExpr(left_side_expr, right_side_expr, operator),
                        },
                        .Error => |_| right_expr_res,
                    };
                }
                return ParseResult{ .Success = left_side_expr };
            },
            else => return left_expr_res,
        }
        return Parser.unexpectedTokenErr(self.peek());
    }

    fn parsePrimary(self: *Self) ParseResult {
        var now: scanner.Token = self.peek();
        if (now.token_type == scanner.TokenType.TOKEN_SAHI) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Boolean = true }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_GALAT) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Boolean = false }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_INT) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Integer = std.fmt.parseInt(i32, now.literal, 10) catch |err| {
                std.debug.panic("error parsing int: {any}\n", .{err});
            } }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_STRING) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .String = now.literal }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_IDENTIFIER) {
            return ParseResult{ .Success = .{ .VariableExpr = .{ .var_name = now.lexeme } } };
        } else if (now.token_type == scanner.TokenType.TOKEN_LEFT_PAREN) {
            var left_paren: scanner.Token = now;
            self.current += 1; // skip '('
            var group_expr_res = self.parseAddition();
            switch (group_expr_res) {
                .Success => |group_expr| {
                    var right_paren: scanner.Token = self.peek();
                    if (right_paren.token_type != scanner.TokenType.TOKEN_RIGHT_PAREN) {
                        return ParseResult{
                            .Error = .{
                                .err_token = left_paren,
                                .err_type = ParseError.ParenNotClosed,
                            },
                        };
                    }
                    return ParseResult{ .Success = Parser.createGroupExpr(group_expr) };
                },
                .Error => |_| return Parser.unexpectedTokenErr(self.peek()),
            }
        } else {
            return Parser.unexpectedTokenErr(self.peek());
        }
    }

    fn createGroupExpr(expr: ast.Expr) ast.Expr {
        var tmp_alloc: std.mem.Allocator = Parser.allocator.allocator();
        var tmp_expr = tmp_alloc.create(ast.Expr) catch |err| {
            std.debug.panic("Error: {any}\n", .{err});
        };
        tmp_expr.* = expr;
        Parser.exprs_allocated.append(tmp_expr) catch |err| {
            std.debug.panic("Error: {any}\n", .{err});
        };
        return ast.Expr{ .GroupExpr = .{ .expr = tmp_expr } };
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

    // skip to the token after next ';'
    fn hopToNextStmt(self: *Self) void {
        while (true) {
            var now: scanner.Token = self.peek();
            if (now.token_type != scanner.TokenType.TOKEN_NONE) {
                var found: bool = now.token_type == scanner.TokenType.TOKEN_SEMICOLON;
                if (found) {
                    self.current += 1; // skip the ';'
                    break;
                }
                self.current += 1;
            } else {
                break;
            }
        }
    }

    // creates an unexpected token error instance at the current token
    fn unexpectedTokenErr(token: scanner.Token) ParseResult {
        return ParseResult{
            .Error = .{
                .err_token = token,
                .err_type = ParseError.UnexpectedToken,
            },
        };
    }

    fn expectToken(self: *Self, expected_type: scanner.TokenType, token_lexeme: []const u8) bool {
        var now: scanner.Token = self.peek();
        if (now.token_type != expected_type) {
            if (now.token_type != scanner.TokenType.TOKEN_EOF) {
                var arena: std.mem.Allocator = Parser.allocator.allocator();
                var msg: []u8 = std.fmt.allocPrint(arena, "chaiyeko '{s}' tara vetiyo '{s}'", .{ token_lexeme, now.lexeme }) catch |_err| {
                    std.debug.panic("Error: {any}\n", .{_err});
                };
                var hint: []u8 = std.fmt.allocPrint(arena, "yaha '{s}' lekhnus", .{token_lexeme}) catch |_err| {
                    std.debug.panic("Error: {any}\n", .{_err});
                };
                errorutil.reportErrorFatal(now, msg, hint);
                arena.free(msg);
                arena.free(hint);
                has_error = true;
                return false;
            }
        }
        return true;
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
