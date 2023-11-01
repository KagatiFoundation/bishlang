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
const bu = @import("./utils/bishutil.zig");

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
    has_error: bool,
    allocator: std.mem.Allocator,
    exprs_allocated: std.ArrayList(*ast.Expr),
    stmts_allocated: std.ArrayList(*ast.Stmt),

    const Self: type = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8, tokens: std.ArrayList(scanner.Token)) Parser {
        errorutil.initErrorEngine(source);
        return .{
            .source_lines = std.mem.split(u8, source, "\n"),
            .tokens = tokens,
            .current = 0,
            .tokens_len = tokens.items.len,
            .has_error = false,
            .allocator = allocator,
            .exprs_allocated = std.ArrayList(*ast.Expr).init(allocator),
            .stmts_allocated = std.ArrayList(*ast.Stmt).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.exprs_allocated.items) |expr| {
            self.allocator.destroy(expr);
        }
        for (self.stmts_allocated.items) |stmt| {
            self.allocator.destroy(stmt);
        }
        self.exprs_allocated.deinit();
        self.stmts_allocated.deinit();
    }

    pub fn parse(self: *Self) !std.ArrayList(ast.Stmt) {
        var stmts: std.ArrayList(ast.Stmt) = std.ArrayList(ast.Stmt).init(self.allocator);
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
            .TOKEN_GHUMAU => {
                self.skip(); // skip 'ghumau'
                return self.parseGhumauStmt();
            },
            .KW_KARYA => {
                self.skip(); // skip 'karya'
                return self.parseKaryaDeclStmt();
            },
            .KW_FARKAU => {
                self.skip(); // skip 'farkau'
                return self.parseFarkauStmt();
            },
            .TOKEN_EOF => {
                self.current += 1;
                return null;
            },
            else => return self.parseExprStmt(),
        }
    }

    fn parseExprStmt(self: *Self) ?ast.Stmt {
        var possible_expr_tokens: [7]scanner.TokenType = [7]scanner.TokenType{
            scanner.TokenType.TOKEN_IDENTIFIER,
            scanner.TokenType.TOKEN_INT,
            scanner.TokenType.TOKEN_STRING,
            scanner.TokenType.TOKEN_SAHI,
            scanner.TokenType.TOKEN_GALAT,
            scanner.TokenType.TOKEN_FLOAT,
            scanner.TokenType.TOKEN_LEFT_PAREN,
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
            var msg: []u8 = std.fmt.allocPrint(self.allocator, "'{s}' yaha aasha gariyeko thiyiyena", .{now.lexeme}) catch |_err| {
                std.debug.panic("Error: {any}\n", .{_err});
            };
            errorutil.reportErrorFatal(now, msg, null);
            self.allocator.free(msg);
        }
        self.has_error = true;
        self.hopToNextStmt();
        return null;
    }

    fn parseFarkauStmt(self: *Self) ?ast.Stmt {
        var now: scanner.Token = self.peek();
        if (now.token_type != scanner.TokenType.TOKEN_SEMICOLON) {
            if (self.parseExpr()) |expr| {
                if (!self.expectToken(scanner.TokenType.TOKEN_SEMICOLON, ";")) {
                    self.has_error = true;
                    self.hopToNextStmt();
                    return null;
                }
                self.skip(); // skip ';'
                return ast.Stmt{ .FarkauStmt = .{ .expr = expr } };
            }
            self.hopToNextStmt();
            return null;
        } else {
            self.skip(); // skip ';'
            return ast.Stmt{ .FarkauStmt = .{ .expr = null } };
        }
    }

    fn parseKaryaDeclStmt(self: *Self) ?ast.Stmt {
        var name_token: scanner.Token = self.peek();
        if (name_token.token_type == scanner.TokenType.TOKEN_IDENTIFIER) {
            self.skip(); // skip karya name
            if (!self.expectToken(scanner.TokenType.TOKEN_LEFT_PAREN, "(")) {
                self.has_error = true;
                self.hopToNextStmt();
                return null;
            }
            self.skip(); // skip '('
            // parsing parameter names
            var now: scanner.Token = self.peek();
            var param_names = std.ArrayList([]const u8).init(self.allocator);
            if (now.token_type != scanner.TokenType.TOKEN_RIGHT_PAREN) {
                while (true) {
                    if (now.token_type != scanner.TokenType.TOKEN_IDENTIFIER) {
                        // This situation will arise in the first run of this loop.
                        // This situation can arise in or from the second iteration of this infinite while loop.
                        // If this situation arises, we just break out of it.
                        if (now.token_type == scanner.TokenType.TOKEN_RIGHT_PAREN) break;
                        errorutil.reportErrorFatal(now, "yaha parameter ko naam sachyaunus", "yo parameter ko naam galat ho");
                        self.hopToNextStmt();
                        return null;
                    }
                    var new_var_name: []const u8 = now.lexeme;
                    if (bu.stringArrListContains(new_var_name, param_names)) {
                        errorutil.reportErrorFatal(now, "ustai naam ko parameter feri vetiyo", "Yo naam ko parameter pahile banisakeko chha");
                        self.hopToNextStmt();
                        return null;
                    }
                    param_names.append(now.lexeme) catch |app_err| {
                        std.debug.panic("Error adding parameter name to the ArrayList: {any}\n", .{app_err});
                    }; // add 'parameter' name to function parameter list
                    self.skip(); // skip parameter name
                    now = self.peek();
                    if (now.token_type == scanner.TokenType.TOKEN_RIGHT_PAREN) {
                        break;
                    } else if (now.token_type == scanner.TokenType.TOKEN_COMMA) {
                        self.skip(); // skip ','
                        now = self.peek(); // point to next parameter name
                        continue;
                    } else {
                        errorutil.reportErrorFatal(now, "TODO ERROR MESSAGE!!!", "yaha ',' ya ')' aaunu parne hunchha");
                        self.hopToNextStmt();
                        return null;
                    }
                }
            }

            if (!self.expectToken(scanner.TokenType.TOKEN_RIGHT_PAREN, ")")) {
                self.has_error = true;
                self.hopToNextStmt();
                return null;
            }
            self.skip(); // skip ')'
            if (self.parseStmt()) |stmt| {
                var karya_body_stmt: *ast.Stmt = self.allocator.create(ast.Stmt) catch |err| {
                    std.debug.panic("Error: {any}\n", .{err});
                };
                karya_body_stmt.* = stmt;
                self.stmts_allocated.append(karya_body_stmt) catch |app_err| {
                    std.debug.panic("Error: {any}\n", .{app_err});
                };
                return ast.Stmt{
                    .KaryaDeclStmt = .{
                        .name = name_token.lexeme,
                        .stmt = karya_body_stmt,
                        .params = param_names,
                    },
                };
            } else return null;
        }
        self.hopToNextStmt();
        return null;
    }

    // <ghumau-statement> ::= GHUMAU <expression> PATAK [|<variable>|]  { <statement-list> }
    fn parseGhumauStmt(self: *Self) ?ast.Stmt {
        if (self.parseExpr()) |expr| {
            if (!self.expectToken(scanner.TokenType.KW_PATAK, "patak")) {
                self.has_error = true;
                self.hopToNextStmt();
                return null;
            }
            self.skip(); // skip 'patak'
            var now: scanner.Token = self.peek();
            var var_expr_id: []const u8 = "_";
            if (now.token_type == scanner.TokenType.TOKEN_PIPE) {
                self.skip(); // skip starting '|'
                var expr_tok: scanner.Token = self.peek();
                if (self.parseExpr()) |ghumau_var_expr| {
                    if (!self.expectToken(scanner.TokenType.TOKEN_PIPE, "|")) {
                        self.hopToNextStmt();
                        return null;
                    }
                    self.skip(); // skip ending '|'
                    switch (ghumau_var_expr) {
                        .VariableExpr => |var_expr| {
                            var_expr_id = var_expr.var_name;
                        },
                        else => {
                            self.has_error = true;
                            errorutil.reportErrorFatal(expr_tok, "yaha variable ko naam lekhnus", "yeslai hataunu hos");
                            return null;
                        },
                    }
                }
            }
            if (self.parseStmt()) |stmt| {
                var ghumau_body_stmt: *ast.Stmt = self.allocator.create(ast.Stmt) catch |err| {
                    std.debug.panic("Error: {any}\n", .{err});
                };
                ghumau_body_stmt.* = stmt;
                self.stmts_allocated.append(ghumau_body_stmt) catch |app_err| {
                    std.debug.panic("Error: {any}\n", .{app_err});
                };
                return ast.Stmt{
                    .GhumauStmt = .{
                        .expr = expr,
                        .identifier = var_expr_id,
                        .stmt = ghumau_body_stmt,
                    },
                };
            } else return null;
        } else {
            self.hopToNextStmt();
            return null;
        }
    }

    fn parseYadiStmt(self: *Self) ?ast.Stmt {
        if (self.parseExpr()) |expr| {
            var yadi_sahi: *ast.Stmt = undefined;
            if (self.parseStmt()) |ys| {
                yadi_sahi = self.allocator.create(ast.Stmt) catch |errr| {
                    std.debug.panic("Error: {any}\n", .{errr});
                };
                yadi_sahi.* = ys;
                self.stmts_allocated.append(yadi_sahi) catch |app_err| {
                    std.debug.panic("Error: {any}\n", .{app_err});
                };
            } else return null;

            var yadi_galat: *?ast.Stmt = undefined;
            var now: scanner.Token = self.peek();
            if (now.token_type == scanner.TokenType.KW_NATRA) {
                self.current += 1;
                if (self.parseStmt()) |yd| {
                    yadi_galat = self.allocator.create(?ast.Stmt) catch |errr| {
                        std.debug.panic("Error: {any}\n", .{errr});
                    };
                    yadi_galat.* = yd;
                    self.stmts_allocated.append(&yadi_galat.*.?) catch |app_err| {
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
        var block_start_token: scanner.Token = self.tokens.items[self.current - 1]; // 'suru' token
        var stmts: std.ArrayList(*ast.Stmt) = std.ArrayList(*ast.Stmt).init(self.allocator);
        while (true) {
            var now: scanner.Token = self.peek();
            if (now.token_type == scanner.TokenType.KW_ANTYA or now.token_type == scanner.TokenType.TOKEN_EOF) {
                break;
            }
            if (self.parseStmt()) |stmt| {
                var tmp_stmt: *ast.Stmt = self.allocator.create(ast.Stmt) catch |err| {
                    std.debug.panic("Error: {any}\n", .{err});
                };
                tmp_stmt.* = stmt;
                self.stmts_allocated.append(tmp_stmt) catch |err| {
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
            self.has_error = true;
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
            self.has_error = true;
            self.hopToNextStmt();
            errorutil.reportErrorFatal(var_name, "'rakha' pacchi variable ko naam dinus", "yaha variable ko naam huna parchha");
            return null;
        }
        self.current += 1; // skip past identifier name

        var now: scanner.Token = self.peek(); // 'ma' keyword
        if (now.token_type != scanner.TokenType.TOKEN_MA) {
            self.has_error = true;
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
        switch (self.parseLogicalOr()) {
            .Success => |expr| {
                var now: scanner.Token = self.peek();
                if (now.token_type == scanner.TokenType.KW_CHHA or now.token_type == scanner.TokenType.TOKEN_CHHAINA) {
                    self.skip(); // skip 'chha' or 'chhaina'
                    var tmp_expr: *ast.Expr = self.allocator.create(ast.Expr) catch |err| {
                        std.debug.panic("Error: {any}\n", .{err});
                    };
                    tmp_expr.* = expr;
                    self.exprs_allocated.append(tmp_expr) catch |app_err| {
                        std.debug.panic("Error: {any}\n", .{app_err});
                    };
                    return ast.Expr{ .UnaryExpr = .{
                        .expr = tmp_expr,
                        .operator = now.lexeme,
                    } };
                }
                return expr;
            },
            .Error => |err| {
                self.has_error = true;
                switch (err.err_type) {
                    ParseError.ParenNotClosed => {
                        errorutil.reportErrorFatal(err.err_token, "'(' suru garisakepacchi banda pani garnuhos", null);
                    },
                    ParseError.UnexpectedToken => {
                        var msg: []u8 = std.fmt.allocPrint(self.allocator, "'{s}' yaha aasha gariyeko thiyiyena", .{err.err_token.lexeme}) catch |_err| {
                            std.debug.panic("Error: {any}\n", .{_err});
                        };
                        errorutil.reportErrorFatal(err.err_token, msg, "yeslai hataunu hos");
                        self.allocator.free(msg);
                    },
                }
                return null;
            },
        }
    }

    inline fn parseLogicalOr(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parseLogicalAnd(),
            &[1]scanner.TokenType{scanner.TokenType.KW_YA},
        );
    }

    inline fn parseLogicalAnd(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parseEquality(),
            &[1]scanner.TokenType{scanner.TokenType.KW_RA},
        );
    }

    inline fn parseEquality(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parseComparision(),
            &[1]scanner.TokenType{scanner.TokenType.TOKEN_BARABAR},
        );
    }

    inline fn parseComparision(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parseAddition(),
            &[2]scanner.TokenType{ scanner.TokenType.TOKEN_SANO, scanner.TokenType.TOKEN_THULO },
        );
    }

    inline fn parseAddition(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parseFactor(),
            &[2]scanner.TokenType{ scanner.TokenType.TOKEN_PLUS, scanner.TokenType.TOKEN_MINUS },
        );
    }

    inline fn parseFactor(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parsePower(),
            &[3]scanner.TokenType{ scanner.TokenType.TOKEN_STAR, scanner.TokenType.TOKEN_SLASH, scanner.TokenType.TOKEN_PERCENTAGE },
        );
    }

    inline fn parsePower(self: *Self) ParseResult {
        return self.tryParsingBinaryExpr(
            self.parseCallExpr(),
            &[1]scanner.TokenType{scanner.TokenType.TOKEN_POWER},
        );
    }

    fn tryParsingBinaryExpr(self: *Self, left_side_res: ParseResult, tokens: []const scanner.TokenType) ParseResult {
        switch (left_side_res) {
            .Success => |left_side_expr| {
                var operator: []const u8 = undefined;
                var now: scanner.Token = self.peek();
                var ok: bool = false;
                for (tokens) |token| {
                    if (token == now.token_type) {
                        operator = now.lexeme;
                        ok = true;
                        break;
                    }
                }
                if (ok) {
                    self.current += 1; // skip the binary operator
                    var right_side_res: ParseResult = self.parseLogicalOr();
                    return switch (right_side_res) {
                        .Success => |right_side_expr| ParseResult{
                            .Success = self.createBinaryExpr(
                                left_side_expr,
                                right_side_expr,
                                operator,
                            ),
                        },
                        .Error => |_| right_side_res,
                    };
                } else {
                    return ParseResult{ .Success = left_side_expr };
                }
            },
            else => return left_side_res,
        }
    }

    fn parseCallExpr(self: *Self) ParseResult {
        var possible_call_expr_token: scanner.Token = self.peek();
        var prim_parse_res: ParseResult = self.parsePrimary();
        switch (prim_parse_res) {
            .Success => |prim_expr| {
                if (self.peek().token_type == scanner.TokenType.TOKEN_LEFT_PAREN) {
                    self.skip(); // skip '('
                    var now: scanner.Token = self.peek();
                    var exprs = std.ArrayList(*ast.Expr).init(self.allocator);
                    if (now.token_type != scanner.TokenType.TOKEN_RIGHT_PAREN) {
                        while (true) {
                            if (self.parseExpr()) |expr| {
                                var tmp_expr: *ast.Expr = self.allocator.create(ast.Expr) catch |cre_err| {
                                    std.debug.panic("Can't create a new expression: {any}\n", .{cre_err});
                                };
                                tmp_expr.* = expr;
                                exprs.append(tmp_expr) catch |app_err| {
                                    std.debug.panic("Can't append a new expression into function call expression list: {any}\n", .{app_err});
                                };
                            }
                            now = self.peek();
                            if (now.token_type == scanner.TokenType.TOKEN_RIGHT_PAREN) {
                                break;
                            } else if (now.token_type == scanner.TokenType.TOKEN_COMMA) {
                                self.skip(); // skip ','
                                now = self.peek(); // point to next argument expression
                                continue;
                            } else {
                                errorutil.reportErrorFatal(now, "chaiyeko expression tara vetiyo ''", "yaha ',' ya ')' aaunu parne hunchha");
                                self.hopToNextStmt();
                                return Parser.unexpectedTokenErr(now);
                            }
                        }
                    }
                    self.skip(); // skip ')'
                    switch (prim_expr) {
                        .VariableExpr => |var_expr| {
                            return ParseResult{ .Success = .{
                                .CallExpr = .{
                                    .name = var_expr.var_name,
                                    .exprs = exprs,
                                },
                            } };
                        },
                        else => {
                            self.hopToNextStmt();
                            return Parser.unexpectedTokenErr(possible_call_expr_token);
                        },
                    }
                }
            },
            else => {},
        }
        return prim_parse_res;
    }

    fn parsePrimary(self: *Self) ParseResult {
        var now: scanner.Token = self.peek();
        self.current += 1;
        if (now.token_type == scanner.TokenType.TOKEN_SAHI) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Boolean = true }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_GALAT) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Boolean = false }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_INT) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Integer = std.fmt.parseInt(i32, now.literal, 10) catch |err| {
                std.debug.panic("error parsing int: {any}\n", .{err});
            } }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_FLOAT) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .Float = std.fmt.parseFloat(f32, now.literal) catch |err| {
                std.debug.panic("error parsing float: {any}\n", .{err});
            } }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_STRING) {
            return ParseResult{ .Success = Parser.createLiteralExpr(.{ .String = now.literal }) };
        } else if (now.token_type == scanner.TokenType.TOKEN_IDENTIFIER) {
            return ParseResult{ .Success = .{ .VariableExpr = .{ .var_name = now.lexeme } } };
        } else if (now.token_type == scanner.TokenType.TOKEN_LEFT_PAREN) {
            var left_paren: scanner.Token = now;
            // self.skip(); // skip '('
            var group_expr_res = self.parseLogicalOr();
            switch (group_expr_res) {
                .Success => |group_expr| {
                    if (!self.expectToken(scanner.TokenType.TOKEN_RIGHT_PAREN, ")")) {
                        return ParseResult{
                            .Error = .{
                                .err_token = left_paren,
                                .err_type = ParseError.ParenNotClosed,
                            },
                        };
                    }
                    self.skip(); // skip ')'
                    return ParseResult{ .Success = self.createGroupExpr(group_expr) };
                },
                .Error => |_| return Parser.unexpectedTokenErr(self.peek()),
            }
        } else {
            self.current -= 1;
            return Parser.unexpectedTokenErr(now);
        }
    }

    fn createGroupExpr(self: *Self, expr: ast.Expr) ast.Expr {
        var tmp_expr = self.allocator.create(ast.Expr) catch |err| {
            std.debug.panic("Error: {any}\n", .{err});
        };
        tmp_expr.* = expr;
        self.exprs_allocated.append(tmp_expr) catch |err| {
            std.debug.panic("Error: {any}\n", .{err});
        };
        return ast.Expr{ .GroupExpr = .{ .expr = tmp_expr } };
    }

    fn createBinaryExpr(self: *Self, expr1: ast.Expr, expr2: ast.Expr, operator: []const u8) ast.Expr {
        var leftt = self.allocator.create(ast.Expr) catch |errr| {
            std.debug.panic("Error: {any}\n", .{errr});
        };
        leftt.* = expr1;
        self.exprs_allocated.append(leftt) catch |app_err| {
            std.debug.panic("Error: {any}\n", .{app_err});
        };

        var rightt = self.allocator.create(ast.Expr) catch |errr| {
            std.debug.panic("Error: {any}\n", .{errr});
        };
        rightt.* = expr2;
        self.exprs_allocated.append(rightt) catch |app_err| {
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

    inline fn createLiteralExpr(lit_val: ast.LiteralValueType) ast.Expr {
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
                var found: bool = now.token_type == scanner.TokenType.TOKEN_SEMICOLON or now.token_type == scanner.TokenType.KW_ANTYA;
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
    inline fn unexpectedTokenErr(token: scanner.Token) ParseResult {
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
                var msg: []u8 = std.fmt.allocPrint(self.allocator, "chaiyeko '{s}' tara vetiyo '{s}'", .{ token_lexeme, now.lexeme }) catch |_err| {
                    std.debug.panic("Error: {any}\n", .{_err});
                };
                var hint: []u8 = std.fmt.allocPrint(self.allocator, "yaha '{s}' lekhnus", .{token_lexeme}) catch |_err| {
                    std.debug.panic("Error: {any}\n", .{_err});
                };
                errorutil.reportErrorFatal(now, msg, hint);
                self.allocator.free(msg);
                self.allocator.free(hint);
                self.has_error = true;
                return false;
            }
        }
        return true;
    }

    inline fn previous(self: *Self) ?scanner.Token {
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

    inline fn skip(self: *Self) void {
        self.current += 1;
    }

    inline fn isAtEnd(self: *Self) bool {
        return self.current >= self.tokens_len;
    }
};
