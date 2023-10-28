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
const parser = @import("./parser.zig");
const ast = @import("./ast.zig");
const bu = @import("./utils//bishutil.zig");

pub const Interpreter = struct {
    stmts: std.ArrayList(ast.Stmt),
    var_env: std.StringHashMap(ast.LiteralValueType),
    const Self = @This();

    pub fn init(stmts: std.ArrayList(ast.Stmt)) Self {
        return Interpreter{
            .stmts = stmts,
            .var_env = std.StringHashMap(ast.LiteralValueType).init(std.heap.page_allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.var_env.deinit();
    }

    pub fn interpret(self: *Self) void {
        for (self.stmts.items) |stmt| {
            self.execStmt(stmt);
        }
    }

    fn execStmt(self: *Self, stmt: ast.Stmt) void {
        switch (stmt) {
            .DekhauStmt => |_| self.execDekhauStmt(stmt),
            .RakhaStmt => |_| self.execRakhaStmt(stmt),
            .YadiNatraStmt => |_| self.execYadiNatraStmt(stmt),
            .BlockStmt => |_| self.execBlockStmt(stmt),
            .GhumauStmt => |_| self.execGhumauStmt(stmt),
            else => {},
        }
    }

    fn execGhumauStmt(self: *Self, stmt: ast.Stmt) void {
        switch (stmt) {
            .GhumauStmt => |ghumau| {
                var ghumau_expr_val: ?ast.LiteralValueType = self.evaluateExpr(ghumau.expr);
                if (ghumau_expr_val) |value_type| {
                    var create_var: bool = false;
                    if (!std.mem.eql(u8, ghumau.identifier, "_")) create_var = true; // '_' is ignored
                    switch (value_type) {
                        .Float => |num_val| {
                            for (0..@as(usize, @intFromFloat(num_val))) |_num| {
                                if (create_var) {
                                    self.execStmt(Interpreter._createRakhaStmt(
                                        ghumau.identifier,
                                        .{
                                            .LiteralExpr = .{ .value = ast.LiteralValueType{ .Float = @floatFromInt(_num) } },
                                        },
                                    ));
                                }
                                self.execStmt(ghumau.stmt.*);
                            }
                        },
                        .String => |str_val| {
                            for (str_val) |char| {
                                if (create_var) {
                                    self.execStmt(Interpreter._createRakhaStmt(
                                        ghumau.identifier,
                                        .{
                                            .LiteralExpr = .{ .value = ast.LiteralValueType{ .String = &[1]u8{char} } },
                                        },
                                    ));
                                }
                                self.execStmt(ghumau.stmt.*);
                            }
                        },
                        else => {}, // 'ghumau' statement not supported for this type
                    }
                }
            },
            else => {},
        }
    }

    fn _createRakhaStmt(var_name: []const u8, expr: ast.Expr) ast.Stmt {
        return ast.Stmt{
            .RakhaStmt = .{
                .expr = expr,
                .var_name = var_name,
            },
        };
    }

    fn execBlockStmt(self: *Self, stmt: ast.Stmt) void {
        switch (stmt) {
            .BlockStmt => |block| {
                for (block.stmts.items) |_stmt| {
                    self.execStmt(_stmt.*);
                }
            },
            else => {},
        }
    }

    fn execYadiNatraStmt(self: *Self, stmt: ast.Stmt) void {
        switch (stmt) {
            .YadiNatraStmt => |yadi_natra| {
                if (self.evaluateExpr(yadi_natra.condition)) |eval_res| {
                    var condition: bool = switch (eval_res) {
                        .Boolean => |bool_val| bool_val,
                        .Integer => |int_val| int_val != 0,
                        else => false,
                    };
                    if (condition) {
                        self.execStmt(yadi_natra.yadi_sahi.*);
                    } else {
                        if (@intFromPtr(yadi_natra.yadi_galat) != 0xAAAAAAAAAAAAAAAA) { // 0xAAAAAAAAAAAAAAAA = undefined
                            self.execStmt(yadi_natra.yadi_galat.*.?);
                        }
                    }
                }
            },
            else => {},
        }
    }

    fn execRakhaStmt(self: *Self, stmt: ast.Stmt) void {
        switch (stmt) {
            .RakhaStmt => |rakha| {
                var var_name = rakha.var_name;
                if (self.evaluateExpr(rakha.expr)) |value| {
                    self.var_env.put(var_name, value) catch |err| {
                        std.debug.print("{any}\n", .{err});
                    };
                }
            },
            else => {},
        }
    }

    fn execDekhauStmt(self: *Self, stmt: ast.Stmt) void {
        switch (stmt) {
            .DekhauStmt => |dekhau| {
                switch (dekhau.expr) {
                    else => |_| {
                        if (self.evaluateExpr(dekhau.expr)) |value| {
                            Interpreter.dumpLiteralValueType(value);
                        }
                    },
                }
            },
            else => {},
        }
    }

    fn dumpLiteralValueType(lit: ast.LiteralValueType) void {
        switch (lit) {
            .Boolean => |bool_val| std.debug.print("{s}\n", .{if (bool_val) "sahi" else "galat"}),
            .Integer => |int_val| std.debug.print("{d}\n", .{int_val}),
            .String => |str_val| std.debug.print("{s}\n", .{str_val}),
            .Float => |float_val| {
                if (float_val == @trunc(float_val)) {
                    std.debug.print("{d:.0}\n", .{float_val});
                } else {
                    std.debug.print("{d:.6}\n", .{float_val});
                }
            },
        }
    }

    fn evaluateExpr(self: *Self, expr: ast.Expr) ?ast.LiteralValueType {
        return switch (expr) {
            .LiteralExpr => |lit_expr| lit_expr.value,
            .VariableExpr => |_| self.evaluateVarExpr(expr),
            .BinaryExpr => |_| self.evaluateBinaryExpr(expr),
            .GroupExpr => |_| self.evaluateGroupExpr(expr),
        };
    }

    fn evaluateGroupExpr(self: *Self, expr: ast.Expr) ?ast.LiteralValueType {
        return switch (expr) {
            .GroupExpr => |grp_expr| self.evaluateExpr(grp_expr.expr.*),
            else => null,
        };
    }

    fn evaluateBinaryExpr(self: *Self, expr: ast.Expr) ?ast.LiteralValueType {
        return switch (expr) {
            .BinaryExpr => |bin_expr| {
                var left_evaled: ast.LiteralValueType = undefined;
                if (self.evaluateExpr(bin_expr.left.*)) |_expr| {
                    left_evaled = _expr;
                }

                var right_evaled: ast.LiteralValueType = undefined;
                if (self.evaluateExpr(bin_expr.right.*)) |_expr| {
                    right_evaled = _expr;
                }
                return Interpreter.computeBinaryLiteralVal(left_evaled, right_evaled, bin_expr.operator);
            },
            else => null,
        };
    }

    fn evaluateVarExpr(self: *Self, expr: ast.Expr) ?ast.LiteralValueType {
        switch (expr) {
            .VariableExpr => |var_expr| {
                if (self.var_env.get(var_expr.var_name)) |value| {
                    return value;
                } else {
                    std.debug.print("'{s}' naam gareko variable tapaile pahile banaunu vayeko chhaina\n", .{var_expr.var_name});
                    return null;
                }
            },
            else => {
                return null;
            },
        }
    }

    fn computeBinaryLiteralVal(left: ast.LiteralValueType, right: ast.LiteralValueType, op: []const u8) ?ast.LiteralValueType {
        if (bu.strMatchesAny(op, &[2][]const u8{ "ra", "ya" })) {
            return Interpreter.evalBinaryOpLogical(op, left, right);
        } else if (bu.strMatchesAny(op, &[5][]const u8{ "+", "-", "/", "*", "**" })) {
            return Interpreter.evalBinaryOpArith(op, left, right);
        } else if (bu.strcmp(op, "barabar")) {
            return Interpreter.evalBinaryOpEquality(left, right);
        } else if (bu.strMatchesAny(op, &[2][]const u8{ "sano", "thulo" })) {
            return Interpreter.evalBinaryOpComparision(op, left, right);
        }
        return null;
    }

    fn evalBinaryOpComparision(op: []const u8, left: ast.LiteralValueType, right: ast.LiteralValueType) ?ast.LiteralValueType {
        var val1: f64 = switch (left) {
            .Float => |float_val| float_val,
            else => 0, // TODO: this is error, can'a apply comparision operator to types other than floats
        };
        var val2: f64 = switch (right) {
            .Float => |float_val| float_val,
            else => 0, // TODO: this is error, can'a apply comparision operator to types other than floats
        };
        if (bu.strcmp(op, "sano")) {
            return ast.LiteralValueType{ .Boolean = val1 < val2 };
        } else if (bu.strcmp(op, "thulo")) {
            return ast.LiteralValueType{ .Boolean = val1 > val2 };
        }
        return null;
    }

    fn evalBinaryOpEquality(left: ast.LiteralValueType, right: ast.LiteralValueType) ?ast.LiteralValueType {
        var bool_res: bool = false;
        switch (left) {
            .Float => |float_val| {
                bool_res = switch (right) {
                    .Float => |float_val2| float_val == float_val2,
                    .Boolean => |bool_val| (float_val != 0) == bool_val,
                    else => false,
                };
            },
            .Boolean => |bool_val| {
                bool_res = switch (right) {
                    .Float => |float_val| bool_val == (float_val != 0),
                    .Boolean => |bool_val2| bool_val == bool_val2,
                    else => false,
                };
            },
            .String => |str_val| {
                bool_res = switch (right) {
                    .String => |str_val2| bu.strcmp(str_val, str_val2),
                    else => false,
                };
            },
            else => {},
        }
        return ast.LiteralValueType{ .Boolean = bool_res };
    }

    // TODO: add support for string concatination
    fn evalBinaryOpArith(operator: []const u8, left: ast.LiteralValueType, right: ast.LiteralValueType) ?ast.LiteralValueType {
        var val1: f64 = switch (left) {
            .Integer => |int_val| @as(f64, @floatFromInt(int_val)),
            .Float => |float_val| float_val,
            else => 0, // TODO: error: arithmetic operator not supported for this type
        };
        var val2: f64 = switch (right) {
            .Integer => |int_val| @as(f64, @floatFromInt(int_val)),
            .Float => |float_val| float_val,
            else => 0, // TODO: error: arithmetic operator not supported for this type
        };
        if (std.mem.eql(u8, operator, "+")) {
            return ast.LiteralValueType{ .Float = val1 + val2 };
        } else if (std.mem.eql(u8, operator, "-")) {
            return ast.LiteralValueType{ .Float = val1 - val2 };
        } else if (std.mem.eql(u8, operator, "/")) {
            return ast.LiteralValueType{ .Float = val1 / val2 };
        } else if (std.mem.eql(u8, operator, "*")) {
            return ast.LiteralValueType{ .Float = val1 * val2 };
        } else if (std.mem.eql(u8, operator, "**")) {
            return ast.LiteralValueType{ .Float = std.math.pow(f64, val1, val2) };
        }
        return null;
    }

    fn evalBinaryOpLogical(operator: []const u8, left: ast.LiteralValueType, right: ast.LiteralValueType) ?ast.LiteralValueType {
        var expr_res1 = switch (left) {
            .Boolean => |bool_val| bool_val,
            .Integer => |int_val| int_val != 0,
            else => false, // TODO: error: operator not supported for the given type
        };
        var expr_res2 = switch (right) {
            .Boolean => |bool_val| bool_val,
            .Integer => |int_val| int_val != 0,
            else => false, // TODO: error: operator not supported for the given type
        };
        if (std.mem.eql(u8, operator, "ra")) {
            return ast.LiteralValueType{ .Boolean = expr_res1 and expr_res2 };
        } else if (std.mem.eql(u8, operator, "ya")) {
            return ast.LiteralValueType{ .Boolean = expr_res1 or expr_res2 };
        }
        // unknown operator
        return ast.LiteralValueType{ .Boolean = false };
    }
};

pub fn main() !void {
    scanner.init();
    defer scanner.deinit();
    const source =
        \\ rakha str_len ma 0;
        \\ rakha name ma "ramesh poudel";
        \\ ghumau name patak |_| suru
        \\      rakha str_len ma str_len + 1;
        \\ antya
        \\ 
        \\ dekhau str_len;
    ;
    var ss = scanner.Scanner(source){};
    var tokens: std.ArrayList(scanner.Token) = try ss.scanTokens();
    if (!ss.has_error) {
        var p: parser.Parser = parser.Parser.init(source, tokens);
        defer parser.Parser.deinit();
        var stmts: std.ArrayList(ast.Stmt) = try p.parse();
        if (!parser.has_error) {
            var int: Interpreter = Interpreter.init(stmts);
            defer int.deinit();
            int.interpret();
        }
    }
}
