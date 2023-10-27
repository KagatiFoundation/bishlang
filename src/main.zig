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

pub const Interpreter = struct {
    stmts: std.ArrayList(ast.Stmt),
    var var_env: std.StringHashMap(ast.LiteralValueType) = std.StringHashMap(ast.LiteralValueType).init(std.heap.page_allocator);
    const Self = @This();

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
            else => {},
        }
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
                    var_env.put(var_name, value) catch |err| {
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
            .Boolean => |boolVal| std.debug.print("{s}\n", .{if (boolVal) "sahi" else "galat"}),
            .Integer => |intVal| std.debug.print("{d}\n", .{intVal}),
            .String => |strVal| std.debug.print("{s}\n", .{strVal}),
            else => {},
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
                return self.computeBinaryLiteralVal(left_evaled, right_evaled, bin_expr.operator);
            },
            else => null,
        };
    }

    fn evaluateVarExpr(self: *Self, expr: ast.Expr) ?ast.LiteralValueType {
        _ = self;
        switch (expr) {
            .VariableExpr => |var_expr| {
                if (Interpreter.var_env.get(var_expr.var_name)) |value| {
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

    fn computeBinaryLiteralVal(self: *Self, left: ast.LiteralValueType, right: ast.LiteralValueType, operator: []const u8) ?ast.LiteralValueType {
        _ = self;
        switch (left) {
            .Integer => |int| {
                if (std.mem.eql(u8, operator, "+")) {
                    return switch (right) {
                        .Integer => |int2| ast.LiteralValueType{ .Integer = int + int2 },
                        .Float => |float| ast.LiteralValueType{ .Float = @as(f32, @floatFromInt(int)) + float },
                        else => null,
                    };
                } else if (std.mem.eql(u8, operator, "-")) {
                    return switch (right) {
                        .Integer => |int2| ast.LiteralValueType{ .Integer = int - int2 },
                        .Float => |float2| ast.LiteralValueType{ .Float = @as(f32, @floatFromInt(int)) - float2 },
                        else => null,
                    };
                } else if (std.mem.eql(u8, operator, "*")) {
                    return switch (right) {
                        .Integer => |int2| ast.LiteralValueType{ .Integer = int * int2 },
                        .Float => |float2| ast.LiteralValueType{ .Float = @as(f32, @floatFromInt(int)) * float2 },
                        else => null,
                    };
                } else if (std.mem.eql(u8, operator, "/")) {
                    return switch (right) {
                        .Integer => |int2| ast.LiteralValueType{ .Integer = @divExact(int, int2) },
                        .Float => |float2| ast.LiteralValueType{ .Float = @as(f32, @floatFromInt(int)) / float2 },
                        else => null,
                    };
                } else if (std.mem.eql(u8, operator, "**")) {
                    return switch (right) {
                        .Integer => |int2| ast.LiteralValueType{ .Integer = std.math.pow(i32, int, int2) },
                        .Float => |float2| ast.LiteralValueType{ .Float = std.math.pow(f32, @as(f32, @floatFromInt(int)), float2) },
                        else => null,
                    };
                }
            },
            else => return null,
        }
        return null;
    }
};

pub fn main() !void {
    scanner.init();
    defer scanner.deinit();
    const source: []const u8 = "rakha a ma 2 ** 2; dekhau a";
    var ss = scanner.Scanner(source){};
    var tokens: std.ArrayList(scanner.Token) = try ss.scanTokens();
    if (!ss.has_error) {
        var p: parser.Parser = parser.Parser.init(source, tokens);
        defer parser.Parser.deinit();
        var stmts: std.ArrayList(ast.Stmt) = try p.parse();
        if (!parser.has_error) {
            var int: Interpreter = Interpreter{ .stmts = stmts };
            int.interpret();
        }
    }
}
