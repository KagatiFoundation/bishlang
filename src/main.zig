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
const bu = @import("./utils/bishutil.zig");
const scope = @import("./scope.zig");
const sym_table = @import("./symtable.zig");

const InterpretResult = union(enum) {
    Success,
    Return: ast.LiteralValueType,
    Break,
    Continue,
};

pub const Interpreter = struct {
    stmts: std.ArrayList(ast.Stmt),
    _internal_scope: scope.Scope,
    allocator: std.mem.Allocator,
    sym_table: sym_table.Symtable,
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, stmts: std.ArrayList(ast.Stmt), syms: sym_table.Symtable) Self {
        return Interpreter{
            .stmts = stmts,
            ._internal_scope = scope.Scope.init(allocator),
            .allocator = allocator,
            .sym_table = syms,
        };
    }

    pub fn deinit(self: *Self) void {
        self._internal_scope.deinit();
        for (self.stmts.items) |item| {
            switch (item) {
                .KaryaDeclStmt => |karya_decl| {
                    karya_decl.params.deinit();
                },
                else => {},
            }
        }
        self.stmts.deinit();
    }

    pub fn interpret(self: *Self) void {
        for (self.stmts.items) |stmt| {
            switch (self.execStmt(stmt)) {
                .Return => break,
                else => continue,
            }
        }
    }

    fn execStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        return switch (stmt) {
            .DekhauStmt => |_| self.execDekhauStmt(stmt),
            .RakhaStmt => |_| self.execRakhaStmt(stmt),
            .YadiNatraStmt => |_| self.execYadiNatraStmt(stmt),
            .BlockStmt => |_| self.execBlockStmt(stmt),
            .GhumauStmt => |_| self.execGhumauStmt(stmt),
            .KaryaDeclStmt => |_| self.execKaryaDeclStmt(stmt),
            .ExprStmt => |_| self.execExprStmt(stmt), // expression statement values are ignored
            .FarkauStmt => |_| self.execFarkauStmt(stmt),
            .RokaStmt => |_| self.execRokaStmt(stmt),
        };
    }

    fn execExprStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .ExprStmt => |expr_stmt| {
                _ = self.evaluateExpr(expr_stmt.expr);
            },
            else => {},
        }
        return .Success;
    }

    fn execRokaStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        _ = self;
        return switch (stmt) {
            .RokaStmt => .Break,
            else => .Success,
        };
    }

    fn execFarkauStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .FarkauStmt => |farkau_stmt| {
                if (farkau_stmt.expr) |expr| {
                    return InterpretResult{ .Return = self.evaluateExpr(expr) };
                }
            },
            else => {},
        }
        return .Success;
    }

    fn execKaryaDeclStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .KaryaDeclStmt => |karya| {
                var karya_name: []const u8 = karya.name;
                self._internal_scope.createNewFunc(karya_name, stmt);
            },
            else => {},
        }
        return .Success;
    }

    fn execGhumauStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .GhumauStmt => |ghumau| {
                var value_type: ast.LiteralValueType = self.evaluateExpr(ghumau.expr);
                var create_var: bool = false;
                if (!std.mem.eql(u8, ghumau.identifier, "_")) create_var = true; // '_' is ignored
                switch (value_type) {
                    .Float => |num_val| {
                        for (0..@as(usize, @intFromFloat(num_val))) |_num| {
                            if (create_var) {
                                _ = self.execStmt(Interpreter._createRakhaStmt(
                                    ghumau.identifier,
                                    .{
                                        .LiteralExpr = .{ .value = ast.LiteralValueType{ .Float = @floatFromInt(_num) } },
                                    },
                                ));
                            }
                            var result: InterpretResult = self.execStmt(ghumau.stmt.*);
                            switch (result) {
                                .Success => continue,
                                .Break => break,
                                else => return result,
                            }
                        }
                    },
                    .String => |str_val| {
                        for (str_val) |char| {
                            if (create_var) {
                                _ = self.execStmt(Interpreter._createRakhaStmt(
                                    ghumau.identifier,
                                    .{
                                        .LiteralExpr = .{ .value = ast.LiteralValueType{ .String = &[1]u8{char} } },
                                    },
                                ));
                            }
                            var result: InterpretResult = self.execStmt(ghumau.stmt.*);
                            switch (result) {
                                .Success => continue,
                                .Break => break,
                                else => return result,
                            }
                        }
                    },
                    else => {}, // TODO: error: 'ghumau' statement not supported for this type
                }
            },
            else => {},
        }
        return .Success;
    }

    fn _createRakhaStmt(var_name: []const u8, expr: ast.Expr) ast.Stmt {
        return ast.Stmt{
            .RakhaStmt = .{
                .expr = expr,
                .var_name = var_name,
            },
        };
    }

    fn execBlockStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .BlockStmt => |block| {
                for (block.stmts.items) |_stmt| {
                    var result: InterpretResult = self.execStmt(_stmt.*);
                    switch (result) {
                        .Success => continue,
                        else => return result,
                    }
                }
            },
            else => {},
        }
        return .Success;
    }

    fn execYadiNatraStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .YadiNatraStmt => |yadi_natra| {
                var condition: bool = switch (self.evaluateExpr(yadi_natra.condition)) {
                    .Boolean => |bool_val| bool_val,
                    .Float => |int_val| int_val != 0,
                    .String => |str_val| !bu.strcmp(str_val, ""),
                    else => false,
                };
                if (condition) {
                    return self.execStmt(yadi_natra.yadi_sahi.*);
                } else {
                    if (@intFromPtr(yadi_natra.yadi_galat) != 0xAAAAAAAAAAAAAAAA) { // 0xAAAAAAAAAAAAAAAA = undefined
                        return self.execStmt(yadi_natra.yadi_galat.*.?);
                    }
                }
            },
            else => {},
        }
        return .Success;
    }

    fn execRakhaStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .RakhaStmt => |rakha| {
                var var_name: []const u8 = rakha.var_name;
                self._internal_scope.var_env.put(var_name, self.evaluateExpr(rakha.expr)) catch |err| {
                    std.debug.print("{any}\n", .{err});
                };
            },
            else => {},
        }
        return .Success;
    }

    fn execDekhauStmt(self: *Self, stmt: ast.Stmt) InterpretResult {
        switch (stmt) {
            .DekhauStmt => |dekhau| Interpreter.dumpLiteralValueType((self.evaluateExpr(dekhau.expr))),
            else => {},
        }
        return .Success;
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
            .Null => std.debug.print("nil\n", .{}),
        }
    }

    fn evaluateExpr(self: *Self, expr: ast.Expr) ast.LiteralValueType {
        return switch (expr) {
            .LiteralExpr => |lit_expr| lit_expr.value,
            .VariableExpr => |_| self.evaluateVarExpr(expr),
            .BinaryExpr => |_| self.evaluateBinaryExpr(expr),
            .GroupExpr => |_| self.evaluateGroupExpr(expr),
            .UnaryExpr => |_| self.evaluateUnaryExpr(expr),
            .CallExpr => |_| self.evaluateCallExpr(expr),
        };
    }

    fn evaluateCallExpr(self: *Self, expr: ast.Expr) ast.LiteralValueType {
        switch (expr) {
            .CallExpr => |karya| {
                var karya_name: []const u8 = karya.name;
                if (self._internal_scope.func_decls.get(karya_name)) |karya_body| {
                    switch (karya_body) {
                        .KaryaDeclStmt => |karya_decl| {
                            var current_scope: scope.Scope = self._internal_scope;
                            var new_scope: scope.Scope = current_scope.copy();
                            self._internal_scope = new_scope;
                            // TODO: error: check parameter length and argument length
                            for (0..karya_decl.params.items.len) |idx| {
                                var param_name: []const u8 = karya_decl.params.items[idx];
                                var param_value: ast.LiteralValueType = self.evaluateExpr(karya.exprs.items[idx].*);
                                self._internal_scope.createNewVar(param_name, param_value);
                            }
                            var ret_val = switch (self.execStmt(karya_decl.stmt.*)) {
                                .Return => |lit_val| lit_val,
                                else => .Null,
                            };
                            new_scope.deinit();
                            self._internal_scope = current_scope;

                            for (karya.exprs.items) |expr_item| {
                                self.allocator.destroy(expr_item);
                            }
                            karya.exprs.deinit(); // free the expressions list
                            return ret_val;
                        },
                        else => return .Null,
                    }
                }
                // TODO: error: function not found
                return .Null;
            },
            else => return .Null,
        }
    }

    fn evaluateUnaryExpr(self: *Self, expr: ast.Expr) ast.LiteralValueType {
        switch (expr) {
            .UnaryExpr => |unary| {
                var evaluated: ast.LiteralValueType = self.evaluateExpr(unary.expr.*);
                if (bu.strcmp(unary.operator, "chhaina")) {
                    switch (evaluated) {
                        .Boolean => |bool_val| return ast.LiteralValueType{ .Boolean = !bool_val },
                        .Float => |float_val| return ast.LiteralValueType{ .Float = if (float_val != 0) 0 else 1 },
                        else => return evaluated, // TODO: error: operator not supported for the given type
                    }
                } else return evaluated;
            },
            else => return .Null,
        }
    }

    fn evaluateGroupExpr(self: *Self, expr: ast.Expr) ast.LiteralValueType {
        return switch (expr) {
            .GroupExpr => |grp_expr| self.evaluateExpr(grp_expr.expr.*),
            else => .Null,
        };
    }

    fn evaluateBinaryExpr(self: *Self, expr: ast.Expr) ast.LiteralValueType {
        return switch (expr) {
            .BinaryExpr => |bin_expr| {
                var left_evaled: ast.LiteralValueType = self.evaluateExpr(bin_expr.left.*);
                var right_evaled: ast.LiteralValueType = self.evaluateExpr(bin_expr.right.*);
                return Interpreter.computeBinaryLiteralVal(left_evaled, right_evaled, bin_expr.operator);
            },
            else => .Null,
        };
    }

    fn evaluateVarExpr(self: *Self, expr: ast.Expr) ast.LiteralValueType {
        switch (expr) {
            .VariableExpr => |var_expr| {
                if (self._internal_scope.var_env.get(var_expr.var_name)) |value| {
                    return value;
                } else {
                    std.debug.print("'{s}' naam gareko variable tapaile pahile banaunu vayeko chhaina\n", .{var_expr.var_name});
                    return .Null;
                }
            },
            else => {
                return .Null;
            },
        }
    }

    fn computeBinaryLiteralVal(left: ast.LiteralValueType, right: ast.LiteralValueType, op: []const u8) ast.LiteralValueType {
        if (bu.strMatchesAny(op, &[2][]const u8{ "ra", "ya" })) {
            return Interpreter.evalBinaryOpLogical(op, left, right);
        } else if (bu.strMatchesAny(op, &[5][]const u8{ "+", "-", "/", "*", "**" })) {
            return Interpreter.evalBinaryOpArith(op, left, right);
        } else if (bu.strcmp(op, "barabar")) {
            return Interpreter.evalBinaryOpEquality(left, right);
        } else if (bu.strMatchesAny(op, &[2][]const u8{ "sano", "thulo" })) {
            return Interpreter.evalBinaryOpComparision(op, left, right);
        }
        return .Null;
    }

    fn evalBinaryOpComparision(op: []const u8, left: ast.LiteralValueType, right: ast.LiteralValueType) ast.LiteralValueType {
        var val1: f64 = switch (left) {
            .Float => |float_val| float_val,
            else => 0, // TODO: error: can'a apply comparision operator to types other than floats
        };
        var val2: f64 = switch (right) {
            .Float => |float_val| float_val,
            else => 0, // TODO: error: can'a apply comparision operator to types other than floats
        };
        if (bu.strcmp(op, "sano")) {
            return ast.LiteralValueType{ .Boolean = val1 < val2 };
        } else if (bu.strcmp(op, "thulo")) {
            return ast.LiteralValueType{ .Boolean = val1 > val2 };
        }
        return .Null;
    }

    fn evalBinaryOpEquality(left: ast.LiteralValueType, right: ast.LiteralValueType) ast.LiteralValueType {
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
    fn evalBinaryOpArith(operator: []const u8, left: ast.LiteralValueType, right: ast.LiteralValueType) ast.LiteralValueType {
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
        } else if (std.mem.eql(u8, operator, "%")) {
            if (@trunc(val1) == val1) {
                if (@trunc(val2) == val2) {
                    return ast.LiteralValueType{ .Float = @rem(val1, val2) };
                }
            }
            // TODO: error: '%' not supported for the given types
            return .Null;
        } else if (std.mem.eql(u8, operator, "**")) {
            return ast.LiteralValueType{ .Float = std.math.pow(f64, val1, val2) };
        }
        return .Null;
    }

    fn evalBinaryOpLogical(operator: []const u8, left: ast.LiteralValueType, right: ast.LiteralValueType) ast.LiteralValueType {
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator: std.heap.ArenaAllocator = std.heap.ArenaAllocator.init(gpa.allocator());

    const source =
        \\ karya b() suru farkau 3; antya
        \\ rakha b ma 3;
        \\ dekhau b();
    ;
    var ss: scanner.Scanner = scanner.Scanner.init(allocator.allocator(), source);
    var tokens: std.ArrayList(scanner.Token) = try ss.scanTokens();
    if (!ss.has_error) {
        var p: parser.Parser = parser.Parser.init(allocator.allocator(), source, tokens);
        var stmts: std.ArrayList(ast.Stmt) = try p.parse();
        if (!p.has_error) {
            var int: Interpreter = Interpreter.init(allocator.allocator(), stmts, p.sym_table);
            int.interpret();
            int.deinit();
        }
        p.deinit();
    }
    ss.deinit();
    allocator.deinit();
    switch (gpa.deinit()) {
        .leak => {
            std.debug.print("PANICKED BECAUSE MEMORY LEAK...", .{});
        },
        .ok => {},
    }
}
