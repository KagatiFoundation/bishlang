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
            switch (stmt) {
                .DekhauStmt => |_| self.execDekhauStmt(stmt),
                .RakhaStmt => |_| self.execRakhaStmt(stmt),
            }
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
            .Boolean => |boolVal| std.debug.print("{s}\n", .{switch (boolVal) {
                true => "sahi",
                false => "galat",
            }}),
            .Integer => |intVal| std.debug.print("{d}\n", .{intVal}),
            .String => |strVal| std.debug.print("{s}\n", .{strVal}),
            else => {},
        }
    }

    fn evaluateExpr(self: *Self, expr: ast.Expr) ?ast.LiteralValueType {
        return switch (expr) {
            .LiteralExpr => |lit_expr| lit_expr.value,
            .VariableExpr => |_| self.evaluateVarExpr(expr),
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
};

pub fn main() !void {
    scanner.init();
    defer scanner.deinit();

    const source = "rakha name ma sahi;\ndekhau namee;";
    var ss = scanner.Scanner(source){};
    var tokens: std.ArrayList(scanner.Token) = try ss.scanTokens();
    var p = parser.Parser.init(source, tokens);
    var stmts: std.ArrayList(ast.Stmt) = try p.parse();
    var interp = Interpreter{ .stmts = stmts };
    interp.interpret();
}
