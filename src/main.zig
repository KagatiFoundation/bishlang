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
    const Self = @This();

    pub fn interpret(self: *Self) void {
        for (self.stmts.items) |stmt| {
            switch (stmt) {
                .DekhauStmt => |_| self.execDekhauStmt(stmt),
                else => std.debug.print("Error\n", .{}),
            }
        }
    }

    fn execDekhauStmt(self: *Self, stmt: ast.Stmt) void {
        _ = self;
        switch (stmt) {
            .DekhauStmt => |dekhau| {
                switch (dekhau.expr) {
                    .LiteralExpr => |lit| {
                        switch (lit.value) {
                            .Boolean => |boolVal| std.debug.print("{s}\n", .{switch (boolVal) {
                                true => "sahi",
                                false => "galat",
                            }}),
                            .Integer => |intVal| std.debug.print("{d}\n", .{intVal}),
                            .String => |strVal| std.debug.print("{s}\n", .{strVal}),
                            else => {},
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
};

pub fn main() !void {
    scanner.init();
    defer scanner.deinit();

    const source = "dekhau 'ramesh poudel';";
    var ss = scanner.Scanner(source){};
    var tokens: std.ArrayList(scanner.Token) = try ss.scanTokens();
    var p = parser.Parser.init(source, tokens);
    var stmts: std.ArrayList(ast.Stmt) = try p.parse();
    var interp = Interpreter{ .stmts = stmts };
    interp.interpret();
}
