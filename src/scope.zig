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
const ast = @import("./ast.zig");

pub const Scope = struct {
    var_env: std.StringHashMap(ast.LiteralValueType),
    func_decls: std.StringHashMap(ast.Stmt),
    allocator: std.mem.Allocator,
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .var_env = std.StringHashMap(ast.LiteralValueType).init(allocator),
            .func_decls = std.StringHashMap(ast.Stmt).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.var_env.deinit();
        self.func_decls.deinit();
    }

    pub fn createNewVar(self: *Self, var_name: []const u8, var_value: ast.LiteralValueType) void {
        self.var_env.put(var_name, var_value) catch |err| {
            std.debug.panic("Paniced during variable creation: {any}\n", .{err});
        };
    }

    pub fn getVarValue(self: *Self, var_name: []const u8) ?ast.LiteralValueType {
        if (self.var_env.get(var_name)) |var_value| return var_value;
        return null;
    }

    pub fn createNewFunc(self: *Self, func_name: []const u8, func_body: ast.Stmt) void {
        self.func_decls.put(func_name, func_body) catch |err| {
            std.debug.panic("Paniced during function creation: {any}\n", .{err});
        };
    }

    pub fn getFuncBody(self: *Self, func_name: []const u8) ?ast.Stmt {
        if (self.func_decls.get(func_name)) |func_body| return func_body;
        return null;
    }

    pub fn copy(self: *Self) Self {
        var new_scope: Scope = Scope.init(self.allocator);
        var var_env = self.var_env.iterator();
        var func_env = self.func_decls.iterator();
        while (var_env.next()) |_var| {
            new_scope.createNewVar(_var.key_ptr.*, _var.value_ptr.*);
        }
        while (func_env.next()) |_func| {
            new_scope.createNewFunc(_func.key_ptr.*, _func.value_ptr.*);
        }
        return new_scope;
    }
};
