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
const bu = @import("./utils/bishutil.zig");
const scanner = @import("./scanner.zig");

const NSYMBOLS: usize = 1024; // max number of symbols

// to represent different types of symbols
pub const SymType = union(enum) { Function, Variable };

pub const SymScope = union(enum) {
    FuncParam: struct {
        func_name: []const u8,
    },
    Global,
    BlockLocal,
};

pub const SymInfo = struct {
    name: []const u8,
    sym_type: SymType,
    token: ?scanner.Token,
    scope: SymScope,
};

pub const Symtable = struct {
    syms: std.ArrayList(SymInfo),
    counter: usize,
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .syms = std.ArrayList(SymInfo).init(allocator),
            .counter = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.syms.deinit();
    }

    pub fn next(self: *Self) usize {
        self.counter += 1;
        if (self.counter >= NSYMBOLS) {
            std.debug.panic("max number of symbol count reached", .{});
        }
        return self.counter;
    }

    pub fn find(self: *Self, name: []const u8) usize {
        if (self.syms.items.len == 0) return 0xFFFFFFFF;
        var cc: usize = 0;
        for (self.syms.items) |symbol| {
            if (bu.strcmp(symbol.name, name)) {
                return cc;
            }
            cc += 1;
        }
        return 0xFFFFFFFF; // if symbol not found
    }

    pub fn get(self: *Self, name: []const u8) ?SymInfo {
        for (self.syms.items) |symbol| {
            if (bu.strcmp(symbol.name, name)) {
                return symbol;
            }
        }
        return null;
    }

    pub fn add(self: *Self, sym: SymInfo) usize {
        if (self.find(sym.name) == 0xFFFFFFFF) {
            self.syms.append(sym) catch |err| {
                std.debug.panic("Error appending into symbol table: {any}", .{err});
            };
            var pos: usize = self.counter;
            _ = self.next();
            return pos;
        }
        // symbol name already exists, can't add another with the same name
        return 0xFFFFFFFF;
    }

    pub fn append(self: *Self, stable: Symtable) void {
        if (stable.syms.items.len == 0) return;
        for (stable.syms.items) |item| {
            self.syms.append(item) catch |err| {
                std.debug.panic("Error appending symbol table: {any}", .{err});
            };
        }
    }
};

test "symtable add" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var a = gpa.allocator();
    var alloc = std.heap.ArenaAllocator.init(a);
    var st = Symtable.init(alloc.allocator());
    try std.testing.expect(0 == st.add(SymInfo{ .name = "a", .sym_type = .Variable, .token = null }));
    // this addition should result in 0xFFFFFFFF because the symbol with same name already exists
    try std.testing.expect(0xFFFFFFFF == st.add(SymInfo{ .name = "a", .sym_type = .Variable, .token = null }));
    try std.testing.expect(1 == st.add(SymInfo{ .name = "b", .sym_type = .Variable, .token = null }));
    try std.testing.expect(2 == st.add(SymInfo{ .name = "c", .sym_type = .Variable, .token = null }));
    try std.testing.expect(0xFFFFFFFF == st.add(SymInfo{ .name = "c", .sym_type = .Variable, .token = null }));
    st.deinit();
    alloc.deinit();
    _ = gpa.deinit();
}

test "symtable find" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var a = gpa.allocator();
    var alloc = std.heap.ArenaAllocator.init(a);
    var st = Symtable.init(alloc.allocator());
    try std.testing.expect(0 == st.add(SymInfo{ .name = "a", .sym_type = .Variable, .token = null }));
    try std.testing.expect(0xFFFFFFFF == st.find("b"));
    try std.testing.expect(0 == st.find("a"));
    try std.testing.expect(0xFFFFFFFF == st.find("abc"));
    try std.testing.expect(0xFFFFFFFF == st.find(""));
    st.deinit();
    alloc.deinit();
    _ = gpa.deinit();
}

test "symtable get" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var a = gpa.allocator();
    var alloc = std.heap.ArenaAllocator.init(a);
    var st = Symtable.init(alloc.allocator());
    try std.testing.expect(0 == st.add(SymInfo{ .name = "a", .sym_type = .Variable, .token = null }));
    try std.testing.expect(.Variable == st.get("a").?.sym_type);
    try std.testing.expect(1 == st.add(SymInfo{ .name = "b", .sym_type = .Function, .token = null }));
    try std.testing.expect(bu.strcmp(st.get("b").?.name, "b"));
    try std.testing.expect(null == st.get("ddd"));
}
