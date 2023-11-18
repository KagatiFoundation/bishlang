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

const NSYMBOLS: usize = 1024; // max number of symbols

pub const SymInfo = struct {
    name: []const u8,
    sym_type: union(enum) {
        Function,
        Variable,
    },
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
        var cc: usize = 0;
        for (self.syms.items) |symbol| {
            if (bu.strcmp(symbol.name, name)) {
                return self.syms.items[cc];
            }
            cc += 1;
        }
        return null;
    }

    pub fn add(self: *Self, sym: SymInfo) usize {
        var pos = self.find(sym.name);
        if (pos != 0xFFFFFFFF) return 0xFFFFFFFF; // symbol name already exists, can't add another with the same name
        pos = self.next();
        self.syms.append(sym) catch |err| {
            std.debug.panic("Error appending into symbol table: {any}", .{err});
        };
        return pos;
    }
};
