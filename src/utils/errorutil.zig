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

const scanner = @import("../scanner.zig");
const std = @import("std");

var _source: std.mem.SplitIterator(u8, .sequence) = undefined;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator: std.heap.ArenaAllocator = std.heap.ArenaAllocator.init(gpa.allocator());

pub fn initErrorEngine(source: []const u8) void {
    _source = std.mem.split(u8, source, "\n");
}

pub fn deinitErrorEngine() void {
    _ = gpa.deinit();
    allocator.deinit();
}

pub fn reportUnexpectedTokenError(actual: scanner.Token, expected: []const u8) void {
    var tmp_alloc: std.mem.Allocator = allocator.allocator();
    var msg: []u8 = std.fmt.allocPrint(tmp_alloc, "chaiyeko '{s}' tara vetiyo '{s}'", .{ expected, actual.lexeme }) catch |_err| {
        std.debug.panic("Error: {any}\n", .{_err});
    };
    var hint: []u8 = std.fmt.allocPrint(tmp_alloc, "yeslai hatayera '{s}' rakhnus", .{expected}) catch |_err| {
        std.debug.panic("Error: {any}\n", .{_err});
    };
    reportErrorFatal(actual, msg, hint);
    tmp_alloc.free(msg);
    tmp_alloc.free(hint);
}

pub fn reportErrorFatal(token: scanner.Token, msg: []const u8, hint: ?[]const u8) void {
    var idx: usize = 1;
    var source_line: []const u8 = undefined;
    while (_source.next()) |line| {
        if (idx == token.line) {
            source_line = line;
            _source.reset();
            break;
        }
        idx += 1;
    }
    std.debug.print("{d}:{d}: error: {s}\n", .{ token.line, token.column, msg });
    std.debug.print("    {s}\n", .{source_line});
    var spaces: [100]u8 = undefined;
    @memset(&spaces, 0);
    @memset(spaces[0 .. token.column - 1], ' ');
    std.debug.print("{s}", .{spaces});
    std.debug.print("    ^", .{});

    if (token.lexeme.len > 0) {
        for (0..token.lexeme.len - 1) |_| {
            std.debug.print("~", .{});
        }
    }
    if (hint) |_hint| {
        if (_hint.len > 0) {
            std.debug.print("\n{s}", .{spaces});
            std.debug.print("    |____\n", .{});
            std.debug.print("{s}     ", .{spaces});
            std.debug.print("    {s}\n", .{_hint});
        }
    }
    std.debug.print("\n", .{});
}
