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
var _alloc_main = std.heap.page_allocator;
var _arena_alloc = std.heap.ArenaAllocator.init(_alloc_main);

pub fn initErrorEngine(source: []const u8) void {
    _source = std.mem.split(u8, source, "\n");
}

pub fn deinitErrorEngine() void {
    _arena_alloc.deinit();
}

pub fn reportUnexpectedTokenError(actual: scanner.Token, expected: []const u8) void {
    var tmp_alloc = _arena_alloc.allocator();
    var msg = try std.fmt.allocPrint(tmp_alloc, "chaiyeko '{s}' tara vetiyo '{s}'", .{ actual.lexeme, expected });
    var hint = try std.fmt.allocPrint(tmp_alloc, "yeslai hatayera '{s}' rakhnus", .{expected});
    reportErrorFatal(actual, msg, hint);
    tmp_alloc.free(msg);
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
    std.debug.print("    ^\n", .{});
    if (hint) |_hint| {
        std.debug.print("{s}", .{spaces});
        std.debug.print("    |____\n", .{});
        std.debug.print("{s}     ", .{spaces});
        std.debug.print("    {s}\n", .{_hint});
    }
}
