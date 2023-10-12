const std = @import("std");

pub fn main() !void {
    std.debug.print("All you {s}", .{"code is here"});
}
