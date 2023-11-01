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

pub fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn strMatchesAny(needle: []const u8, haystack: []const []const u8) bool {
    for (haystack) |item| {
        if (strcmp(needle, item)) {
            return true;
        }
    }
    return false;
}

pub fn stringArrListContains(needle: []const u8, arr: std.ArrayList([]const u8)) bool {
    for (arr.items) |item| {
        if (strcmp(needle, item)) return true;
    }
    return false;
}
