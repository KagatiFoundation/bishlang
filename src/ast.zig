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

pub const LiteralValueType = union(enum) {
    Integer: i32,
    Float: f32,
    String: []const u8,
    Boolean: bool,
};

pub const Expr = union(enum) {
    LiteralExpr: struct {
        value: LiteralValueType,
    },

    BinaryExpr: struct {
        left: *Expr,
        operator: []const u8,
        right: *Expr,
    },
};

pub const Stmt = union(enum) {
    PrintStmt: struct {
        expr: Expr,
    },

    DekhauStmt: struct {
        expr: Expr,
    },
};