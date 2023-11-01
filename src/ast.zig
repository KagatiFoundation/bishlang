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
    Float: f64,
    String: []const u8,
    Boolean: bool,
    Null,
};

pub const Expr = union(enum) {
    LiteralExpr: struct {
        value: LiteralValueType,
    },

    VariableExpr: struct {
        var_name: []const u8,
    },

    BinaryExpr: struct {
        left: *Expr,
        operator: []const u8,
        right: *Expr,
    },

    GroupExpr: struct {
        expr: *Expr,
    },

    UnaryExpr: struct {
        operator: []const u8,
        expr: *Expr,
    },

    CallExpr: struct {
        name: []const u8,
        exprs: std.ArrayList(*Expr),
    },
};

pub const Stmt = union(enum) {
    DekhauStmt: struct {
        expr: Expr,
    },

    RakhaStmt: struct {
        var_name: []const u8,
        expr: Expr,
    },

    ExprStmt: struct {
        expr: Expr,
    },

    YadiNatraStmt: struct {
        condition: Expr,
        yadi_sahi: *Stmt,
        yadi_galat: *?Stmt,
    },

    BlockStmt: struct {
        stmts: std.ArrayList(*Stmt),
    },

    // <ghumau-statement> ::= GHUMAU <expression> PATAK [|<variable>|]  { <statement-list> }
    GhumauStmt: struct {
        // if expr is 'true', loop only once, if it 'false', do not loop at all
        // else if expr is 'string', loop over every character of the string
        //      and store it in 'identifier'
        // else if expr is 'int' loop that 'int' times
        expr: Expr,
        identifier: []const u8,
        stmt: *Stmt,
    },

    // <karya-statement> ::= KARYA <variable>([<parameter-list>]) <block-statement>
    KaryaDeclStmt: struct {
        name: []const u8,
        params: std.ArrayList([]const u8),
        stmt: *Stmt,
    },

    FarkauStmt: struct {
        expr: ?Expr,
    },
};
