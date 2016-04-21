/// Copyright (C) 2016 The Authors.
module Expression

// The terminals of the abstract syntax tree.
type Expr =
    FNum of float | FVar of string | FAdd of Expr * Expr | FMult of Expr * Expr | FSub of Expr * Expr | FDiv of Expr * Expr | FExponent of Expr * Expr | FRoot of Expr * Expr

// Parse a character sequence and construct an abstract syntax tree.
val parse : char seq -> Expr
