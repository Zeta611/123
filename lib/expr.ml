open Base

type expr = Val of int | Add of expr * expr [@@deriving sexp, compare, show]
