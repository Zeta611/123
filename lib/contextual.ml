open Base
open Expr

type con = Hole | AddL of con * expr | AddR of expr * con
[@@deriving sexp_of, compare]

let rec fill con e =
  match con with
  | Hole -> e
  | AddL (c, e') -> Add (fill c e, e')
  | AddR (e', c) -> Add (e', fill c e)

let%test_unit "fill" =
  [%test_eq: expr] (fill (AddL (Hole, Val 1)) (Val 2)) (Add (Val 2, Val 1))

let rec split e =
  (Hole, e)
  ::
  (match e with
  | Val _ -> []
  | Add (x, y) ->
      let open List.Let_syntax in
      (let%bind c, e = split x in
       return (AddL (c, y), e))
      @
      let%bind c, e = split y in
      return (AddR (x, c), e))

let%test_unit "split" =
  [%test_eq: (con * expr) list]
    (split (Add (Val 1, Val 2)))
    [
      (Hole, Add (Val 1, Val 2));
      (AddL (Hole, Val 2), Val 1);
      (AddR (Val 1, Hole), Val 2);
    ]

type expr_list = expr list [@@deriving show]

let reduce = function Add (Val n, Val m) -> [ Val (n + m) ] | _ -> []

(** Separate basic rules and structural rules *)
let trans e =
  let open List.Let_syntax in
  let%bind c, x = split e in
  let%bind x' = reduce x in
  return (fill c x')
