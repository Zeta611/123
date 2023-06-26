open Base
open Expr

(* Solve eval' e c = exec c (eval e).
   This can be done systematically, using structural induction on e.
   When the free variables occur, augment the cont type with a new constructor that packs those. *)

type cont = HALT | EVAL of expr * cont | ADD of int * cont

let rec eval' e c =
  match e with Val n -> exec c n | Add (x, y) -> eval' x (EVAL (y, c))

and exec c n =
  match c with
  | HALT -> n
  | EVAL (y, c) -> eval' y (ADD (n, c))
  | ADD (m, c) -> exec c (m + n)

let eval e = eval' e HALT
let%test_unit "eval" = [%test_eq: int] (eval (Add (Val 1, Val 2))) 3
