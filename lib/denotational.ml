open Base
open Expr

let rec fold f g = function
  | Val n -> f n
  | Add (x, y) -> g (fold f g x) (fold f g y)

(** Valuation function must be compositional, and hence is modular.
    Defined by folding over the syntax. *)
let eval e = fold (fun x -> x) ( + ) e

let%test_unit "eval" =
  [%test_eq: int] (eval (Add (Val 1, Add (Val 1, Val 2)))) 4
