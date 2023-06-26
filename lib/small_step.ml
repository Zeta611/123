open Base
open Expr

(** Transition relation, indirectly implemented by a non-deterministic function. *)
let rec trans = function
  | Val _ -> []
  | Add (Val n, Val m) -> [ Val (n + m) ]
  | Add (x, y) ->
      let open List.Let_syntax in
      (let%bind x' = trans x in
       return (Add (x', y)))
      @
      let%bind y' = trans y in
      return (Add (x, y'))

let%test_unit "trans" =
  [%test_eq: expr list]
    (trans (Add (Add (Val 1, Val 2), Add (Val 3, Val 4))))
    [ Add (Val 3, Add (Val 3, Val 4)); Add (Add (Val 1, Val 2), Val 7) ]

(** Transition tree *)
type 'a tree = Node of 'a * 'a tree list [@@deriving sexp_of, compare, show]

(** Unfolding to transition trees, where as denotational semantics folds over syntax trees.
    They are dual! *)
let rec unfold f g x =
  Node
    ( f x,
      let open List.Let_syntax in
      let%bind x' = g x in
      return (unfold f g x') )

let exec = unfold (fun x -> x) trans
