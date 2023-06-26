open Base
open Expr

(** Differs from the denotational semantics that it does not need to be compositional. *)
let rec eval = function
  | Val n -> [ n ]
  | Add (x, y) ->
      let open List.Let_syntax in
      let%bind n = eval x in
      let%bind m = eval y in
      return (n + m)
