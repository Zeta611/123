open Stdio
open Easy_as_123

let () =
  let open Expr in
  let open Small_step in
  (* let open Contextual in *)
  let expr = Add (Add (Val 1, Val 2), Add (Val 3, Val 4)) in
  printf "%s\n" (show_tree pp_expr ((unfold (fun x -> x) trans) expr))
