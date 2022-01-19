(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let () =
  let line = "(lambda (x) x)" in
  match Parser.sexpr_of_string line with
  | Ok ast ->
    let _ = Expander.expand (* TODO fix this please *)
        (Expander.sexpr_to_dyn ast) in
    print_endline "todo"
  | Error str ->
    print_endline str
