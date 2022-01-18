(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let () =
  (* let line = read_line () in *)
  match Parser.sexpr_of_string "(+ 1 (some/symbol 3 4 5))" with
  | Ok _ ->
    print_endline "Done"
  | Error str -> print_endline str
