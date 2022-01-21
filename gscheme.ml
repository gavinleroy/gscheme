(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let () =
  let line = "(map (lambda (x) (* x 2)) '(1 2 3 4))" in
  match Parser.sexpr_of_string line with
  | Ok ast ->
    (* TODO fix me pls *)
    Types.Dyn.of_sexpr ast
    (* |> Types.datum_to_syntax
     * |> Expander.expand *)
    |> Eval.eval
    |> Result.get_ok
    |> Types.Dyn.fmt
    |> print_endline
  | Error str ->
    print_endline str
