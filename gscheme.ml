(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let () =
  let line = "((lambda (x) x) 1)" in
  match Parser.sexpr_of_string line with
  | Ok ast ->
    (* TODO fix me pls *)
    Expander.sexpr_to_dyn ast
    (* |> Types.datum_to_syntax
     * |> Expander.expand *)
    |> Eval.eval
    |> Result.get_ok
    |> Types.fmt_dyn
    |> print_endline
  | Error str ->
    print_endline str
