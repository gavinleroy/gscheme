(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let () =
  let line = "'((1 . (1 2 . 3)))" in
  match Parser.sexpr_of_string line with
  | Ok ast ->
    (* TODO fix me pls *)
    Util.dyn_of_sexp ast
    (* |> Types.datum_to_syntax
     * |> Expander.expand *)
    |> Eval.eval |> Result.get_ok
    |> Util.fmt
    |> print_endline
  | Error (Types.Parser s) ->
    print_endline s
  | Error _ -> print_endline "OTHER"
