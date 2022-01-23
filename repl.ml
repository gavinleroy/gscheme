(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let start () =
  let rec loop env =
    print_string "> ";
    begin match read_line () |> Parser.sexpr_of_string with
      | Ok ast ->

        Util.dyn_of_sexp ast
        |> Eval.eval ~env:env
        |> begin function
          | Ok (ref_val, env') ->
            print_endline (Util.fmt !ref_val);
            loop env'
          | Error _ ->
            print_endline "and error occurred todo :|"
        end

      | Error (Types.Parser s) ->
        print_endline s

      | Error _ -> print_endline "todo errors"
    end;
    loop env

  in begin

    Printf.printf "Welcome to GScheme v0.0.1\n";
    loop Env.base

  end
