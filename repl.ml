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
    Format.print_flush ();
    print_newline ();
    print_string "> ";
    begin match read_line () |> Parser.sexpr_of_string with
      | Ok ast ->
        Util.dyn_of_sexp ast
        |> Eval.eval ~env:env
        |> begin function
          | Ok (ref_val, env') ->
            Util.format_scheme_obj Format.std_formatter !ref_val;
            loop env'
          | Error e ->
            Util.format_runtime_exn Format.std_formatter e;
        end
      | Error e ->
        Util.format_runtime_exn Format.std_formatter e;
    end; loop env
  in begin
    Format.pp_set_geometry ~max_indent:4 ~margin:100 Format.std_formatter;
    Printf.printf "Welcome to GScheme v0.0.1";
    loop Env.base
  end
