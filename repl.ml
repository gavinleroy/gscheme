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
    begin match read_line ()
                |> Parser.sexpr_of_string with
    | Ok asts ->
      List.map Util.scheme_object_of_sexp asts
      |> List.fold_left do_eval_print env
      |> loop
    | Error e ->
      Util.format_runtime_exn Format.std_formatter e;
      loop env
    end

  and do_eval_print env scm_obj =
    Eval.eval ~env:env scm_obj |> begin function
      | Ok (ref_val, env') ->
        Box.get ref_val
        |> Util.format_scheme_obj Format.std_formatter;
        env'
      | Error e ->
        Util.format_runtime_exn Format.std_formatter e;
        env
    end
  in
  Format.pp_set_geometry ~max_indent:6 ~margin:100 Format.std_formatter;
  Printf.printf "Welcome to GScheme v0.0.1";
  loop Namespace.base
