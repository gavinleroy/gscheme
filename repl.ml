(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

let repl_fmt = Format.std_formatter

let display_result v =
  if not (Util.is_void v) then
    begin
      Util.format_scheme_obj repl_fmt v;
      Format.print_flush ();
      print_newline ()
    end

let display_exn e =
  Util.format_runtime_exn repl_fmt e;
  Format.print_flush ();
  print_newline ()

let start () =

  let do_eval_print env scm_obj =
    Eval.eval ~env:env scm_obj |> begin function
      | Ok (ref_val, env') ->
        display_result  (Box.get ref_val);
        env'
      | Error e ->
        display_exn e;
        env
    end
  in

  let rec loop env =
    print_string "> ";
    (* TODO instead of reading a single line, allow the user
     * to type an expression accross multiple lines and only capture
     * once all parens are close
     ***)
    begin match read_line () |> Parser.scheme_object_of_string with
      | Ok asts ->
        List.fold_left do_eval_print env asts
        |> loop
      | Error e ->
        display_exn e;
        loop env
    end

  in
  Format.pp_set_geometry ~max_indent:6 ~margin:30 repl_fmt;
  Printf.printf "Welcome to GScheme v0.0.1\n";
  loop Namespace.base
