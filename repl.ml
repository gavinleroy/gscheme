(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

open struct
  let ( >>= ), ( >>=? ) = Types.Err.( >>= ), Types.Err.( >>=? )
end

(* MAIN LOOP *)

let start () =

  let do_eval_print scm_obj =
    try
      (Expand_main.expand_expression scm_obj
       >>= Expand_main.compile
       >>=? (Cfg.is_eval_all ()))
        Expand_main.eval
      |>
      Terminal.display_result_endline ~ignore:(Util.is_void)
    with
      Types.Err.Unexpected tup ->
      Terminal.format_internal_exn_endline tup
  in

  let rec loop () =
    Terminal.print_flush "> ";
    (* TODO instead of reading a single line, allow the user
     * to type an expression accross multiple lines and only capture
     * once all parens are close
     ***)
    begin match Terminal.read_expr ()
                |> Parser.scheme_object_of_string with
    | Ok asts ->
      List.iter do_eval_print asts;
      loop ()
    | Error e ->
      Terminal.display_exn_endline e;
      loop ()
    end

  in
  Terminal.init ();
  Terminal.print_endline "Welcome to GScheme v0.0.1\n";
  Expand_main.register ();
  loop ()
