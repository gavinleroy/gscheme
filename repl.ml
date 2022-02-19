(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

module Err = Types.Err

open struct
  open Types
  let ( >>= ), ( >>=? ) =
    Err.( >>= ), Err.( >>=? )
end

let start () : unit =
  Sys.catch_break true;

  let do_eval_print scm_obj =
    try
      (Expand_main.expand_expression scm_obj
       >>= Expand_main.compile
       >>=? (Cfg.is_eval_all ()))
        Expand_main.eval
      |>
      Terminal.display_result_endline ~ignore:(Util.is_void)
    with
    | Err.Unexpected tup ->
      Terminal.format_internal_exn_endline tup
    | Sys.Break -> Terminal.print_endline "interrupted ..."
  in

  let rec loop () : unit =
    Terminal.print_flush "> ";
    try
      (Terminal.read_expr ()
       |> Parser.scheme_object_of_string
       >>= (fun expr ->
           (if Cmd.is_command expr then
              Cmd.run expr
            else Err.ok expr))
       |> (function
           | Ok asts -> List.iter do_eval_print asts
           | Error e -> Terminal.display_exn_endline e)
      ; loop ())
    with
    | Sys.Break ->
      Terminal.force_newline ();
      loop ()

    | End_of_file ->
      Terminal.print_endline "Ctrl-D";
      exit 0
  in
  Terminal.init ();
  Terminal.print_endline "Welcome to GScheme v0.1\n";
  Expand_main.register ();
  loop ()
