(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm
open Types

let is_command = function
  | [List [ Id "unquote"; Id "cfg" ]; _] ->
    true
  | _ -> false

(** required command? => true *)
let run = function
  | [ _; cmd ] ->
    Err.map (Cfg.update_with_command cmd)
      (fun v -> [ v ])
  | o -> raise (Err.Unexpected ("'cmd-run invalid call", Util.make_list o))
