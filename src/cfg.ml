(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types

type t = cfg_opt list

and cfg_opt =
  | Eval_all of bool

let cfg = ref
    [ Eval_all true
    (* TODO please add other configurations here *)
    ]

let is_eval_all () =
  match (List.find_opt (function
      | Eval_all _ -> true) !cfg) with
  | Some (Eval_all b) -> b
  | None -> raise (Err.Unexpected ("configuration missing `Eval_all", Types.void))

let set_eval b =
  cfg := List.map (function
      | Eval_all _ -> Eval_all b
      | o -> o) !cfg

let update_with_command = function
  | List [ Id "eval"; Bool b ] ->
    set_eval b; Err.ok void
  | List ( Id "eval" :: ots ) ->
    Err.error (Command ("cfg 'eval: requires bool?", Util.make_list ots))
  | oth -> Err.error (Command ("unrecognized aux command", oth))
