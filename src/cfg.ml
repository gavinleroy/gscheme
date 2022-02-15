(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

type 'a t = cfg_opt list

and cfg_opt =
  | Eval_all of bool

let cfg = ref
    [ Eval_all true
    (* XXX please add other configurations here *)
    ]

let is_eval_all () =
  match (List.find_opt (function
      | Eval_all _ -> true) !cfg) with
  | Some (Eval_all b) -> b
  | None -> raise (Types.Err.Unexpected ("configuration missing `Eval_all", Types.void))

let turn_eval_off () =
  cfg := List.map (function
      | Eval_all _ -> Eval_all false
      | o -> o) !cfg
