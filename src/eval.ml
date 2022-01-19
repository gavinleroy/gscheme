(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* Hygienic Racket-like Macro Expander     *)
(* Based off Matthew Flatt's:              *)
(*  "Let's Build a Hygenic Macro Expander" *)
(*      Strange Loop 2016                  *)
(*                                         *)
(*******************************************)

open Types
open Result

module Env = struct
  module M = Map.Make(String)
  type 'a t = 'a M.t
  let lookup env id =
    M.find_opt id env
  let extend env id (v : dyn) =
    M.add id v env
  let extend_many env ids vs =
    List.fold_left2 extend env ids vs
  let empty = M.empty
end

let rec eval ?env:(e = Env.empty) s =
  match s with
  | Dyn(IntT, Int i) -> ok s
  | Dyn(IdT, Id id) ->
    begin match Env.lookup e id with
      | Some d -> ok d
      | None -> error "symbol not found: todo"
    end
  | Dyn (FuncT, Func f) -> ok s
  (* NOTE currently support is only for single argument lambdas *)
  | Dyn(ListT, List [ Dyn (IdT, Id "lambda")
                    ; Dyn (ListT, List [(Dyn(IdT, Id param_id))])
                    ; body]) ->
    ok (Dyn (FuncT, Func (fun arg ->
        eval body ~env:(Env.extend e param_id arg)
        |> function
        | Ok d -> d
        | Error s -> raise (Unexpected __LOC__)
      )))
  | Dyn(ListT, List [ Dyn (IdT, Id "quote"); v]) ->
    ok v
  | Dyn(ListT, List (func :: [arg])) ->
    begin match (eval ~env:e func), (eval ~env:e arg) with
      | Ok f, Ok arg -> apply f arg
      | _, _ -> raise (Unexpected __LOC__)
    end
  | v -> raise (Unexpected __LOC__)

and apply f v =
  match f with
  | Dyn(FuncT, Func f) -> ok (f v)
  | _ -> raise (Unexpected __LOC__)
