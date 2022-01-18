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

(* TODO *)
module Env = struct
  module M = Map.Make(String)
  let lookup env id =
    M.find id env
  let extend env id v =
    M.add id v env
  let extend_many env ids vs =
    List.fold_left2 extend env ids vs
  let empty = M.empty
end

exception Todo

let rec eval ?env:(e = Env.empty) (s : dyn) : (dyn, string) t =
  match s with
  | Dyn (IntT, Int i) -> ok s
  | Dyn (IdT, Id id) ->
    begin match Env.lookup e id with
      | Some d -> ok d
      | None -> error "symbol not found: todo"
    end
  | Dyn (ListT, List [ Dyn (IdT, Id "lambda")
                     ; Dyn (ListT, List params)
                     ; body]) ->
    ok (Dyn (FuncT, Func (fun args ->
        let id_params = List.map (fun (Dyn(IdT, Id i)) ->
            i) params
        in
        eval body ~env:(Env.extend_many e id_params args))))
  | Dyn (ListT, List [ Dyn (IdT, Id "quote"); v]) ->
    ok v
(* TODO general application case *)
