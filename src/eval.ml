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

open Result
open Types
open Dyn

(* TODO remove exception and use fail result *)
exception Runtime_error of string

(* misc that shouldn't be exported *)

let num_binop : (Int64.t -> Int64.t -> Int64.t) -> Dyn.t list -> Dyn.t
  = fun op ls ->
    List.map unwrap_int ls
    |> foldl1 op
    |> make_int

module Env = struct
  module M = Map.Make(String)
  type 'a t = 'a M.t
  let lookup env id =
    M.find_opt id env
  let extend env id v =
    M.add id v env
  let extend_many env ids vs =
    List.fold_left2 extend env ids vs
  let empty = M.empty

  (* base environment with core primitives *)
   (*
   * ; "list"
   * ; "cons"
   * ; "car"
   * ; "cdr"
] *)

  let base =
    let ext = fun id v e -> extend e id v in
    M.empty
    |> ext "datum->syntax" (Dyn.make_func (fun [v] -> datum_to_syntax v))
    |> ext "syntax->datum" (Dyn.make_func (fun [v] -> syntax_to_datum v))
    |> ext "syntax-e" (Dyn.make_func (function
        | [ Dyn(StxT, Stx (e, _)) ] -> Dyn(IdT, Id e)
        | s -> raise (Runtime_error ("expected syntax object but received todo "))))
    |> ext "map" (Dyn.make_func (function
        | [ Dyn(FuncT, Func f) ; Dyn(ListT, List ls) ] ->
          Dyn(ListT, List (List.map (fun v -> f [v]) ls))
        | s -> raise (Runtime_error "expected list object but received todo")))
    |> ext "+" (Dyn.make_func (num_binop (Int64.add)))
    |> ext "*" (Dyn.make_func (num_binop (Int64.mul)))
end

let rec eval ?env:(e = Env.base) s =
  match s with
  | s when is_bool s -> ok s
  | s when is_int s -> ok s
  | s when is_func s -> ok s

  | Dyn(ListT, List [ Dyn (IdT, Id "quote"); v]) ->
    ok v

  | Dyn(IdT, Id id) ->
    begin match Env.lookup e id with
      | Some d -> ok d
      | None -> error "symbol not found: todo"
    end

  (* Core forms *)

  (* NOTE currently support is only for single argument lambdas *)
  | Dyn(ListT, List [ Dyn (IdT, Id "lambda")
                    ; Dyn (ListT, List param_ids)
                    ; body]) ->
    ok (Dyn.Dyn (FuncT, Func (fun args ->
        let param_ids = List.map unwrap_id param_ids in
        eval body ~env:(Env.extend_many e param_ids args)
        |> function
        | Ok d -> d
        | Error s -> raise (Unexpected __LOC__)
      )))

  | Dyn(ListT, List (func :: args)) ->
    begin match (eval ~env:e func), (List.map (fun arg ->
        eval ~env:e arg) args) with
    | (Ok (Dyn(FuncT, Func f) as fdyn), args) ->
      ok (f (List.map get_ok args))
    | _, _ -> raise (Unexpected __LOC__)
    end

  | v -> raise (Unexpected __LOC__)

(* and apply : Dyn.t -> Dyn.t list -> Dyn.t
 *   = fun f vs ->
 *     match f with
 *     | Dyn.Dyn(FuncT, Func f) ->
 *       ok (f (Dyn(ListT, List vs)))
 *     | _ -> raise (Unexpected __LOC__) *)
