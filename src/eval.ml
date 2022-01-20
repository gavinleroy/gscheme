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

(* TODO remove exception and use fail result *)
exception Runtime_error of string

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
    |> ext "datum->syntax" (Dyn(FuncT, Func datum_to_syntax))
    |> ext "syntax->datum" (Dyn(FuncT, Func syntax_to_datum))
    |> ext "syntax-e" (Dyn(FuncT, Func (function
        | Dyn(StxT, Stx (e, _)) -> Dyn(IdT, Id e)
        | s -> raise (Runtime_error ("expected syntax object but received " ^ fmt_dyn s)))))
    |> ext "map" (Dyn(FuncT, Func (function
        | Dyn(ListT, List [ Dyn(FuncT, Func f) ; Dyn(ListT, List ls)]) ->
          Dyn(ListT, List (List.map f ls))
        | s -> raise (Runtime_error ("expected list object but received " ^ fmt_dyn s)))))
end

let rec eval ?env:(e = Env.base) s =
  match s with
  | Dyn(IntT, Int i) -> ok s
  | Dyn(IdT, Id id) ->
    begin match Env.lookup e id with
      | Some d -> ok d
      | None -> error "symbol not found: todo"
    end
  | Dyn (FuncT, Func f) -> ok s

  (* Core primitives *)
  (* NOTE currently support is only for single argument lambdas *)
  | Dyn(ListT, List [ Dyn (IdT, Id "lambda")
                    ; Dyn (ListT, List [(Dyn(IdT, Id param_id))])
                    ; body]) ->
    ok (Dyn (FuncT, Func (fun args ->
        eval body ~env:(Env.extend e param_id args)
        |> function
        | Ok d -> d
        | Error s -> raise (Unexpected __LOC__)
      )))
  | Dyn(ListT, List [ Dyn (IdT, Id "quote"); v]) ->
    ok v

  | Dyn(ListT, List (func :: args)) ->
    begin match (eval ~env:e func), (List.map (fun arg ->
        eval ~env:e arg) args) with
    | Ok f, args -> apply f (List.map get_ok args)
    | _, _ -> raise (Unexpected __LOC__)
    end
  | v -> raise (Unexpected __LOC__)

and apply f vs =
  match f with
  | Dyn(FuncT, Func f) ->
    ok (f (Dyn(ListT, List vs)))
  | _ -> raise (Unexpected __LOC__)
