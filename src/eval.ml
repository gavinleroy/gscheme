(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module U = Util
open Types

(* misc that shouldn't be exported *)

let num_binop : (int64 -> int64 -> int64) -> dyn list -> dyn maybe_exn
  = fun op ls ->
    map_m U.unwrap_int ls
    >>= fun ls ->
    U.foldl1 op ls
    |> U.make_int |> ok

module Env = struct
  module M = Map.Make(String)
  type 'a t = 'a M.t

  let lookup : type a. a t -> id -> a maybe_exn
    = fun env id ->
      match M.find_opt id env with
      | Some r -> ok r
      | None -> error (Free_var ("", id))

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
    |> ext "datum->syntax" (U.make_proc (fun [v] -> U.datum_to_syntax v |> ok))
    |> ext "syntax->datum" (U.make_proc (fun [v] -> U.syntax_to_datum v |> ok))
    (* |> ext "syntax-e" (make_func (function
     *     | [ Dyn(StxT, Stx (e, _)) ] -> Dyn(IdT, Id e)
     *     | s -> error (Runtime_error "expected syntax object but received todo "))) *)

    (* |> ext "map" (U.make_proc (function
     *     | [ f ; Dyn(ListT, List ls) ] ->
     *       U.make_list (List.map (apply f) ls) |> ok
     *
     *     | s -> error (Runtime_error "expected list object but received todo"))) *)

    |> ext "+" (U.make_proc (num_binop (Int64.add)))
    |> ext "*" (U.make_proc (num_binop (Int64.mul)))
end

let rec eval ?env:(e = Env.base) s : dyn maybe_exn =
  match s with
  | s when U.is_bool s -> ok s
  | s when U.is_int s -> ok s
  | s when U.is_func s -> ok s

  | Dyn(ListT, List [ Dyn (IdT, Id "quote"); v]) ->
    ok v

  | Dyn(IdT, Id id) -> Env.lookup e id

  (* Core forms *)

  (* NOTE currently support is only for single argument lambdas *)
  (* | Dyn(ListT, List [ Dyn (IdT, Id "lambda")
   *                   ; Dyn (ListT, List param_ids)
   *                   ; body]) ->
   *   ok (Dyn (FuncT, Func (fun args ->
   *       map_m U.unwrap_id param_ids
   *       >>= fun param_ids ->
   *       eval body ~env:(Env.extend_many e param_ids args)
   *       |> function
   *       | Ok d -> d
   *       | Error s -> raise (Unexpected __LOC__)))) *)

  | Dyn(ListT, List (func :: args)) ->
    eval ~env:e func >>= fun f ->
    map_m (eval ~env:e) args >>= fun args ->
    apply f args

  | v -> raise (Unexpected __LOC__)

and apply : dyn -> dyn list -> dyn maybe_exn
  = fun f args ->
    match f with
    | f when U.is_lambda f ->
      apply_proc f args
    | f when U.is_proc f ->
      apply_lambda f args
    | _ -> error (Type_mismatch ("todo", f))

(* TODO unwrap the proc/lambda types and expect those in the apply_* respective signatures *)

and apply_proc : dyn -> dyn list -> dyn maybe_exn
  = fun f args ->
    raise (Unexpected __LOC__)

and apply_lambda : dyn -> dyn list -> dyn maybe_exn
  = fun f args ->
    raise (Unexpected __LOC__)
