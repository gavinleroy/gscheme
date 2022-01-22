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

module Hidden = struct

  let bool_binop : type a. (dyn -> a maybe_exn) -> (a -> a -> bool) -> dyn list -> dyn maybe_exn
    = fun cnv op args ->
      match args with
      | [ lhs; rhs ] ->
        cnv lhs >>= fun l ->
        cnv rhs >>= fun r ->
        op l r |> U.make_bool |> ok
      | _ -> error (Arity_mismatch (2, List.length args, args))

  let num_binop : (int64 -> int64 -> int64) -> dyn list -> dyn maybe_exn
    = fun op ls ->
      match ls with
      (* TODO require at least two arguments for numeric operations, however,
       *      some could be done with 1. For example, (+ 1) => 1
       **)
      | (_ :: _ :: _) ->
        map_m U.unwrap_int ls
        >>= fun ls ->
        U.foldl1 op ls
        |> U.make_int |> ok
      | _ -> error (Arity_mismatch (2, List.length ls, ls))

  let num_bool_binop = bool_binop U.unwrap_int
  let bool_bool_binop = bool_binop U.unwrap_bool
  (* let string_bool_binop = bool_binop U.unwrap_string *)

  let car : dyn list -> dyn maybe_exn
    = function
      | [ Dyn (ListT, List (v :: _)) ]
      | [ Dyn (DottedT, Dotted ((v :: _), _)) ] ->
        ok v
      | [ arg ] ->
        error (Type_mismatch ("", arg))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let cdr : dyn list -> dyn maybe_exn
    = function
      | [ Dyn (ListT, List (_ :: ls)) ] ->
        U.make_list ls |> ok
      | [ Dyn (DottedT, Dotted ([_], tl)) ] ->
        ok tl
      | [ Dyn (DottedT, Dotted ((_ :: ls), tl)) ] ->
        U.make_dotted (ls, tl) |> ok
      | [ arg ] ->
        error (Type_mismatch ("", arg))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let cons : dyn list -> dyn maybe_exn
    = function
      | [ x; Dyn (ListT, List []) ] ->
        U.make_list [x] |> ok
      | [ x; Dyn (ListT, List ls) ] ->
        U.make_list (x :: ls) |> ok
      | [ x; Dyn (DottedT, Dotted (ls, tl)) ] ->
        U.make_dotted (x :: ls, tl) |> ok
      | [ x; y ] ->
        U.make_dotted ([x], y) |> ok
      | args -> error (Arity_mismatch (2, List.length args, args))

end

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
    let open Hidden in
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

    |> ext "cons" (U.make_proc cons)
    |> ext "car" (U.make_proc car)
    |> ext "cdr" (U.make_proc cdr)

    |> ext "+" (U.make_proc (num_binop (Int64.add)))
    |> ext "-" (U.make_proc (num_binop (Int64.sub)))
    |> ext "*" (U.make_proc (num_binop (Int64.mul)))
    |> ext "/" (U.make_proc (num_binop (Int64.div)))
    |> ext "remainder" (U.make_proc (num_binop (Int64.rem)))
    |> ext "=" (U.make_proc (num_bool_binop (Int64.equal)))

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

  | Dyn(ListT, List [ Dyn(IdT, Id "if"); cond; te; fe ]) ->
    eval ~env:e cond >>= (function
        | Dyn(BoolT, Bool true) -> eval ~env:e te
        | _ -> eval ~env:e fe)

  | Dyn(ListT, List (func :: args)) ->
    eval ~env:e func >>= fun f ->
    map_m (eval ~env:e) args >>= fun args ->
    apply f args

  | v -> raise (Unexpected __LOC__)

and apply : dyn -> dyn list -> dyn maybe_exn
  = fun f args ->
    match f with
    | f when U.is_lambda f ->
      raise (Unexpected __LOC__)
    | f when U.is_proc f ->
      (f |> U.unwrap_proc |> get_ok) args
    | _ -> error (Type_mismatch ("todo", f))
