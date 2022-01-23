(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
module U = Util

(* TODO hidden will become its own module
 *      called Primitves -- or something similar
 **)
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
        U.List.foldl1 op ls
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

module M = Map.Make(String)

type 'a t = 'a M.t

let lookup : type a. a t -> id -> a maybe_exn
  = fun env id ->
    match M.find_opt id env with
    | Some r -> ok r
    | None -> error (Free_var ("", id))

let extend env id v =
  M.add id v env

(* NOTE this function is unsafe as it doesn't report errors on
 *      unbound variables. This should only be called when a function
 *      is bound with variable arguments, but the signature doeosn't
 *      enforce security.
 **)
let extend_many env ids vs =
  let rec loop acc is vs = match is,vs with
    | (i :: is), (v :: vs) ->
      loop (extend acc i v) is vs
    | (_ :: _), []
    | [], []
    | [], (_ :: _) -> acc
  in loop env ids vs

let empty = M.empty

(* base environment with core primitives *)

let base =
  let open Hidden in
  let ext = fun id v e -> extend e id (ref v) in
  M.empty
  |> ext "datum->syntax" (U.make_proc (fun [v] -> U.datum_to_syntax v |> ok))
  |> ext "syntax->datum" (U.make_proc (fun [v] -> U.syntax_to_datum v |> ok))

  (* |> ext "syntax-e" (make_func (function
   *     | [ Dyn(StxT, Stx (e, _)) ] -> Dyn(IdT, Id e)
   *     | s -> error (Runtime_error "expected syntax object but received todo "))) *)

  |> ext "cons" (U.make_proc cons)
  |> ext "car" (U.make_proc car)
  |> ext "cdr" (U.make_proc cdr)

  |> ext "+" (U.make_proc (num_binop (Int64.add)))
  |> ext "-" (U.make_proc (num_binop (Int64.sub)))
  |> ext "*" (U.make_proc (num_binop (Int64.mul)))
  |> ext "/" (U.make_proc (num_binop (Int64.div)))
  |> ext "remainder" (U.make_proc (num_binop (Int64.rem)))
  |> ext "=" (U.make_proc (num_bool_binop (Int64.equal)))
