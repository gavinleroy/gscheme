(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
module U = Util

(* TODO put somewhere else *)
module Hidden = struct

  let bool_binop : type a. (scheme_object -> a maybe_exn) -> (a -> a -> bool) -> scheme_object list -> scheme_object maybe_exn
    = fun cnv op args ->
      match args with
      | [ lhs; rhs ] ->
        cnv lhs >>= fun l ->
        cnv rhs >>= fun r ->
        op l r |> U.make_bool |> ok
      | _ -> error (Arity_mismatch (2, List.length args, args))

  let num_binop : (Number.t -> Number.t -> Number.t) -> scheme_object list -> scheme_object maybe_exn
    = fun op ls ->
      match ls with
      (* TODO require at least two arguments for numeric operations, however,
       *      some could be done with 1. For example, (+ 1) => 1
       **)
      | (_ :: _ :: _) ->
        map_m U.unwrap_num ls
        >>= fun ls ->
        U.List.foldl1 op ls
        |> U.make_num |> ok
      | _ -> error (Arity_mismatch (2, List.length ls, ls))

  let predicate : (scheme_object -> bool) -> (scheme_object list -> scheme_object maybe_exn)
    = fun t -> fun ls -> match ls with
      | [ o ] -> Ok (U.make_bool (t o))
      | _ -> error (Arity_mismatch (1, List.length ls, ls))

  let num_bool_binop = bool_binop U.unwrap_int
  let bool_bool_binop = bool_binop U.unwrap_bool
  (* let string_bool_binop = bool_binop U.unwrap_string *)

  let car : scheme_object list -> scheme_object maybe_exn
    = function
      | [ S_obj (ListT, List (v :: _)) ]
      | [ S_obj (DottedT, Dotted ((v :: _), _)) ] ->
        ok v
      | [ arg ] ->
        error (Type_mismatch ("pair?", arg))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let cdr : scheme_object list -> scheme_object maybe_exn
    = function
      | [ S_obj (ListT, List (_ :: ls)) ] ->
        U.make_list ls |> ok
      | [ S_obj (DottedT, Dotted ([_], tl)) ] ->
        ok tl
      | [ S_obj (DottedT, Dotted ((_ :: ls), tl)) ] ->
        U.make_dotted (ls, tl) |> ok
      | [ arg ] ->
        error (Type_mismatch ("pair?", arg))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let cons : scheme_object list -> scheme_object maybe_exn
    = function
      | [ x; S_obj (ListT, List []) ] ->
        U.make_list [x] |> ok
      | [ x; S_obj (ListT, List ls) ] ->
        U.make_list (x :: ls) |> ok
      | [ x; S_obj (DottedT, Dotted (ls, tl)) ] ->
        U.make_dotted (x :: ls, tl) |> ok
      | [ x; y ] ->
        U.make_dotted ([x], y) |> ok
      | args -> error (Arity_mismatch (2, List.length args, args))

  (* IO primitives (some of) *)

  let open_input_file
    = function
      | [ S_obj (StringT, String fn) ] ->
        ReadPort (open_in fn)
        |> U.make_port |> ok
      | [ bad ] ->
        error (Type_mismatch ("string?", bad))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let open_output_file
    = function
      | [ S_obj (StringT, String fn) ] ->
        WritePort (open_out fn)
        |> U.make_port |> ok
      | [ bad ] ->
        error (Type_mismatch ("string?", bad))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let close_input_port
    = function
      | [ S_obj (PortT, Port (ReadPort p)) ] ->
        begin
          close_in p;
          ok U.make_void
        end
      | [ bad ] ->
        error (Type_mismatch ("input-port?", bad))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

  let close_output_port
    = function
      | [ S_obj (PortT, Port (WritePort p)) ] ->
        begin
          close_out p;
          ok U.make_void
        end
      | [ bad ] ->
        error (Type_mismatch ("output-port?", bad))
      | args ->
        error (Arity_mismatch (1, List.length args, args))

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
 *      is bound with variable arguments, but the signature doesn't
 *      enforce security.
 **)
let extend_many : type a. a t -> id list -> a list -> a t
  = fun env ids vs ->
    let rec loop acc is vs = match is,vs with
      | (i :: is), (v :: vs) ->
        loop (extend acc i v) is vs
      | (_ :: _), []
      | [], []
      | [], (_ :: _) -> acc
    in loop env ids vs

let empty : type a. a t
  = M.empty

(* base environment with /eventually/ all the core primitives *)

let base : scheme_object Box.t t
  = let open Hidden in
  let ext = fun id v e -> extend e id (Box.make v) in
  M.empty

  (* predicates *)
  |> ext "boolean?" (U.make_proc (predicate U.is_bool))
  |> ext "symbol?" (U.make_proc (predicate U.is_id))
  |> ext "char?" (U.make_proc (predicate U.is_char))
  |> ext "vector?" (U.make_proc (predicate U.is_vector))
  |> ext "null?" (U.make_proc (predicate U.is_null))
  |> ext "pair?" (U.make_proc (predicate U.is_pair))
  |> ext "number?" (U.make_proc (predicate U.is_number))
  |> ext "string?" (U.make_proc (predicate U.is_string))
  |> ext "procedure?" (U.make_proc (predicate U.is_func))

  |> ext "cons" (U.make_proc cons)
  |> ext "car" (U.make_proc car)
  |> ext "cdr" (U.make_proc cdr)

  |> ext "+" (U.make_proc (num_binop Number.add))
  |> ext "-" (U.make_proc (num_binop Number.sub))
  |> ext "*" (U.make_proc (num_binop Number.mul))
(* |> ext "/" (U.make_proc (num_binop Number.div)) *)
(* |> ext "=" (U.make_proc (num_bool_binop Number.equal)) *)

let%test_module _ = (module struct

  open Util.Test

  let%test _ = (
    extend empty "x" 10
    |> (fun env ->
        lookup env "x")
       = Ok 10)

  let%test _ = (extend empty "y" 10
                |> (fun env -> extend env "x" 0)
                |> (fun env -> lookup env "y")
                   = Ok 10)

  let%test _ = (extend empty "x" 10
                |> (fun env -> extend env "x" 0)
                |> (fun env -> lookup env "x")
                   = Ok 0)

  let%test _ = (expect_exn (Free_var ("", ""))
                  (lookup empty "y"))
end)
