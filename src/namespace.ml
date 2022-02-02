(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
module U = Util

module Wrappers = struct

  (* NOTE this obviously isn't sustainable for n arg procedures but this can come with macros *)
  let single_arg_procedure : (scheme_object -> scheme_object maybe_exn) -> scheme_object
    = fun func -> U.make_proc (function
        | [ arg ] -> func arg
        | args -> error (Arity_mismatch (1, List.length args, args)))

  let double_arg_procedure : (scheme_object -> scheme_object -> scheme_object maybe_exn) -> scheme_object
    = fun func -> U.make_proc (function
        | [ arg1; arg2 ] -> func arg1 arg2
        | args -> error (Arity_mismatch (2, List.length args, args)))

  let min_2_arg_procedure : type a. (a -> a -> a) -> (scheme_object -> a maybe_exn) -> (a -> scheme_object) -> scheme_object
    = fun op cvt_from cvt_to ->
      U.make_proc (fun ls -> match ls with
          | ( _ :: _ :: _ ) ->
            map_m cvt_from ls >>= fun ls ->
            U.List.foldl1 op ls |> cvt_to |> ok
          | _ -> error (Arity_mismatch (2, List.length ls, ls)))

  let predicate : (scheme_object -> bool) -> scheme_object
    = fun t -> single_arg_procedure
        (fun o -> t o |> U.make_bool |> ok)

  let bool_binop : type a. (scheme_object -> a maybe_exn) -> (a -> a -> bool) -> scheme_object
    = fun cnv op -> double_arg_procedure
        (fun lhs rhs ->
           cnv lhs >>= fun l ->
           cnv rhs >>= fun r ->
           op l r |> U.make_bool |> ok)

  let num_bool_binop = bool_binop U.unwrap_num
  let bool_bool_binop = bool_binop U.unwrap_bool

  (* let string_bool_binop = bool_binop U.unwrap_string *)

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
  = let open Wrappers in
  let ext = fun id v e -> extend e id (Box.make v) in
  M.empty

  (* predicates *)
  |> ext "boolean?" (predicate U.is_bool)
  |> ext "symbol?" (predicate U.is_id)
  |> ext "char?" (predicate U.is_char)
  |> ext "vector?" (predicate U.is_vector)
  |> ext "null?" (predicate U.is_null)
  |> ext "pair?" (predicate U.is_pair)
  |> ext "number?" (predicate U.is_number)
  |> ext "string?" (predicate U.is_string)
  |> ext "procedure?" (predicate U.is_func)
  (* numerical predicates *)
  |> ext "integer?" (predicate U.is_integer)

  (* general boolean *)
  |> ext "not" (predicate U.is_not)

  |> ext "car" (single_arg_procedure Lib.car)
  |> ext "cdr" (single_arg_procedure Lib.cdr)
  (* Add additional cadr cdar ... *)
  |> ext "cons" (double_arg_procedure Lib.cons)
  |> ext "list-ref" (double_arg_procedure Lib.list_ref)

  |> ext "+" (min_2_arg_procedure Number.add U.unwrap_num U.make_num)
  |> ext "-" (min_2_arg_procedure Number.sub U.unwrap_num U.make_num)
  |> ext "*" (min_2_arg_procedure Number.mul U.unwrap_num U.make_num)
  |> ext "=" (num_bool_binop Number.equal)

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
