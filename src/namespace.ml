(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
open Err
module U = Util

module Wrappers = struct

  (* NOTE this obviously isn't sustainable for n arg procedures but this can come with macros *)
  let single_arg_procedure (* : (scheme_object -> scheme_object Err.t) -> scheme_object *)
    = fun func -> (function
        | [ arg ] -> func arg
        | args -> error (Arity_mismatch (1, List.length args, args)))

  let double_arg_procedure (* : (scheme_object -> scheme_object -> scheme_object Err.t) -> scheme_object *)
    = fun func -> (function
        | [ arg1; arg2 ] -> func arg1 arg2
        | args -> error (Arity_mismatch (2, List.length args, args)))

  let triple_arg_procedure
    = fun func -> (function
        | [ arg1; arg2; arg3 ] -> func arg1 arg2 arg3
        | args -> error (Arity_mismatch (3, List.length args, args)))

  let min_2_arg_procedure (* : type a. (a -> a -> a) -> (scheme_object -> a Err.t) -> (a -> scheme_object) -> scheme_object *)
    = fun op cvt_from cvt_to ->
      (fun ls -> match ls with
         | ( _ :: _ :: _ ) ->
           map_m cvt_from ls >>= fun ls ->
           U.List.foldl1 op ls |> cvt_to |> ok
         | _ -> error (Arity_mismatch (2, List.length ls, ls)))

  let predicate (* : (scheme_object -> bool) -> scheme_object *)
    = fun t -> single_arg_procedure
        (fun o -> t o |> U.make_bool |> ok)

  let bool_binop (* : type a. (scheme_object -> a Err.t) -> (a -> a -> bool) -> scheme_object *)
    = fun cnv op -> double_arg_procedure
        (fun lhs rhs ->
           cnv lhs >>= fun l ->
           cnv rhs >>= fun r ->
           op l r |> U.make_bool |> ok)

  let num_bool_binop = bool_binop U.unwrap_num
  let bool_bool_binop = bool_binop U.unwrap_bool

  (* let string_bool_binop = bool_binop U.unwrap_string *)

end

type 'a t = 'a dyn_ref_map

let mx_size = 1000

let fail_no_tables () =
  raise (Unexpected ("namespace-extend called with an empty list", Types.void))

let lookup : type a. a t -> Identifier.t -> a Err.t
  = fun nmspc id ->
    List.find_opt (fun tbl ->
        Hashtbl.mem tbl id) nmspc
    |> function
    | Some tbl ->
      Hashtbl.find tbl id |> ok
    | None -> error (Free_var (id, None))

let extend : type a. a t -> Identifier.t -> a -> unit
  = fun nmspc id v ->
    match nmspc with
    | [] -> fail_no_tables ()
    | (tbl :: _) ->
      Hashtbl.add tbl id v

(* NOTE this function is unsafe as it doesn't report errors on
 *      unbound variables. This should only be called when a function
 *      is bound with variable arguments, but the signature doesn't
 *      enforce security.
 **)
let extend_many : type a. a t -> id list -> a list -> unit
  = fun nmspc ids vs ->
    match nmspc with
    | [] -> fail_no_tables ()
    | (tbl :: _) ->
      let rec iter2 is vs = match is, vs with
        | [], [] | (_ :: _), []
        | [], (_ :: _) -> ()
        | (i :: is), (v :: vs) ->
          Hashtbl.add tbl i v;
          iter2 is vs
      in
      iter2 ids vs

let empty_table () =
  Hashtbl.create ~random:false mx_size

let empty : type a. unit -> a t
  = fun () -> [ empty_table () ]

(* base environment with /eventually/ all the core primitives *)

let base_table () =
  let open Wrappers in
  let base = empty_table () in
  let extend i p =
    Hashtbl.add base i (Box.make (Util.make_proc (i, p))) in
  begin
    extend "boolean?" (predicate U.is_bool);
    extend "symbol?" (predicate U.is_id);
    extend "char?" (predicate U.is_char);
    extend "vector?" (predicate U.is_vector);
    extend "null?" (predicate U.is_null);
    extend "pair?" (predicate U.is_pair);
    extend "number?" (predicate U.is_number);
    extend "string?" (predicate U.is_string);
    extend "procedure?" (predicate U.is_func);
    (* numerical predicates *)
    extend "integer?" (predicate U.is_integer);

    (* general boolean *)
    extend "not" (predicate U.is_not);

    extend "car" (single_arg_procedure Lib.car);
    extend "cdr" (single_arg_procedure Lib.cdr);
    extend "caar" (single_arg_procedure Lib.caar);
    extend "cddr" (single_arg_procedure Lib.cddr);
    extend "cadr" (single_arg_procedure Lib.cadr);
    extend "cdar" (single_arg_procedure Lib.cdar);
    (* Add additional cadr cdar ... *)
    extend "cons" (double_arg_procedure Lib.cons);
    extend "list-ref" (double_arg_procedure Lib.list_ref);

    (* FIXME move these to lib wrappers *)
    extend "list" Lib.list;
    extend "vector" Lib.vector;

    extend "make-vector" Lib.vector_make;
    extend "vector-ref" (double_arg_procedure Lib.vector_ref);
    extend "vector-set!" (triple_arg_procedure Lib.vector_set);

    extend "+" (min_2_arg_procedure Number.add U.unwrap_num U.make_num);
    extend "-" (min_2_arg_procedure Number.sub U.unwrap_num U.make_num);
    extend "*" (min_2_arg_procedure Number.mul U.unwrap_num U.make_num);
    extend "=" (num_bool_binop Number.equal);

    base
  end

let base : unit -> scheme_object Box.t t
  = fun () -> [ base_table () ]

let open_scope nmspc =
  base_table () :: nmspc

let close_scope = function
  | [] -> fail_no_tables ()
  | (_ :: rest) -> rest

(* predicates *)

let%test_module _ = (module struct

  open Util.Test


  let%test _ = (let nmspc = empty () in
                extend nmspc "x" 10;
                lookup nmspc "x"
                = Ok 10)

  let%test _ = (let nmspc = empty () in
                extend nmspc "y" 10;
                extend nmspc "x" 0;
                lookup nmspc "y"
                = Ok 10)

  let%test _ = (let nmspc = empty () in
                extend nmspc "x" 10;
                extend nmspc "x" 0;
                lookup nmspc "x";
                = Ok 0)

  let%test _ = (let nmspc = empty () in
                expect_exn (Free_var ("", None))
                  (lookup nmspc "y"))
end)
