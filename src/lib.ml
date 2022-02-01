(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
open Util

(* NOTE library functions should operate on scheme objects
 * these are then wrapped in the namespace module for different
 * argument lenghts
 **)

(* pair / list primitives *)

let car : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (ListT, List (v :: _))
    | S_obj (DottedT, Dotted ((v :: _), _)) ->
      ok v
    | arg -> error (Type_mismatch ("pair?", arg))

let cdr : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (ListT, List (_ :: ls)) ->
      make_list ls |> ok
    | S_obj (DottedT, Dotted ([_], tl)) ->
      ok tl
    | S_obj (DottedT, Dotted ((_ :: ls), tl)) ->
      make_dotted (ls, tl) |> ok
    | arg -> error (Type_mismatch ("pair?", arg))

let cons : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | x, S_obj (ListT, List []) ->
      make_list [x] |> ok
    | x, S_obj (ListT, List ls) ->
      make_list (x :: ls) |> ok
    | x, S_obj (DottedT, Dotted (ls, tl)) ->
      make_dotted (x :: ls, tl) |> ok
    | x, y -> make_dotted ([x], y) |> ok

let list_ref : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | S_obj (ListT, List ls), S_obj (NumT, Num (Number.Int idx)) ->
      let idx = (Int64.to_int idx) in
      if idx < List.length ls then
        List.nth ls idx |> ok
      else error (Runtime_error (Printf.sprintf "index %d out of range" idx))
    | ls, intgr when is_list ls ->
      error (Type_mismatch ("int?", intgr))
    | ls, _ -> error (Type_mismatch ("list?", ls))

(* IO primitives *)

let open_input_file : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (StringT, String fn) ->
      ReadPort (open_in fn)
      |> make_port |> ok
    | bad -> error (Type_mismatch ("string?", bad))

let open_output_file : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (StringT, String fn) ->
      WritePort (open_out fn)
      |> make_port |> ok
    | bad -> error (Type_mismatch ("string?", bad))

let close_input_port : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (PortT, Port (ReadPort p)) ->
      begin
        close_in p;
        ok make_void
      end
    | bad -> error (Type_mismatch ("input-port?", bad))

let close_output_port : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (PortT, Port (WritePort p)) ->
      begin
        close_out p;
        ok make_void
      end
    | bad -> error (Type_mismatch ("output-port?", bad))
