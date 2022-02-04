(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
module U = Util

type t = syntax_record

let syntax_e : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (StxT, Stx r) -> ok r.e
    | obj -> error (Type_mismatch ("syntax?", obj))

let syntax_scopes : scheme_object -> Scopes.t maybe_exn
  = function
    | S_obj (StxT, Stx r) -> ok r.scopes
    | obj -> error (Type_mismatch ("syntax?", obj))

let empty_syntax =
  S_obj (StxT, Stx { e = Types.void
                   ; scopes = Scopes.empty })

let is_identifier : scheme_object -> bool
  = fun s -> U.is_syntax s &&
             begin match syntax_e s with
               | Ok obj when U.is_id obj -> true
               | _ -> false
             end

let is_eq_bound_identifier : scheme_object -> scheme_object -> bool
  = fun a b -> syntax_e a = syntax_e b &&
               begin match syntax_scopes a, syntax_scopes b with
                 | Ok s1, Ok s2 -> Scopes.equal s1 s2
                 | _ -> false
               end

let rec syntax_to_datum
  = fun s ->
    syntax_e s >>= function
    | e when U.is_list e ->
      map_m syntax_to_datum (U.unwrap_list_exn e)
      >>| U.make_list
    | e -> ok e

and datum_to_syntax
  = fun stx_c v ->
    let wrap e =
      U.make_syntax
        { e = e; scopes = (syntax_scopes stx_c |> Result.value ~default:Scopes.empty) }
    in match v with
    | s when U.is_syntax s -> s
    | s when U.is_list s ->
      (List.map (datum_to_syntax stx_c) (U.unwrap_list_exn s))
      |> U.make_list |> wrap
    | s -> wrap v
