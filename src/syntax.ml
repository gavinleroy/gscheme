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
    | S_obj (StxT, Stx r) -> Err.ok r.e
    | obj -> Err.error (Type_mismatch ("syntax?", obj))

let syntax_scopes : scheme_object -> Scopes.t maybe_exn
  = function
    | S_obj (StxT, Stx r) -> Err.ok r.scopes
    | obj -> Err.error (Type_mismatch ("syntax?", obj))

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
  = fun s -> let open Err in
    syntax_e s >>= function
    | e when U.is_list e ->
      map_m syntax_to_datum (U.unwrap_list_exn e)
      >>| U.make_list
    | e -> ok e

and datum_to_syntax : scheme_object option -> scheme_object -> scheme_object
  = fun stx_c_o v ->
    let wrap e =
      let scopes =
        match stx_c_o with
        | None -> Scopes.empty
        | Some stx_c ->
          (syntax_scopes stx_c
           |> function
           | Ok o -> o
           | Error _ ->
             raise (Err.Unexpected
                      ("'datum->syntax called with a non-syntax object", stx_c)))
      in
      U.make_syntax
        { e = e
        ; scopes = scopes
        }
    in match v with
    | s when U.is_syntax s -> s
    | s when U.is_list s ->
      Util.list_map (datum_to_syntax stx_c_o) s
      |> Err.get_ok (* NOTE datum->syntax is a safe function*)
      |> wrap
    | s -> wrap s
