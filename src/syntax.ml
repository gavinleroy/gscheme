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

open struct
  let ( >>= ), ( >>| ) = Err.( >>= ), Err.( >>| )
end

let syntax_e : scheme_object -> scheme_object maybe_exn
  = function
    | Stx r -> Err.ok r.e
    | obj -> Err.error (Type_mismatch ("syntax?", obj))

let syntax_scopes : scheme_object -> Scopes.t maybe_exn
  = function
    | Stx r -> Err.ok r.scopes
    | obj -> Err.error (Type_mismatch ("syntax?", obj))

let empty_syntax =
  Stx { e = Types.void
      ; scopes = Scopes.empty }

let is_identifier : scheme_object -> bool
  = fun s -> U.is_syntax s &&
             begin match syntax_e s with
               | Ok obj when U.is_symbol obj -> true
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
    syntax_e s
    >>= function
    | e when U.is_list e ->
      Err.map_m syntax_to_datum (U.unwrap_list_exn e)
      >>| U.make_list
    | e -> Err.ok e

and datum_to_syntax : scheme_object option -> scheme_object -> scheme_object maybe_exn
  = fun stx_c_o v ->
    let wrap e =
      (match stx_c_o with
       | None -> Err.ok Scopes.empty
       | Some stx_c -> syntax_scopes stx_c)
      >>| fun scopes ->
      U.make_syntax
        { e = e
        ; scopes = scopes
        }
    in match v with
    | s when U.is_syntax s ->
      Err.ok s
    | s when U.is_list s ->
      Util.list_map_m (datum_to_syntax stx_c_o) s
      >>= wrap
    | s -> wrap s
