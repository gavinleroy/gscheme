(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* Hygienic Racket-like Macro Expander     *)
(* Based off Matthew Flatt's:              *)
(*  "Let's Build a Hygenic Macro Expander" *)
(*      Strange Loop 2016                  *)
(*                                         *)
(*******************************************)

open struct
  open Types
  let ( >>= ) = ( >>= )
end

let core_scope =
  Scope.fresh ()

let core_syntax =
  Scope.add_scope Syntax.empty_syntax core_scope

let core_forms =
  Hashtbl.create 1000

let core_primitives =
  Hashtbl.create 1000

let unwrap_sym_exn s =
  Util.unwrap_symbol s |> Types.get_ok

let add_core_binding_bang sym =
  if not (Util.is_symbol sym) then
    raise (Types.Unexpected ("symbol? predicate for core-binding", sym));
  Scope.add_binding_bang
    (Syntax.datum_to_syntax core_syntax sym)
    (`Core_binding  (unwrap_sym_exn sym))
  |> function | Ok () -> ()
              | Error _ -> raise (Types.Unexpected
                                    ("unexpected failure in 'add-core-binding!", Types.void))

let add_core_form_bang : Types.scheme_object -> Types.scheme_object -> unit
  = fun sym proc ->
    if not (Util.is_symbol sym) then
      raise (Types.Unexpected ("symbol? predicate for core-binding", sym));
    add_core_binding_bang sym;
    Hashtbl.add  core_forms sym proc

let add_core_primitive_bang : Types.scheme_object -> Types.scheme_object -> unit
  = fun sym vl ->
    if not (Util.is_symbol sym) then
      raise (Types.Unexpected ("symbol? predicate for core-binding", sym));
    add_core_binding_bang sym;
    Hashtbl.add core_primitives sym vl

let core_form_sym s =
  let id = Util.make_symbol "id"
  and udscr = Util.make_symbol "_" in
  let obj = Util.make_dotted ([ id ], udscr) in
  match Match.try_match_syntax s obj with
  | None -> None
  | Some m ->
    (m id >>= fun res ->
     Scope.resolve res >>=
     (function
       | None -> Types.error (Types.Bad_form ("unbound", res))
       | Some b ->
         if Binding.is_core_binding b then
           Binding.core_sym b |> Types.ok
         else Types.error (Types.Bad_form ("unbound", res))))
    |> Types.to_option
