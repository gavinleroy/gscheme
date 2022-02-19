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

module Err = Types.Err

open struct
  type scheme_object = Types.scheme_object
  let ( >>= ) = Err.( >>= )
end

let core_scope =
  Scope.fresh ()

let core_syntax =
  Scope.add_scope Syntax.empty_syntax core_scope

let core_forms : (string, Types.scheme_object) Hashtbl.t
  = Hashtbl.create 1000

let core_primitives =
  Hashtbl.create 1000

let unwrap_sym_exn s =
  Util.unwrap_symbol s |> Err.get_ok

let add_core_binding : Types.Identifier.t -> unit
  = fun sym ->
    (Syntax.datum_to_syntax (Some core_syntax) (Util.make_symbol sym))
    >>= (fun stx ->
        Scope.add_binding stx
          (Binding.Core_binding sym))
    |> Err.get_ok

let add_core_form
  : Types.Identifier.t
    -> (scheme_object
        -> scheme_object Binding.MacroCompileEnv.t
        -> scheme_object Err.t)
    -> unit
  = fun sym proc ->
    add_core_binding sym;
    Hashtbl.add  core_forms sym
      (Binding.make_core_form proc)

let add_core_primitive
  : Types.Identifier.t
    -> scheme_object -> unit
  = fun sym vl ->
    add_core_binding sym;
    Hashtbl.add core_primitives sym vl

let core_form_sym s =
  match Match.try_match_syntax s (Match.of_string "(id . _)") with
  | None -> None
  | Some m ->
    (m (Util.make_symbol "id")
     >>= fun res -> Scope.resolve res
     >>= (function
         | None -> Err.error (Bad_form ("unbound", res))
         | Some b ->
           if Binding.is_core_binding b then
             Binding.core_binding_sym b |> Err.ok
           else Err.error (Bad_form ("unbound", res))))
    |> Err.to_option
