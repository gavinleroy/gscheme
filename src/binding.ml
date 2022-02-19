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

module S = Scope
open Types (* ^ v stop overusing the open *)
module Scope = S
open Err

module MacroCompileEnv = Map.Make(Types.Gensym)

type Types.scheme_object +=
  | Variable | Missing
  | Core_binding of Identifier.t
  | Core_form of (scheme_object
                  -> scheme_object MacroCompileEnv.t
                  -> scheme_object Err.t)

  | Local_binding of Gensym.t

let is_core_binding = function
  | Core_binding _ -> true
  | _ -> false

and core_binding_sym = function
  | Core_binding sym -> sym
  | _ -> raise (Unexpected ("binding 'core-binding-sym precondition invalidated", Types.void))

let is_local_binding = function
  | Local_binding _ -> true
  | _ -> false

and local_binding_key = function
  | Local_binding key -> key
  | _ -> raise (Unexpected ("binding local-binding-key precondition invalidated", Types.void))

and is_core_form = function
  | Core_form _ -> true
  | _ -> false

and core_form_expander = function
  | Core_form t -> t
  | _ -> raise (Unexpected ("binding core-form-expander precondition invalidated", Types.void))

and make_core_form f = Core_form f

let is_eq_free_identifier
  = fun a b ->
    let ab = Scope.resolve a |> get_ok |> Option.get
    and bb = Scope.resolve b |> get_ok |> Option.get
    in (* FIXME could the above two ever fail? *)
    match ab with
    |  ab when is_core_binding ab ->
      (is_core_binding bb) && (core_binding_sym ab = core_binding_sym bb)
    |  ab when is_local_binding ab ->
      (is_local_binding bb) && (local_binding_key ab = local_binding_key bb)
    | ab -> raise (Unexpected
                     ("the guarded expressions of 'free-identifier=? should be exhaustive"
                     , Types.void))

let add_local_binding
  = fun id ->
    begin
      if not (Syntax.is_identifier id) then
        raise (Unexpected
                 ("arg must satisfy predicate identifier?", id));
      let key = Types.Gensym.gensym ~sym:(
          Syntax.syntax_e id |> get_ok
          |> Util.unwrap_symbol |> get_ok) () in
      Scope.add_binding id (Local_binding key)
      >> ok (Util.make_symbol key)
    end

(***********************************************************************)

let empty_env =
  MacroCompileEnv.empty

let env_extend env key vl =
  if not (Util.is_symbol key) then
    raise (Unexpected ("binding-env-extend predicate symbol? broken", key))
  else MacroCompileEnv.add (Util.unwrap_symbol_exn key) vl env

let variable = Variable

let is_variable = function
  | Variable -> true
  | _ -> false

let missing = Missing

let is_missing = function
  | Missing -> true
  | _ -> false

let is_transformer = function
  | o when Util.is_procedure o -> true
  | _ -> false

(* FIXME ----
 * questionss:
 * - Wat are the possible return values?
 * - what is env, core_forms?
 **)
let binding_lookup b core_forms env id =
  if is_core_binding b then
    begin match  Hashtbl.find_opt core_forms (core_binding_sym b) with
      | None -> variable
      | Some c -> c (* S_obj (IdT, Core_form c) *)
    end
  else if is_local_binding b then
    begin match MacroCompileEnv.find_opt (local_binding_key b) env with
      | None -> raise (Unexpected
                         ("identifier used out of context", id))
      | Some v -> v
    end
  else
    raise (Unexpected
             ("internal error: unknown binding for lookup", Types.void))
