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

type t = Types.binding_variant

include struct
  open Types
  let ( >> ) = ( >> )
end

let is_core_binding = function
  | `Core_binding _ -> true
  | `Core_form _ -> false
  | `Local_binding _ -> false
  | _other -> false

and core_sym = function
  | `Core_binding sym -> sym
  | _ -> raise (Types.Unexpected ("binding 'core-sym precondition invalidated", Types.void))

let is_local_binding = function
  | `Local_binding _ -> true
  | _ -> false

and local_key = function
  | `Local_binding key -> key
  | _ -> raise (Types.Unexpected ("binding local-key precondition invalidated", Types.void))

let is_eq_free_identifier
  = fun a b ->
    let ab = Scope.resolve a |> Types.get_ok |> Option.get
    and bb = Scope.resolve b |> Types.get_ok |> Option.get
    in (* FIXME could the above two ever fail? *)
    match ab with
    |  ab when is_core_binding ab ->
      (is_core_binding bb) && (core_sym ab = core_sym bb)
    |  ab when is_local_binding ab ->
      (is_local_binding bb) && (local_key ab = local_key bb)
    | ab -> raise (Types.Unexpected
                     ("the guarded expressions of 'free-identifier=? should be exhaustive"
                     , Types.void))

let add_local_binding_bang
  = fun id ->
    begin
      if not (Syntax.is_identifier id) then
        raise (Types.Unexpected
                 ("arg must satisfy predicate identifier?", id));
      let key = Types.Gensym.gensym ~sym:(
          Syntax.syntax_e id |> Types.get_ok
          |> Util.unwrap_id |> Types.get_ok) () in
      Scope.add_binding_bang id (`Local_binding key) >>
      Types.ok key
    end


(***********************************************************************)

module MacroCompileEnv = Map.Make(Types.Gensym)

let empty_env =
  MacroCompileEnv.empty

let env_extend env key vl =
  MacroCompileEnv.add key vl env

let variable =
  Types.Gensym.gensym ~sym:"variable" ()

let is_variable = function
  | `Variable v when v = variable -> true
  | _ -> false

let missing =
  Types.Gensym.gensym ~sym:"missing" ()

let is_missing = function
  | `Missing v when v = missing -> true
  | _ -> false

let is_transformer t =
  Util.is_procedure t

(* FIXME ----
 * questionss:
 * - Wat are the possible return values?
 * - what is env, core_forms?
 **)
let binding_lookup b core_forms env id =
  if is_core_binding b then
    begin match  MacroCompileEnv.find_opt (core_sym b) core_forms with
      | None -> `Variable variable
      | Some c -> `Core_form c
    end
  else if is_local_binding b then
    begin match MacroCompileEnv.find_opt (local_key b) env with
      | None -> raise (Types.Unexpected
                         ("identifier used out of context", id))
      | Some v -> v
    end
  else
    raise (Types.Unexpected
             ("internal error: unknown binding for lookup", Types.void))

(*
 * ;; A subset of compile-time values are primitive forms
 * (struct core-form (expander) #:transparent)
 *
 * ;; Returns `variable` or a compile-time value
 * (define (binding-lookup b core-forms env id)
 *   (cond
 *    [(core-binding? b)
 *     (define c (hash-ref core-forms (core-binding-sym b) #f))
 *     (if c
 *         (core-form c)
 *         variable)] ; assume a non-form reference is a primitive
 *    [(local-binding? b)
 *     (define t (hash-ref env (local-binding-key b) missing))
 *     (when (eq? t missing)
 *       (error "identifier used out of context:" id))
 *     t]
 *    [else (error "internal error: unknown binding for lookup:" b)])) *)
