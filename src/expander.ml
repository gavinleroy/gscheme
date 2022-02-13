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
  let ( >>= ), ( >>| ) = Err.( >>= ), Err.( >>| )
end

let lookup b env id =
  Binding.binding_lookup b Core.core_forms env id

let rebuild orig new_s =
  Syntax.datum_to_syntax (Some orig) new_s

let rec expand s env =
  match s with
  | s when Syntax.is_identifier s ->
    expand_identifier s env

  | s when (Syntax.syntax_e s >>| Util.is_pair |> Util.or_false &&
            Syntax.syntax_e s >>= Lib.car >>| Syntax.is_identifier |> Util.or_false) ->
    expand_id_application_form s env

  | s when (Syntax.syntax_e s >>| Util.is_pair |> Util.or_false ||
            Syntax.syntax_e s >>| Util.is_null |> Util.or_false) ->
    expand_app s env

  | _ ->
    Util.make_list [ Syntax.datum_to_syntax
                       (Some Core.core_syntax)
                       (Util.make_symbol "quote")
                   ; s
                   ] |> rebuild s |> Err.ok

and expand_identifier s env =
  if not (Syntax.is_identifier s) then
    raise (Err.Unexpected ("'expand-identifier called with non-identifier", s));
  Scope.resolve s >>= function
  | None ->
    Err.error (Types.Free_var ("", Some s))
  | Some binding ->
    dispatch (lookup binding env s) s env

and expand_id_application_form s env =
  Syntax.syntax_e s
  >>= Lib.car
  >>= fun id -> Scope.resolve id
  >>= function
  | None ->
    expand_app s env
  | Some binding ->
    let t = lookup binding env id in
    if Binding.is_variable t then
      expand_app s env
    else dispatch t s env

and expand_app s env =
  let app = Util.make_symbol "#%app"
  and rator = Util.make_symbol "rator"
  and rand = Util.make_symbol "rand" in
  Match.match_syntax s
    (Match.of_string "(rator rand ...)")
  >>= fun m -> m rator
  >>= fun m_rator -> expand m_rator env
  >>= fun expanded -> m rand
  >>= fun m_rand ->
  Util.list_map_m (fun e -> expand e env) m_rand
  >>= (Lib.cons expanded)
  >>= (Lib.cons (Syntax.datum_to_syntax (Some Core.core_syntax) app))
  >>| (rebuild s)

and dispatch t s env =
  match t with
  | t when Binding.is_core_form t ->
    (Binding.core_form_expander t) s env
  | t when Binding.is_transformer t ->
    apply_transformer t s
    >>= fun transformed ->
    expand transformed env
  | t when Binding.is_variable t ->
    Err.ok s (* variables expand to themselves *)
  | other ->
    (* FIXME make a printer for the binding_variant so these will be displayed to the user *)
    Err.error (Types.Bad_form ("illegal use of syntax", other))

and apply_transformer func s =
  let t = Binding.unwrap_transformer_exn func in
  let intro_scope = Scope.fresh () in
  let intro_s = Scope.add_scope s intro_scope
  (* |> Util.unwrap_list |> Err.get_ok *)
  in
  t intro_s
  >>= fun transformed_s ->
  if not (Util.is_syntax transformed_s) then
    Err.error (Types.Bad_form ("transformer produced non-syntax", transformed_s))
  else Scope.flip_scope transformed_s intro_scope |> Err.ok

let expand_transformer s env =
  expand s Binding.empty_env

let eval_for_syntax_binding rhs env =
  expand_transformer rhs env
  >>= Compile.compile
  >>= Compile.expand_time_eval
