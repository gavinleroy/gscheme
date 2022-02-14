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
  let ( >>= ), ( >>| ), ( >> ) =
    Types.Err.( >>= ), Types.Err.( >>| ), Types.Err.( >> )
end

let bind_core_forms () =
  (** Introduce all of the currently available core-forms
      into the expansion environment. *)
  begin

    Core.add_core_form_bang
      "lambda"
      (fun s env ->
         Match.match_syntax s
           (Match.of_string "(lambda (id ...) body)")
         >>= fun m -> let sc = Scope.fresh () in
         m (Util.make_symbol "id")
         >>= Util.list_map (fun id -> Scope.add_scope id sc)
         >>= fun ids -> Util.list_map_m Binding.add_local_binding_bang ids
         >>= Util.list_fold (fun e key ->
             Binding.env_extend e key Binding.variable) env
         >>= fun body_env -> m (Util.make_symbol "body")
         >>= fun body_m -> Expander.expand (Scope.add_scope body_m sc) body_env
         >>= fun exp_body -> m (Util.make_symbol "lambda")
         >>| fun lambda_m -> Expander.rebuild
           s (Util.make_list [ lambda_m
                             ; ids
                             ; exp_body
                             ]));

    Core.add_core_form_bang
      "let-syntax"
      (fun s env ->
         Match.match_syntax s
           (Match.of_string "(let-syntax ((trans-id trans-rhs) ...) body)")
         >>= fun m -> let sc = Scope.fresh () in
         m (Util.make_symbol "trans-id")
         >>= Util.list_map (fun id -> Scope.add_scope id sc)
         >>= fun trans_ids -> Util.list_map_m Binding.add_local_binding_bang trans_ids
         >>= fun trans_keys -> m (Util.make_symbol "trans-rhs")
         >>= Util.list_map_m (fun rhs ->
             Expander.eval_for_syntax_binding rhs env
             >>| Box.get)
         >>= fun trans_vals -> Util.list_fold2 Binding.env_extend env trans_keys trans_vals
         >>= fun body_env -> m (Util.make_symbol "body")
         >>= fun body_m -> Expander.expand (Scope.add_scope body_m sc) body_env);

    Core.add_core_form_bang
      "#%app"
      (fun s env ->
         Match.match_syntax s (Match.of_string "(#%app rator rand ...)")
         >>= fun m -> m (Util.make_symbol "#%app")
         >>= fun app_m -> m (Util.make_symbol "rator")
         >>= fun rator_m -> Expander.expand rator_m env
         >>= fun expanded_rator -> m (Util.make_symbol "rand")
         >>= Util.list_map_m (fun rand -> Expander.expand rand env)
         >>= Lib.cons expanded_rator
         >>= Lib.cons app_m);

    Core.add_core_form_bang
      "quote"
      (fun s env ->
         Match.match_syntax s (Match.of_string "(quote datum)")
         >> Types.Err.ok s);

    Core.add_core_form_bang
      "quote-syntax"
      (fun s env ->
         Match.match_syntax s (Match.of_string "(quote-syntax datum)")
         >> Types.Err.ok s)

  end
