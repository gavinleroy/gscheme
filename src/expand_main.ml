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
  let ( >>| ) = Types.Err.( >>| )
end

let register () =
  begin
    Expand_expr.bind_core_forms ();
    Hashtbl.iter
      (fun i b -> Core.add_core_primitive_bang i (Box.get b))
      (Namespace.base_table ());

    Core.add_core_primitive_bang
      "syntax-e"
      (Namespace.Wrappers.single_arg_procedure Syntax.syntax_e
       |> fun f -> Util.make_proc ("syntax-e", f));

    (* The current version of datum->syntax
       is not  well supported for exposing to
       clients. *)
    Core.add_core_primitive_bang
      "datum->syntax"
      (Namespace.Wrappers.double_arg_procedure
         (fun ctx v ->
            Syntax.datum_to_syntax (Some ctx) v)
       |> fun f -> Util.make_proc ("datum->syntax", f));

  end

let namespace_syntax_introduce s =
  Scope.add_scope s Core.core_scope

let expand s =
  Expander.expand s Binding.empty_env

let expand_expression e =
  Syntax.datum_to_syntax None e
  |> Types.Err.get_ok
  |> namespace_syntax_introduce
  |> expand

let eval s =
  Compile.run_time_eval s >>| Box.get

(* provide the following externally *)

let is_identifier = Syntax.is_identifier
and syntax_e = Syntax.syntax_e
and datum_to_syntax = Syntax.datum_to_syntax
and syntax_to_datum = Syntax.syntax_to_datum
and is_eq_bound_identifier = Syntax.is_eq_bound_identifier
and compile = Compile.compile
(* and syntax_property = ... *)

let%test_module _ = (module struct



end)
