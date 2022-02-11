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

let register () =
  begin
    Expand_expr.bind_core_forms ();
    Core.add_core_primitive_bang
      "syntax-e" Types.void;

    Core.add_core_primitive_bang
      "datum->syntax" Types.void;

    Core.add_core_primitive_bang
      "cons" Types.void;

    Core.add_core_primitive_bang
      "list" Types.void;

    Core.add_core_primitive_bang
      "car" Types.void;

    Core.add_core_primitive_bang
      "cdr" Types.void;

    Core.add_core_primitive_bang
      "null?" Types.void;

    Core.add_core_primitive_bang
      "map" Types.void;

  end

let namespace_syntax_introduce s =
  Scope.add_scope s Core.core_scope

let expand s =
  Expander.expand s Binding.empty_env

let eval s =
  Compile.run_time_eval s

(* provide the following externally *)

let is_identifier = Syntax.is_identifier
and syntax_e = Syntax.syntax_e
and datum_to_syntax = Syntax.datum_to_syntax
and syntax_to_datum = Syntax.syntax_to_datum
and is_eq_bound_identifier = Syntax.is_eq_bound_identifier
and compile = Compile.compile
(* and syntax_property = ... *)
