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
  let ( >>| ) = Err.( >>| )
end

let register () =
  begin
    Expand_expr.bind_core_forms ();

    (* bind core primitives *)
    Hashtbl.iter
      (fun i b -> Core.add_core_primitive i (Box.get b))
      (Namespace.base_table ());

    Core.add_core_primitive
      "syntax-e"
      (Namespace.Wrappers.single_arg_procedure Syntax.syntax_e
       |> fun f -> Util.make_proc (Some "syntax-e", f));

    (* The current version of datum->syntax
       is not  well supported for exposing to
       clients. *)
    Core.add_core_primitive
      "datum->syntax"
      (Namespace.Wrappers.double_arg_procedure
         (fun ctx v ->
            Syntax.datum_to_syntax (Some ctx) v)
       |> fun f -> Util.make_proc (Some "datum->syntax", f));

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

  module Err = Types.Err
  open Util
  open struct
    open Types
    let ( >>= ) = Err.( >>= )
  end

  let _ = register ()
  let _ = Terminal.print_endline "Testing expand-main module ===>\n"

  let expand_print_eval expr =
    (expand_expression expr
     >>= compile
     >>= fun compiled ->
     Terminal.display_scm_obj compiled;
     eval compiled
     >>| fun evaled ->
     Terminal.print " = ";
     Terminal.display_scm_obj_endline evaled;
     Terminal.force_newline ();
     evaled)

  let add_let es =
    Util.Test.string_to_datum
      "((let (lambda (stx)
             (datum->syntax
              (quote-syntax here)
              (cons
               (list (quote-syntax lambda)
                     (map (lambda (b)
                            (car (syntax-e b)))
                          (syntax-e (car (cdr (syntax-e stx)))))
                     (car (cdr (cdr (syntax-e stx)))))
               (map (lambda (b)
                      (car (cdr (syntax-e b))))
                    (syntax-e (car (cdr (syntax-e stx))))))))))"
    |> fun inner ->
    Util.make_list [ Util.make_symbol "let-syntax"
                   ; inner
                   ; Util.Test.string_to_datum es
                   ]

  let check ?(expected = None) e =
    match expected, e with
    | None, _ -> true
    | Some (Ok v), (Ok e) -> v = e
    | _, _ -> false

  let check_against exp e =
    check ~expected:(Some exp) e

  let%test _ = (
    check
      (expand_print_eval
         (Test.string_to_datum
            "(lambda (x) x)")))

  let%test _ = (
    check
      (expand_print_eval
         (add_let
            "(lambda (x)
               (let ((y x))
                 y))")))

  let%test _ = (
    check
      (expand_print_eval
         (add_let
            "(lambda (x)
               (let-syntax ((y (lambda (stx) (quote-syntax 9)))) y))")))

  let%test _ = (
    check_against
      (Test.string_to_datum "(10 10)" |> Err.ok)
      (expand_print_eval
         (add_let
            "(let ((z 9))
               (let-syntax ((m (lambda (stx) (cadr (syntax-e stx)))))
                 (let ((x 5)
                       (y (lambda (z) z)))
                   (let ((z 10))
                     (list z (m 10))))))")))

  let%test _ = (
    check_against
      (Test.string_to_datum "x-1" |> Err.ok)
      (expand_print_eval
         (add_let
            "(let ((x 'x-1))
               (let-syntax ((m (lambda (stx) (quote-syntax x))))
                 (let ((x 'x-3))
                   (m))))")))

  let%test _ = (
    check_against
      (Test.string_to_datum "x-3" |> Err.ok)
      (expand_print_eval
         (add_let
            "(let ((x 'x-1))
               (let-syntax ((m (lambda (stx)
                                 (datum->syntax
                                  (quote-syntax here)
                                  (list (quote-syntax let)
                                        (list (list (quote-syntax x)
                                                    (quote-syntax 'x-2)))
                                        (cadr (syntax-e stx)))))))
                 (let ((x 'x-3))
                   (m x))))")))

  (* FIXME include a good way to check the proper errors were thrown *)
  let%test _ = (
    check
      (expand_print_eval
         (add_let
            (* XXX THROWS illegal syntax *)
            "(let-syntax ((v 1)) v)")))

  let _ = Terminal.print_endline "<=== Done expand-main\n"

end)
