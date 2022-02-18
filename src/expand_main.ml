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
    Hashtbl.iter
      (fun i b -> Core.add_core_primitive_bang i (Box.get b))
      (Namespace.base_table ());

    Core.add_core_primitive_bang
      "syntax-e"
      (Namespace.Wrappers.single_arg_procedure Syntax.syntax_e
       |> fun f -> Util.make_proc (Some "syntax-e", f));

    (* The current version of datum->syntax
       is not  well supported for exposing to
       clients. *)
    Core.add_core_primitive_bang
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

  open Util

  open struct
    open Types
    let ( >>= ), ( >> ) = Err.( >>= ), Err.( >> )
  end

  let _ = register ()
  let _ = Terminal.print_endline "Testing expand-main module ===>"

  let display cmp vl =
    Terminal.display_scm_obj cmp;
    Terminal.print " = ";
    Terminal.display_scm_obj_endline vl

  let expand_print_eval expr =
    (expand_expression expr
     >>= compile
     >>= fun compiled -> eval compiled
     >>| fun evaled ->
     (display compiled evaled);
     evaled) |> fun r ->
    Terminal.display_result_endline r;
    Terminal.force_newline ();
    Types.Err.ok ()

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

  let%test _ = (
    let _v = expand_print_eval
        (Test.string_to_datum
           "(lambda (x) x)") in
    true)

  let%test _ = (
    let _v =
      (expand_print_eval
         (add_let
            "(lambda (x)
               (let ((y x))
                 y))")) in
    true)

  let%test _ = (
    let _v =
      (expand_print_eval
         (add_let
            "(lambda (x)
               (let-syntax ((y (lambda (stx) (quote-syntax 9)))) y))")) in
    true)

  (*
   * (define (eval-expression e #:check [check-val #f])
   *   (define-values (c v) (compile+eval-expression e))
   *   (when check-val
   *     (unless (equal? v check-val)
   *       (error "check failed")))
   *   v)
   *
   * (compile+eval-expression
   *  (add-let
   *   '(let ([z 9])
   *     (let-syntax ([m (lambda (stx) (car (cdr (syntax-e stx))))])
   *       (let ([x 5]
   *             [y (lambda (z) z)])
   *         (let ([z 10])
   *           (list z (m 10))))))))
   *
   * "expansion not captured"
   * (eval-expression
   *  #:check 'x-1
   *  (add-let
   *   '(let ([x 'x-1])
   *     (let-syntax ([m (lambda (stx) (quote-syntax x))])
   *       (let ([x 'x-3])
   *         (m))))))
   *
   * "non-capturing expansion"
   * (eval-expression
   *  #:check 'x-3
   *  (add-let
   *   '(let ([x 'x-1])
   *     (let-syntax ([m (lambda (stx)
   *                       (datum->syntax
   *                        (quote-syntax here)
   *                        (list (quote-syntax let)
   *                              (list (list (quote-syntax x)
   *                                          (quote-syntax 'x-2)))
   *                              (car (cdr (syntax-e stx))))))])
   *       (let ([x 'x-3])
   *         (m x))))))
   *
   * "distinct generated variables"
   * #;
   * (eval-expression
   *  #:check '(2 1)
   *  '(letrec-syntaxes+values
   *    ([(gen) (lambda (stx)
   *              (let-values ([(vals) (syntax-e (car (cdr (syntax-e stx))))]
   *                           [(binds) (syntax-e (car (cdr (cdr (syntax-e stx)))))]
   *                           [(refs) (syntax-e (car (cdr (cdr (cdr (syntax-e stx))))))])
   *                (datum->syntax
   *                 #f
   *                 (if (null? vals)
   *                     (list (quote-syntax bind) binds refs)
   *                     (list (quote-syntax gen)
   *                           (cdr vals)
   *                           (cons (list (list (quote-syntax x))
   *                                       (car vals))
   *                                 binds)
   *                           (cons (quote-syntax x)
   *                                 refs))))))]
   *     [(bind) (lambda (stx)
   *               (let-values ([(binds) (car (cdr (syntax-e stx)))]
   *                            [(refs) (car (cdr (cdr (syntax-e stx))))])
   *                 (datum->syntax
   *                  (quote-syntax here)
   *                  (list (quote-syntax let-values)
   *                        binds
   *                        (cons (quote-syntax list)
   *                              refs)))))])
   *    ()
   *    (gen (1 2) () ())))
   *
   *
   * "non-transformer binding misuse"
   * (with-handlers ([exn:fail? (lambda (exn)
   *                              (unless (regexp-match? #rx"illegal use of syntax"
   *                                                     (exn-message exn))
   *                                (error "wrong error"))
   *                              'illegal-use)])
   *   (expand (namespace-syntax-introduce
   *            (datum->syntax #f
   *                           '(let-syntax ([v 1])
   *                             v))))
   *   (error "shouldn't get here")) *)

  let _ = Terminal.print_endline "<=== Done expand-main\n"

end)
