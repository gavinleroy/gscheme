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

module U = Util

open struct
  include Types
  let ( >>= ), ( >>| ) = ( >>= ), ( >>| )
end

let is_list_len_2_or_va pattern =
  U.is_list pattern &&
  (Lib.list_length pattern >>= U.unwrap_int = Ok 2L) &&
  ((Lib.cadr pattern >>= U.unwrap_symbol = Ok "...") ||
   (Lib.cadr pattern >>= U.unwrap_symbol = Ok "...+"))

let rec to_syntax_list : scheme_object -> scheme_object
  = function
    | s when U.is_pair s ->
      (Lib.car s >>= fun car ->
       Lib.cdr s >>= fun cdr ->
       Lib.cons car (to_syntax_list cdr))
      |> Types.get_ok
    | s when U.is_syntax s ->
      (Syntax.syntax_e s >>| to_syntax_list)
      |> Types.get_ok
    | s -> s

let rec make_empty_vars
  = function
    | pattern when U.is_symbol pattern ->
      U.make_list [ U.make_list [ pattern; Types.null ] ]
    | pattern when is_list_len_2_or_va pattern ->
      Lib.list_map (fun m ->
          Lib.cadr m >>= fun cadr ->
          Lib.car m >>= fun car ->
          Lib.cons car (U.make_list [ cadr ])) pattern
      |> get_ok

    | pattern when U.is_pair pattern ->
      (Lib.car pattern >>= fun car ->
       Lib.cdr pattern >>= fun cdr ->
       Lib.pair_append
         (make_empty_vars car)
         (make_empty_vars cdr))
      |> get_ok
    | els -> Types.null


let match_syntax orig_s pattern =
  let rec matcher s pattern = match pattern with
    | pattern when U.is_symbol pattern ->
      if (Str.string_match (Str.regexp "^id(:\\|$)") (U.unwrap_symbol pattern |> Types.get_ok) 0
          && not (U.is_id s)
         ) then
        Types.error (Types.Bad_form ("not an identifier", s))
      else U.make_list [ U.make_list [ pattern; s ] ] |> Types.ok

    | _ when U.is_syntax s ->
      Syntax.syntax_e s >>= (fun e -> matcher e pattern)

    | pattern when is_list_len_2_or_va pattern ->
      begin match to_syntax_list s with
        | flat_s when U.is_null flat_s ->
          if Lib.cadr pattern >>= U.unwrap_symbol = Ok "...+" then
            Types.error (Types.Bad_form ("bad syntax", orig_s))
          else make_empty_vars pattern |> Types.ok
        | flat_s when U.is_list flat_s ->
          (* TODO not sure how to replicate this best *)
          (* [(list? flat-s)
           *         (define a-lists
           *           (for/list ([s (in-list flat-s)])
           *             (match s (car pattern))))
           *         (apply map
           *                (lambda slice
           *                  (list (caar slice)
           *                        (map cadr slice)))
           *                a-lists)] *)
          raise (Types.Unexpected ("TODO not implemented", orig_s))

        | otherwise -> Types.error (Types.Bad_form ("bad syntax", otherwise))
      end

    | pattern when U.is_pair pattern ->
      if U.is_pair s then
        Lib.car s >>= fun cars ->
        Lib.car pattern >>= fun carp ->
        Lib.cdr s >>= fun cdrs ->
        Lib.cdr pattern >>= fun cdrp ->
        matcher cars carp >>= fun firsta ->
        matcher cdrs cdrp >>= fun seconda ->
        Lib.pair_append firsta seconda
      else Types.error (Types.Bad_form ("bad syntax", orig_s))

    | pattern when U.is_null pattern ->
      if U.is_null s then
        Types.null |> Types.ok
      else Types.error (Types.Bad_form ("bad syntax", orig_s))

    | pattern when (U.is_bool pattern ||
                    U.is_keyword pattern) &&
                   (pattern = s) ->
      Types.null |> Types.ok

    | other -> Types.error (Types.Bad_form ("bad pattern", other))
  in
  (* FIXME should this return a procedure s_obj? *)
  (* (define a-list (match orig-s pattern))
   * (lambda (sym)
   *   (define a (assq sym a-list))
   *   (if a
   *       (cadr a)
   *       (error "no such pattern variable:" sym)))) *)
  matcher orig_s pattern >>| fun a_list ->
  (fun sym ->
     Lib.assq sym a_list >>= function
     | Some a -> Lib.cadr a
     | None -> Types.error (Types.Bad_form ("no such pattern variable", sym)))

(* #lang racket/base
 * (require "syntax.rkt")
 *
 * (provide match-syntax
 *          try-match-syntax)
 *
 * ;; A lightweight pattern matcher along the lines of `syntax-rules`.
 * ;; The result of matching is a function that takes a symbol and
 * ;; returns its match.
 * (define (match-syntax orig-s pattern
 *                       #:error [error error])
 *   (define (match s pattern)
 *     (cond
 *      [(symbol? pattern)
 *       (when (regexp-match? #rx"^id(:|$)" (symbol->string pattern))
 *         (unless (identifier? s)
 *           (error "not an identifier:" s)))
 *       (list (list pattern s))]
 *      [(syntax? s) (match (syntax-e s) pattern)]
 *      [(and (list? pattern)
 *            (= (length pattern) 2)
 *            (or (eq? '... (cadr pattern))
 *                (eq? '...+ (cadr pattern))))
 *       (define flat-s (to-syntax-list s))
 *       (cond
 *        [(null? flat-s)
 *         (when (eq? '...+ (cadr pattern))
 *           (error "bad syntax:" orig-s))
 *         (make-empty-vars pattern)]
 *        [(list? flat-s)
 *         (define a-lists
 *           (for/list ([s (in-list flat-s)])
 *             (match s (car pattern))))
 *         (apply map
 *                (lambda slice
 *                  (list (caar slice)
 *                        (map cadr slice)))
 *                a-lists)]
 *        [else (error "bad syntax:" orig-s)])
   ]
 *      [(pair? pattern)
 *       (cond
 *        [(pair? s)
 *         (append (match (car s) (car pattern))
 *                 (match (cdr s) (cdr pattern)))]
 *        [else (error "bad syntax:" orig-s)])]
 *      [(null? pattern)
 *       (cond
 *        [(null? s) null]
 *        [else (error "bad syntax:" orig-s)])]
 *      [(and (or (keyword? pattern)
 *                (boolean? pattern))
 *            (eq? pattern s))
 *       null]
 *      [else
 *       (error "bad pattern")]))
 *   (define a-list (match orig-s pattern))
 *   (lambda (sym)
 *     (define a (assq sym a-list))
 *     (if a
 *         (cadr a)
 *         (error "no such pattern variable:" sym))))
 *
 * (define (make-empty-vars pattern)
 *   (cond
 *    [(symbol? pattern)
 *     (list (list pattern null))]
 *    [(and (list? pattern)
 *          (= (length pattern) 2)
 *          (or (eq? '... (cadr pattern))
 *              (eq? '...+ (cadr pattern))))
 *     (map (lambda (m)
 *            (cons (car m) (list (cadr m))))
 *          (make-empty-vars (car pattern)))]
 *    [(pair? pattern)
 *     (append (make-empty-vars(car pattern))
 *             (make-empty-vars(cdr pattern)))]
 *    [else
 *     null]))
 *
 * (define (try-match-syntax orig-s pattern)
 *   (let/ec esc
 *     (match-syntax orig-s pattern
 *                   #:error (lambda args (esc #f)))))
 *
 * (define (to-syntax-list s)
 *   (cond
 *    [(pair? s) (cons (car s) (to-syntax-list (cdr s)))]
 *    [(syntax? s) (to-syntax-list (syntax-e s))]
 *    [else s])) *)
