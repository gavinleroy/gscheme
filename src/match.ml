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

open Types
open Err

let is_list_len_2_or_va pattern =
  U.is_list pattern &&
  (Lib.list_length pattern >>= U.unwrap_int = Ok 2L) &&
  ((Lib.cadr pattern >>= U.unwrap_symbol = Ok "...") ||
   (Lib.cadr pattern >>= U.unwrap_symbol = Ok "...+"))

let rec to_syntax_list : scheme_object -> scheme_object
  = function
    | s when U.is_pair s ->
      (Lib.car s
       >>= fun car -> Lib.cdr s
       >>= fun cdr ->
       Lib.cons car (to_syntax_list cdr))
      |> get_ok
    | s when U.is_syntax s ->
      (Syntax.syntax_e s
       >>| to_syntax_list)
      |> get_ok
    | s -> s

let rec make_empty_vars
  = function
    | pattern when U.is_symbol pattern ->
      U.make_list [ U.make_list [ pattern; null ] ]
    | pattern when is_list_len_2_or_va pattern ->
      Lib.list_map (fun m ->
          Lib.cadr m
          >>= fun cadr -> Lib.car m
          >>= fun car ->
          Lib.cons car (U.make_list [ cadr ])) pattern
      |> get_ok

    | pattern when U.is_pair pattern ->
      (Lib.car pattern
       >>= fun car -> Lib.cdr pattern
       >>= fun cdr -> Lib.pair_append
         (make_empty_vars car)
         (make_empty_vars cdr))
      |> get_ok
    | els -> null



let match_syntax : scheme_object -> scheme_object
  (* FIXME should this wrap the function into a procedure?
   * This depends on how match_syntax is used ...
   **)
  -> (scheme_object -> scheme_object maybe_exn) maybe_exn
  = fun orig_s pattern ->
    let rec matcher s pattern =

      if U.is_symbol pattern then
        if (Str.string_match (Str.regexp "^id(:\\|$)")
              (U.unwrap_symbol pattern |> get_ok) 0
            && not (U.is_symbol s)) then
          error (Bad_form ("not an identifier", s))
        else U.make_list [ U.make_list [ pattern; s ] ] |> ok

      else if U.is_syntax s then
        Syntax.syntax_e s
        >>= (fun e -> matcher e pattern)

      else if is_list_len_2_or_va pattern then
        begin match to_syntax_list s with
          | flat_s when U.is_null flat_s ->
            if Lib.cadr pattern >>= U.unwrap_symbol = Ok "...+" then
              error (Bad_form ("bad syntax", orig_s))
            else make_empty_vars pattern |> ok
          | flat_s when U.is_list flat_s ->
            (* NOTE original code for when my implementation goes horribly wrong *)
            (* [(list? flat-s)
             *         (define a-lists
             *           (for/list ([s (in-list flat-s)])
             *             (match s (car pattern))))
             *         (apply map
             *                (lambda slice
             *                  (list (caar slice)
             *                        (map cadr slice)))
             *                a-lists)] *)
            Util.list_map_m
              (fun s -> Lib.car pattern >>= matcher s)
              flat_s
            >>= Lib.transpose
            >>= fun trsp ->
            Util.list_map_m (fun slice ->
                Lib.caar slice
                >>= fun caar -> Util.list_map_m Lib.cadr slice
                >>= fun ends ->
                Lib.list [ caar; ends ])
              trsp

          | otherwise -> error (Bad_form ("bad syntax", otherwise))
        end

      else if U.is_pair pattern then
        begin
          if U.is_pair s then
            Lib.car s
            >>= fun cars -> Lib.car pattern
            >>= fun carp -> Lib.cdr s
            >>= fun cdrs -> Lib.cdr pattern
            >>= fun cdrp -> matcher cars carp
            >>= fun firsta -> matcher cdrs cdrp
            >>= fun seconda -> Lib.pair_append firsta seconda
          else error (Bad_form ("bad syntax", orig_s))
        end
      else if U.is_null pattern then
        begin
          if U.is_null s then
            null |> ok
          else error (Bad_form ("bad syntax", orig_s))
        end

      else if ((U.is_bool pattern ||
                U.is_keyword pattern) &&
               (pattern = s)) then
        begin
          null |> ok
        end

      else
        error (Bad_form ("bad pattern", pattern))
    in
    matcher orig_s pattern
    >>| fun a_list ->
    (fun sym ->
       Lib.assq sym a_list
       >>= function
       | Some a -> Lib.cadr a
       | None -> error (Bad_form ("no such pattern variable", sym)))

let try_match_syntax
  = fun orig_s pattern ->
    match_syntax orig_s pattern
    |> to_option

let of_string : string -> scheme_object
  = fun s ->
    Parser.scheme_object_of_string s
    |> begin function
      | Error _ ->
        raise (Unexpected
                 ("internal pattern has invalid syntax : " ^ s, void))
      | Ok o -> o
    end
    |> begin function
      | [ pattern ] -> pattern
      | others ->
        raise (Unexpected
                 ("internal pattern parses to mutliple objects : " ^ s, Util.make_list others))
    end
