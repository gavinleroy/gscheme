(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

(* NOTE the parser is very minimal, and will only be expanded as
 * needed by functionality demands.
 **)

open Angstrom
open Let_syntax
open Types

type sexp =
  | SexpBool of bool
  | SexpInt of int64
  | SexpString of String.t
  | SexpId of Types.id
  | SexpHash of sexp
  | SexpList of sexp list
  | SexpDotted of sexp list * sexp

let char_list_to_string cs =
  List.to_seq cs |> String.of_seq

let first_then ft gt =
  let%bind stem = ft in
  let%bind sub = gt in
  return (stem ^ (char_list_to_string sub))

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_alpha = function
  | 'a'..'z'
  | 'A'..'Z' -> true
  | _ -> false

let whitespace =
  take_while is_whitespace

let lex p =
  p <* whitespace

let parse_int = (* TODO handle signed numbers *)
  let%map sn = take_while1 is_digit in
  SexpInt (Int64.of_string sn)

let digit =
  satisfy is_digit

let letter =
  satisfy is_alpha

let parse_bool =
  let%bind s = char '#' *> take 1 in
  match s with
  | "t" -> SexpBool true |> return
  | "f" -> SexpBool false |> return
  | _ -> fail "boolean values must be #t/#f"

let parse_ss_string =
  let%map str = char '"' *>
                take_while (((<>)'"'))
                <* char '"' in
  SexpString str



let constituent =
  choice [ letter
  (* ; any unicode char whose scalar is > 127, and whose category
     is Lu, Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po, Sc, Sm,
     Sk, So, or Co.
  *)
         ]

let special_initial =
  choice [ char '!' ; char '$' ; char '%'
         ; char '&' ; char '*' ; char '/'
         ; char ':' ; char '<' ; char '='
         ; char '>' ; char '?' ; char '^'
         ; char '_' ; char '~'
         ]

let initial =
  choice [ constituent
         ; special_initial
           (* ; inline_hex_escape *)
         ]

let special_subsequent =
  choice [ char '+' ; char '-' ; char '.' ; char '@' ]

let subsequent =
  choice [ initial
         ; digit
         (* unicode character category Nd, Mc, Me *)
         ; special_subsequent
         ]

let peculiar_identifier =
  first_then
    (choice [ string "+"; string "-"
            ; string "..."; string "->" ])
    (many subsequent)

let identifier =
  choice [ first_then
             (initial >>| String.make 1)
             (many subsequent)
         ; peculiar_identifier
         ] >>| (fun id -> SexpId id)

let parse_expr =
  fix (fun parse_expr ->
      let parse_list =
        lex (char '(') *> sep_by whitespace parse_expr <* lex (char ')')
        >>| fun ls -> SexpList ls

      and parse_dotted =
        let%bind front = lex (char '(') *> sep_by whitespace parse_expr in
        let%map last = lex (char '.') *> lex parse_expr <* char ')' in
        SexpDotted (front, last)

      and parse_quoted =
        (char '\'' *> parse_expr)
        >>| fun e ->
        (SexpList [
            SexpId "quote"; e])

      and parse_quasiquoted =
        (char '`' *> parse_expr)
        >>| fun e ->
        (SexpList [
            SexpId "quasiquote"; e])

      and parse_unquoted =
        (char ',' *> parse_expr)
        >>| fun e ->
        (SexpList [
            SexpId "unquote"; e])

      and parse_hashed =
        (char '#' *> parse_expr)
        >>| fun e ->
        (SexpHash e)

      in
      choice [ lex identifier
             ; lex parse_ss_string
             ; lex parse_int
             ; lex parse_bool
             ; lex parse_hashed
             ; lex parse_quoted
             ; lex parse_quasiquoted
             ; lex parse_unquoted
             ; lex parse_dotted
             ; lex parse_list
             ])

let parse_prog =
  whitespace *> many1 (lex parse_expr)

let rec scheme_object_of_sexp : sexp -> scheme_object
  = function
    | SexpBool b -> Bool b
    | SexpInt i -> Num (Types.Number.Int i)
    | SexpId i -> Id i
    | SexpString s -> String s
    | SexpList l ->
      List (List.map scheme_object_of_sexp l)
    | SexpDotted (hd, tl) ->
      let hd = List.map scheme_object_of_sexp hd in
      begin match scheme_object_of_sexp tl with
        | Dotted (hd', tl) ->
          Dotted (hd @ hd', tl)
        | List ls -> List (hd @ ls)
        | tl -> Dotted (hd, tl)
      end
    | SexpHash inner ->
      begin match scheme_object_of_sexp inner with
        | List (Id "quote" :: values) ->
          List (Id "quote-syntax" :: values)
        | List values ->
          Vec (Vector.of_list values)
        | Id v -> Id ("#" ^ v)
        | _ -> raise (Err.Unexpected
                        ("Hashed value not supported : " ^ __LOC__,
                         Types.void))
      end

let sexpr_of_string : string -> sexp list Err.t
  = fun s ->
    match parse_string ~consume:Consume.All parse_prog s with
    | Ok parsed -> Err.ok parsed
    | Error e -> Err.error (Parser e)

let scheme_object_of_string : string -> scheme_object list Err.t
  = fun s ->
    Err.map
      (sexpr_of_string s)
      (List.map scheme_object_of_sexp)

let%test_module "parser inline tests" = (module struct

  open Types

  let%test _ = (
    sexpr_of_string "1"
    = Ok [ SexpInt 1L ])

  let%test _ = (
    sexpr_of_string "  1   2

    3"
    = Ok [ SexpInt 1L
         ; SexpInt 2L
         ; SexpInt 3L ])

  let%test _ = (
    sexpr_of_string "'(1 2 3)"
    = Ok [ SexpList [ SexpId "quote"
                    ; SexpList [ SexpInt 1L
                               ; SexpInt 2L
                               ; SexpInt 3L ] ] ])

end)
