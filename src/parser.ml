(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Angstrom
open Let_syntax
(* open Types *)
module T = Types

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let is_symbol c =
  String.contains "!$%&|*+-/:<=>?@^_~" c

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
  T.SexpInt (Int64.of_string sn)

let digit =
  satisfy is_digit

let symbol =
  satisfy is_symbol

let letter =
  satisfy is_alpha

let parse_bool =
  let%bind s = char '#' *> take 1 in
  match s with
  | "t" -> T.SexpBool true |> return
  | "f" -> T.SexpBool false |> return
  | _ -> fail "boolean values must be #t/#f"

let parse_ss_string =
  let%map str = char '"' *>
                take_while (((<>)'"'))
                <* char '"' in
  T.SexpString str

let parse_atom =
  let%bind stem = (letter <|> symbol) in
  let%map rest = many (letter <|> digit <|> symbol) in
  let full = stem :: rest
             |> List.to_seq
             |> String.of_seq
  in
  T.SexpId full

let parse_expr =
  fix (fun parse_expr ->
      let parse_list =
        lex (char '(') *> sep_by whitespace parse_expr <* lex (char ')')
        >>| fun ls -> T.SexpList ls

      and parse_dotted =
        let%bind front = lex (char '(') *> sep_by whitespace parse_expr in
        let%map last = lex (char '.') *> lex parse_expr <* char ')' in
        T.SexpDotted (front, last)

      and parse_quoted =
        (char '\'' *> parse_expr)
        >>| fun e ->
        (T.SexpList [
            SexpId "quote"; e])

      and parse_quasiquoted =
        (char '`' *> parse_expr)
        >>| fun e ->
        (T.SexpList [
            SexpId "quasiquote"; e])

      and parse_unquoted =
        (char ',' *> parse_expr)
        >>| fun e ->
        (T.SexpList [
            SexpId "unquote"; e])
      in
      choice [ lex parse_atom
             ; lex parse_ss_string
             ; lex parse_int
             ; lex parse_bool
             ; lex parse_quoted
             ; lex parse_quasiquoted
             ; lex parse_unquoted
             ; lex parse_dotted
             ; lex parse_list
             ])

let parse_prog =
  whitespace *> many1 (lex parse_expr)

let rec scheme_object_of_sexp : T.sexp -> T.scheme_object
  = function
    | SexpBool b -> Types.S_obj (BoolT, Bool b)
    | SexpInt i -> Types.S_obj (NumT, Num (Types.Number.Int i))
    | SexpId i -> Types.S_obj (IdT, Id i)
    | SexpString s -> Types.S_obj (StringT, String s)
    | SexpList l ->
      Types.S_obj (ListT, List
                     (List.map scheme_object_of_sexp l))
    | SexpDotted (hd, tl) ->
      let hd = List.map scheme_object_of_sexp hd in
      begin match scheme_object_of_sexp tl with
        | Types.S_obj (DottedT, Dotted (hd', tl)) ->
          Types.S_obj (DottedT, Dotted
                         (hd @ hd', tl))
        | Types.S_obj (ListT, List ls) ->
          Types.S_obj (ListT, List
                         (hd @ ls))
        | tl ->
          Types.S_obj (DottedT, Dotted
                         (hd, tl))
      end

let sexpr_of_string : string -> T.sexp list T.maybe_exn
  = fun s ->
    match parse_string ~consume:Consume.All parse_prog s with
    | Ok parsed -> T.ok parsed
    | Error e -> T.error (T.Parser e)

let scheme_object_of_string : string -> T.scheme_object list T.maybe_exn
  = fun s -> let open Types in
    sexpr_of_string s >>| List.map scheme_object_of_sexp

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
