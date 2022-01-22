(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
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
  String.contains "!#$%&|*+-/:<=>?@^_~" c

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

let parse_atom =
  let%bind stem = (letter <|> symbol) in
  let%map rest = many (letter <|> digit <|> symbol) in
  let full = stem :: rest
             |> List.to_seq
             |> String.of_seq
  in
  match full with
  | "#t" -> T.SexpBool true
  | "#f" -> T.SexpBool false
  | _ -> T.SexpId full

let parse_expr =
  fix (fun parse_expr ->
      let parse_list =
        (lex (char '(')
         *> sep_by whitespace parse_expr
         <* lex (char ')'))
        >>| fun ls -> T.SexpList ls in

      (* TODO parse_dotted_list = ... *)

      let parse_quoted =
        (char '\'' *> parse_expr)
        >>| fun e ->
        (T.SexpList [
            SexpId "quote"; e])
      in
      choice [ lex parse_atom
             ; lex parse_int
             ; lex parse_quoted
             ; lex parse_list ])

let parse_prog =
  whitespace *> parse_expr

let sexpr_of_string : string -> T.sexp T.maybe_exn
  = fun s ->
    match parse_string ~consume:Consume.All parse_prog s with
    | Ok parsed -> T.ok parsed
    | Error e -> T.error (T.Parser e)
