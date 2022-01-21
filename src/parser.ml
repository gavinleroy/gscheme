(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Angstrom

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
  take_while1 is_digit
  >>= fun sn ->
  return (Types.SxprInt (Int64.of_string sn))

let digit =
  satisfy is_digit

let symbol =
  satisfy is_symbol

let letter =
  satisfy is_alpha

let parse_atom =
  (letter <|> symbol)
  >>= fun stem ->
  many (letter <|> digit <|> symbol)
  >>| fun rest ->
  let full = stem :: rest
             |> List.to_seq
             |> String.of_seq
  in
  match full with
  | "#t" -> Types.SxprBool true
  | "#f" -> Types.SxprBool false
  | _ -> Types.SxprId full

let parse_expr =
  fix (fun expr ->
      let parse_list =
        lex (char '(')
        *> sep_by whitespace expr
        <* lex (char ')')
        >>| fun ls -> Types.SxprList ls
      (* TODO parse_dotted_list = ... *)
      and parse_quoted =
        (char '\'' *> expr)
        >>| fun e ->
        (Types.SxprList [
            Types.SxprId "quote"; e])
      in
      choice [ lex parse_atom
             ; lex parse_int
             ; lex parse_quoted
             ; lex parse_list ])

let parse_prog =
  whitespace *> parse_expr

let sexpr_of_string : string -> (Types.sexp, string) result
  = fun s ->
    parse_string ~consume:Consume.All parse_prog s
