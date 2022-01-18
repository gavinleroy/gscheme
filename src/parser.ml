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

let whitespace =
  take_while is_whitespace

let lex p =
  p <* whitespace

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_char = function
  | 'a'..'z'
  | 'A'..'Z' -> true
  | _ -> false

let parse_int =
  take_while1 is_digit
  >>= fun sn ->
  return (Types.SxprInt (Int64.of_string sn))

let parse_symbol =
  take_till (fun c -> is_whitespace c || c = ')')
  >>= fun s -> return (Types.SxprId s)

let parse_atom =
  peek_char >>= function
  | Some c when (is_digit c) -> parse_int
  | Some _ -> parse_symbol
  | None -> fail "expected character buf found EOF"

let parse_sexpr =
  fix (fun sexp ->
      let parse_list = char '(' *> sep_by whitespace sexp
                       <* char ')' <* whitespace
        >>= fun l -> return (Types.SxprList l)
      in
      choice [parse_atom; parse_list])

let parse_prog =
  whitespace *> parse_sexpr

let sexpr_of_string : string -> (Types.sexp, string) result
  = fun s ->
    parse_string ~consume:Consume.Prefix parse_sexpr s
