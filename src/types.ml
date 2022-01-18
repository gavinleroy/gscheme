(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module Identifier = struct
  type t = string
end

type id = Identifier.t
type symbol = Identifier.t

type _ typ = ..
type _ data = ..
type dyn = Dyn : 'a typ * 'a -> dyn

(* Types allowed in gscheme *)
type _ typ += IdT : id data typ
type _ typ += ListT : dyn list data typ
type _ typ += IntT : Int64.t data typ
type _ typ += FuncT : ('a -> 'b) data typ

(* Data shapes allowed in Gscheme *)
type _ data += Id : id -> id data
type _ data += Int : Int64.t -> Int64.t data
type _ data += List : 'a list -> 'a list data
type _ data += Func : ('a -> 'b) -> ('a -> 'b) data

type sexp =
  | SxprId of id
  | SxprInt of Int64.t
  | SxprList of sexp list

