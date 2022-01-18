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

type sexp =
  | SxprId of id
  | SxprInt of Int64.t
  | SxprList of sexp list

(* currently only used in expander *)
type _ typ = ..
type _ data = ..
