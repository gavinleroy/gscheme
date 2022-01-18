(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* Hygienic Racket-like Macro Expander     *)
(* Based off Matthew Flatt's:              *)
(*  "Let's Build a Hygenic Macro Expander" *)
(*      Strange Loop 2016                  *)
(*                                         *)
(*******************************************)

(* TODO *)
module Env = Map.Make(Int)

exception Todo

let empty_env = Env.empty

let eval ?env:(e = empty_env) s =
  raise Todo
