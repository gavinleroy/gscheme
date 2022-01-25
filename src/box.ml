(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

type 'a t = 'a ref

let make v =
  ref v

let get box =
  !box

let make_from bx =
  make (get bx)

let set box v =
  box := v

let copy_from box_to box_from =
  set box_to (get box_from)
