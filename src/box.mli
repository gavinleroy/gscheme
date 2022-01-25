(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

type 'a t

val make : 'a -> 'a t

val make_from : 'a t -> 'a t

val get : 'a t -> 'a

val set : 'a t -> 'a -> unit

val copy_from : 'a t -> 'a t -> unit
