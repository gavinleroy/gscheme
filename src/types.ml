(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

include Result

(* signals an internal error FIXME remove *)
exception Unexpected of string

module Identifier = struct
  type t = string
end

type id = Identifier.t
type symbol = Identifier.t

module Scope = struct
  type t = int
  let compare = Int.compare
  let count = ref 0
  let fresh () =
    begin
      incr count;
      !count
    end
end

let core_forms = [ "lambda"
                 ; "let-syntax"
                 ; "quote"
                 ; "quote-syntax" ]

let core_primitives = [ "datum->syntax"
                      ; "syntax->datum"
                      ; "syntax-e"
                      ; "list"
                      ; "cons"
                      ; "car"
                      ; "cdr"
                      ; "map" ]

type sexp =
  | SexpId of id
  | SexpBool of bool
  | SexpInt of int64
  | SexpList of sexp list

module Scopes = Set.Make(Scope)

type stx = symbol * Scopes.t

type _ typ =
  | BoolT : bool data typ
  | IntT : int64 data typ
  | IdT : id data typ
  | StxT : stx data typ
  | ListT : dyn list data typ
  (* lambdas are user defined functions *)
  | LambT : (dyn list -> dyn) data typ
  (* Procedures are primitive functions *)
  | ProcT : (dyn list -> dyn maybe_exn) data typ

and _ data =
  | Bool : bool -> bool data
  | Int : int64 -> int64 data
  | Id : id -> id data
  | Stx : stx -> stx data
  | List : 'a list -> 'a list data
  | Lamb : (dyn list -> dyn) -> (dyn list -> dyn) data
  | Proc : (dyn list -> dyn maybe_exn) -> (dyn list -> dyn maybe_exn) data

and (_, _) eq = Eq : ('a, 'a) eq

and dyn = Dyn : 'a typ * 'a -> dyn

and runtime_exn =
  | Runtime_error of string
  | Arity_mismatch of (int * dyn)
  | Type_mismatch of (string * dyn)
  | Free_var of (string * string)
  | Parser of string
  (* | Bad_form (string * dyn) *)

and 'a maybe_exn = ('a, runtime_exn) Result.t

(* module rec Exception : sig
 *
 *   type t = Hidden.runtime_exn
 *   and 'a maybe_exn = 'a Hidden.or_exn
 *
 *   val to_string : t -> string
 *   val trap_exn : 'a maybe_exn -> ('a, string) Result.t
 *   val ( >>= ) : ('a, 'e) Result.t -> ('a -> ('b, 'e) Result.t) -> ('b, 'e) Result.t
 *   val ( >>| ) : ('a -> 'b) -> ('a, 'e) Result.t -> ('b, 'e) Result.t
 *   val map_m : ('a -> ('b, 'e) Result.t) -> 'a list -> ('b list, 'e) Result.t
 *
 *   (\* FIXME there should be a better way ... from the results module *\)
 *
 *   val ok : 'a -> ('a, 'e) Stdlib.result
 *   val error : 'e -> ('a, 'e) Stdlib.result
 *   val value : ('a, 'e) Stdlib.result -> default:'a -> 'a
 *   val get_ok : ('a, 'e) Stdlib.result -> 'a
 *   val get_error : ('a, 'e) Stdlib.result -> 'e
 *   val bind :
 *     ('a, 'e) Stdlib.result ->
 *     ('a -> ('b, 'e) Stdlib.result) -> ('b, 'e) Stdlib.result
 *   val join :
 *     (('a, 'e) Stdlib.result, 'e) Stdlib.result -> ('a, 'e) Stdlib.result
 *   val map : ('a -> 'b) -> ('a, 'e) Stdlib.result -> ('b, 'e) Stdlib.result
 *   val map_error :
 *     ('e -> 'f) -> ('a, 'e) Stdlib.result -> ('a, 'f) Stdlib.result
 *   val fold :
 *     ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) Stdlib.result -> 'c
 *   val iter : ('a -> unit) -> ('a, 'e) Stdlib.result -> unit
 *   val iter_error : ('e -> unit) -> ('a, 'e) Stdlib.result -> unit
 *   val is_ok : ('a, 'e) Stdlib.result -> bool
 *   val is_error : ('a, 'e) Stdlib.result -> bool
 *   val equal :
 *     ok:('a -> 'a -> bool) ->
 *     error:('e -> 'e -> bool) ->
 *     ('a, 'e) Stdlib.result -> ('a, 'e) Stdlib.result -> bool
 *   val compare :
 *     ok:('a -> 'a -> int) ->
 *     error:('e -> 'e -> int) ->
 *     ('a, 'e) Stdlib.result -> ('a, 'e) Stdlib.result -> int
 *   val to_option : ('a, 'e) Stdlib.result -> 'a option
 *   val to_list : ('a, 'e) Stdlib.result -> 'a list
 *   val to_seq : ('a, 'e) Stdlib.result -> 'a Stdlib.Seq.t
 *
 * end = struct *)

let to_string = function
  | Runtime_error _
  | Arity_mismatch _
  | Type_mismatch _
  | Free_var _
  | Parser _ -> "TODO improve exn show"

let trap_exn : type a. a maybe_exn -> (a, string) Result.t
  = fun s -> Result.map_error to_string s

(* TODO other control flow exn primitives *)

let ( >>= ) = Result.bind
let ( >>| ) = Result.map

let map_m : type a b e. (a -> (b, e) Result.t) -> a list -> (b list, e) Result.t
  = fun f ls ->
    List.fold_right
      (fun newr acc ->
         acc >>= (fun acc_ls ->
             f newr >>= (fun v ->
                 ok (v :: acc_ls)))) ls (ok [])
(* end *)

(* and Dyn : sig
 *
 *   type t = Hidden.dyn
 *   and 'a typ = 'a Hidden.typ
 *   and 'a data = 'a Hidden.data
 *
 * end = Dyn *)
