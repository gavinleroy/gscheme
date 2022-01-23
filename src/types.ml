(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
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

module Scope : sig

  type t
  val compare : t -> t -> int
  val fresh : unit -> t

end = struct

  type t = int
  let compare = Int.compare
  let count = ref 0
  let fresh () =
    begin
      incr count;
      !count
    end

end

module Scopes = Set.Make(Scope)

type stx = symbol * Scopes.t

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
  | SexpBool of bool
  | SexpInt of int64
  | SexpId of id
  | SexpList of sexp list
  | SexpDotted of sexp list * sexp

type _ typ =
  | VoidT : unit data typ
  | BoolT : bool data typ
  | IntT : int64 data typ
  | IdT : id data typ
  | StxT : stx data typ
  | ListT : dyn list data typ
  | DottedT : (dyn list * dyn) data typ
  (* lambdas are user defined functions *)
  | LambT : lambda_rec data typ
  (* Procedures are primitive functions *)
  | ProcT : (dyn list -> dyn maybe_exn) data typ

and _ data =
  | Void : unit data
  | Bool : bool -> bool data
  | Int : int64 -> int64 data
  | Id : id -> id data
  | Stx : stx -> stx data
  | List : 'a list -> 'a list data
  | Dotted : (dyn list * dyn) -> (dyn list * dyn) data
  | Lamb : lambda_rec -> lambda_rec data
  | Proc : (dyn list -> dyn maybe_exn) -> (dyn list -> dyn maybe_exn) data

and (_, _) eq = Eq : ('a, 'a) eq

and dyn = Dyn : 'a typ * 'a -> dyn

and lambda_rec = { params : id list
                 ; varargs : id option
                 ; body : dyn list
                 ; closure : dyn_ref_map
                 }

and dyn_ref_map = dyn ref Map.Make(String).t

and runtime_exn =
  | Runtime_error of string
  | Arity_mismatch of (int * int * dyn list)
  | Type_mismatch of (string * dyn)
  | Free_var of (string * string)
  | Parser of string
  (* | Bad_form (string * dyn) *)

and 'a maybe_exn = ('a, runtime_exn) Result.t

(* TODO all functions should be put in utils or a separate library *)

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
