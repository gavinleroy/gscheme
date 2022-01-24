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
  | SexpString of String.t
  | SexpId of id
  | SexpList of sexp list
  | SexpDotted of sexp list * sexp

type _ scheme_type =
  | VoidT : unit scheme_type
  | BoolT : bool scheme_type
  | IntT : int64 scheme_type
  | StringT : String.t scheme_type
  | IdT : id scheme_type
  | StxT : stx scheme_type
  | ListT : dyn list scheme_type
  | DottedT : (dyn list * dyn) scheme_type
  | LambT : lambda_record scheme_type
  | ProcT : (dyn list -> dyn maybe_exn) scheme_type
  | PortT : port scheme_type

and _ data =
  | Void : unit data
  | Bool : bool -> bool data
  | Int : int64 -> int64 data
  | String : String.t -> String.t data
  | Id : id -> id data
  | Stx : stx -> stx data
  | List : 'a list -> 'a list data
  | Dotted : (dyn list * dyn) -> (dyn list * dyn) data
  | Lamb : lambda_record -> lambda_record data
  | Proc : (dyn list -> dyn maybe_exn) -> (dyn list -> dyn maybe_exn) data
  | Port : port -> port data

and (_, _) eq = Eq : ('a, 'a) eq

and dyn = Dyn : 'a scheme_type * 'a data -> dyn

and lambda_record = { params : id list
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

and port =
  | WritePort of out_channel
  | ReadPort of in_channel

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
