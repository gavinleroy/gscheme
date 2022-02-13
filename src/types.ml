(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

(* MODULE types that will eventually be moved *)

module Char = struct
  (* chars should follow unicode standard *)
  type t = char
end

module String = struct
  (* currently restrict strings to be the
     OCaml definition *)
  include Stdlib.String
end

module Number = struct
  (* only support OCaml integer 64 numbers *)
  type t = | Int of int64

  let add lhs rhs = match lhs, rhs with
    | Int l, Int r -> Int (Int64.add l r)

  let mul lhs rhs = match lhs, rhs with
    | Int l, Int r -> Int (Int64.mul l r)

  let sub lhs rhs = match lhs, rhs with
    | Int l, Int r -> Int (Int64.sub l r)

  let equal lhs rhs = match lhs, rhs with
    | Int l, Int r -> Int64.equal l r
end

module Identifier = struct
  type t = string
end

module Gensym : sig
  type t = string
  val gensym : ?sym:string -> unit -> t
  val compare : t -> t -> int
end = struct
  include String
  type t = string
  let counter = ref 0
  let to_string v = v
  let compare a b = String.compare a b
  let gensym ?sym:(c = "g") () =
    begin
      incr counter;
      string_of_int !counter
      |> (^) c
    end
end

module Vector :sig
  type 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val length : 'a t -> int
  val make : int -> 'a -> 'a t
end = struct
  type 'a t = 'a array
  let get vec pos =
    vec.(pos)
  let set vec pos upd =
    vec.(pos) <- upd
  let of_list = Array.of_list
  let to_list = Array.to_list
  let length = Array.length
  let make = Array.make
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

type _ scheme_type = ..

type _ scheme_value = ..

type runtime_exn' =
  | Runtime_error of (string * scheme_object)
  | Arity_mismatch of (int * int * scheme_object list)
  | Type_mismatch of (string * scheme_object)
  | Free_var of (string * (scheme_object option))
  | Bad_form of (string * scheme_object)
  | Parser of string

and 'a maybe_exn = ('a, runtime_exn') Result.t

and proc_sig = Identifier.t * (scheme_object list -> scheme_object maybe_exn)

and (_, _) eq = Eq : ('a, 'a) eq

and scheme_object = S_obj : 'a scheme_type * 'a scheme_value -> scheme_object

and syntax_record = { e : scheme_object
                    ; scopes : Scopes.t
                    }

and lambda_record = { name : Identifier.t option
                    ; params : Identifier.t list
                    ; varargs : Identifier.t option
                    ; body : scheme_object list
                    ; closure : scheme_object Box.t dyn_ref_map
                    }

and 'a dyn_ref_map = (Identifier.t, 'a) Hashtbl.t list

and port =
  | WritePort of out_channel
  | ReadPort of in_channel

type _ scheme_type +=
  | VoidT : unit scheme_type
  | BoolT : bool scheme_type
  | NumT : Number.t scheme_type
  | CharT : Char.t scheme_type
  | StringT : String.t scheme_type
  | IdT : id scheme_type
  | StxT : syntax_record scheme_type
  | ListT : scheme_object list scheme_type
  | VecT : scheme_object Vector.t scheme_type
  | DottedT : (scheme_object list * scheme_object) scheme_type

  | LambT : lambda_record scheme_type
  | ProcT : proc_sig scheme_type

  | PortT : port scheme_type

type _ scheme_value +=
  | Void : unit scheme_value
  | Bool : bool -> bool scheme_value
  | Num : Number.t -> Number.t scheme_value
  | Char : Char.t -> Char.t scheme_value
  | String : String.t -> String.t scheme_value
  | Id : id -> id scheme_value
  | Stx : syntax_record -> syntax_record scheme_value
  | List : scheme_object list -> scheme_object list scheme_value
  | Vec : scheme_object Vector.t -> scheme_object Vector.t scheme_value
  | Dotted : (scheme_object list * scheme_object) -> (scheme_object list * scheme_object) scheme_value
  | Lamb : lambda_record -> lambda_record scheme_value
  | Proc : proc_sig -> proc_sig scheme_value
  | Port : port -> port scheme_value

let void = S_obj (VoidT, Void) (** The singleton void type *)

let null = S_obj (ListT, List []) (** The singleton '() *)

(* TODO other control flow exn primitives *)

module Err : sig

  (* signals an internal error FIXME remove *)
  exception Unexpected of (string * scheme_object)

  type runtime_exn = runtime_exn'

  and 'a t = 'a maybe_exn

  val ok : 'a -> 'a t
  val error : runtime_exn -> 'a t
  val get_ok : 'a t -> 'a
  val value : 'a t -> default:'a -> 'a
  val to_option : 'a t -> 'a option
  val to_bool : 'a t -> bool
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >> ) : 'a t -> 'c t -> 'c t
  val map_m : ('a -> 'b t) -> 'a list -> 'b list t

end = struct

  include Result

  type runtime_exn = runtime_exn'

  and 'a t = 'a maybe_exn

  (* signals an internal error FIXME remove *)
  exception Unexpected of (string * scheme_object)

  let error : type a. runtime_exn -> a t
    = fun e ->
      Error e

  let ( >>= ) = Result.bind
  let map = fun f s -> Result.map s f
  let ( >>| ) = map
  let ( >> ) = fun f s ->
    f >>= fun _ -> s

  let to_bool = function
    | Ok _ -> true
    | Error _ -> false

  let map_m : type a b e. (a -> (b, e) Result.t) -> a list -> (b list, e) Result.t
    = fun f ls ->
      (List.fold_left
         (fun acc  newr->
            acc >>= (fun acc_ls ->
                f newr >>= (fun v ->
                    ok (v :: acc_ls)))) (ok [])) ls
      >>= (fun l -> List.rev l |> ok)

end
