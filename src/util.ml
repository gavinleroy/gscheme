(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types

let fst : type a b. (a * b) -> a
  = fun (x, _) -> x

let snd : type a b. (a * b) -> b
  = fun (_, y) -> y

let ( <.> ) : type a b c. (b -> c) -> (a -> b) -> (a -> c)
  = fun f g -> fun a -> f ( g a )

let compose = (<.>)

module List = struct
  include List

  let remove a ls =
    List.fold_right (fun a' acc ->
        if a = a' then
          acc
        else a :: acc) ls []

  let foldl1 : type a. (a -> a -> a) -> a list -> a
    = fun f l ->
      match l with
      | (l :: ls) ->
        List.fold_left f l ls
      | _ -> raise (Err.Unexpected ("foldl1 received empty list", void))

  let last : type a. a t -> a
    = fun ls -> List.rev ls |> List.hd

end

let is_void = function
  | Void -> true
  | _ -> false

and is_symbol = function
  | Id _ -> true
  | _ -> false

and is_char = function
  | Char _ -> true
  | _ -> false

and is_string = function
  | String _ -> true
  | _ -> false

and is_bool = function
  | Bool _ -> true
  | _ -> false

and is_number = function
  | Num _ -> true
  | _ -> false

and is_integer = function
  | Num (Number.Int _) -> true
  | _ -> false

(* a pair is a tuple or non-empty list *)
and is_pair = function
  | Dotted _ | List (_ :: _) -> true
  | _ -> false

and is_list = function
  | List _ -> true
  | _ -> false

and is_vector = function
  | Vec _ -> true
  | _ -> false

and is_null = function
  | List [] -> true
  | _ -> false

and is_procedure = function
  | Proc _ -> true
  | _ -> false

and is_syntax = function
  | Stx _ -> true
  | _ -> false

and is_not = function
  | Bool false -> true
  | _ -> false

and unwrap_int : scheme_object -> int64 Err.t
  = fun s ->
    match s with
    | Num (Number.Int i) -> Err.ok i
    | s -> Err.error (Type_mismatch ("int?", s))

and unwrap_num : scheme_object -> Number.t Err.t
  = fun s ->
    match s with
    | Num n ->  Err.ok n
    | _ -> Err.error (Type_mismatch ("number?", s))

and unwrap_bool : scheme_object -> bool Err.t
  = fun s ->
    match s with
    | Bool b -> Err.ok b
    | s -> Err.error (Type_mismatch ("bool?", s))

and unwrap_symbol : scheme_object -> id Err.t
  = fun s ->
    match s with
    | Id id -> Err.ok id
    | s -> Err.error (Type_mismatch ("identifier?", s))

and unwrap_list : scheme_object -> scheme_object list Err.t
  = fun s ->
    match s with
    | List l -> Err.ok l
    | s -> Err.error (Type_mismatch ("list?", s))

and unwrap_procedure : scheme_object -> (scheme_object list -> scheme_object Err.t) Err.t
  = fun s ->
    match s with
    | Proc (_, f) -> Err.ok f
    | _ -> Err.error (Type_mismatch ("procedure?", s))

(* XXX unsafe operations *)

let unwrap_symbol_exn =
  (Err.get_ok <.> unwrap_symbol)

and unwrap_list_exn =
  (Err.get_ok <.> unwrap_list)

let make_bool b = Bool b
and make_string s = String s
and make_symbol id = Id id
and make_num n = Num n
and make_int i = Num (Number.Int i)
and make_list l = List l
and make_vector v = Vec v
and make_dotted p = Dotted p
and make_proc f = Proc f
and make_syntax s = Stx s
and make_port p = Port p

let symbol_is s sym =
  is_symbol s &&
  (unwrap_symbol_exn s) = sym

(* NOTE in racket keword? -> true when #:... *)
let is_keyword _ = false

let or_false = Err.value ~default:false

let list_map : (scheme_object -> scheme_object) -> scheme_object -> scheme_object Err.t
  = fun f s -> match s with
    | List ls ->
      make_list (List.map f ls) |> Err.ok
    | obj -> Err.error (Type_mismatch ("list?", obj))

let list_fold : type a. (a -> scheme_object -> a) -> a -> scheme_object -> a Err.t
  = fun f init s -> match s with
    | List ls ->
      List.fold_left f init ls |> Err.ok
    | obj -> Err.error (Type_mismatch ("list?", obj))

let list_fold2 : type a. (a -> scheme_object -> scheme_object -> a) -> a -> scheme_object -> scheme_object -> a Err.t
  = fun f init s1 s2 -> match s1, s2 with
    | List ls, List rs ->
      List.fold_left2 f init ls rs |> Err.ok
    | o1, o2 when is_list o2  -> Err.error (Type_mismatch ("list?", o1))
    | o1, o2 -> Err.error (Type_mismatch ("list?", o2))

let list_map_m : (scheme_object -> scheme_object Err.t) -> scheme_object -> scheme_object Err.t
  = fun f -> function
    | List ls ->
      Err.map (Err.map_m f ls) (fun rs -> make_list rs)
    | obj -> Err.error (Type_mismatch ("list?", obj))

(* Formatting utilities *)

module Test = struct

  let string_to_datum s =
    Parser.scheme_object_of_string s
    |> function
    | Ok ast ->
      (* FIXME choose a different name of fix functionality *)
      List.last ast
    | Error s ->
      raise (Err.Unexpected (__LOC__, void))

  let expect_exn : type a. Err.runtime_exn -> a Types.Err.t -> bool
  (** Check that the expected type of runtime exception was returned.
      NOTE does not check equality of exception arguments. *)
    = fun exp act -> match act with
      | Error act -> begin match exp, act with
          | Types.Runtime_error _, Types.Runtime_error _ -> true
          | Types.Arity_mismatch _, Types.Arity_mismatch _ -> true
          | Types.Type_mismatch _, Types.Type_mismatch _ -> true
          | Types.Free_var _, Types.Free_var _ -> true
          | Types.Parser _, Types.Parser _ -> true
          | _ -> false
        end
      | _ -> false

end
