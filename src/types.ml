(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

exception Unexpected of string

let foldl1 f l  =
  match l with
  | (l :: ls) ->
    List.fold_left f l ls
  | _ -> raise (Unexpected "foldl1 received empty list")

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

type sexp =
  | SxprId of id
  | SxprBool of bool
  | SxprInt of Int64.t
  | SxprList of sexp list

module Scopes = Set.Make(Scope)

type stx = symbol * Scopes.t

module Dyn = struct

  type _ typ =
    | BoolT : bool data typ
    | IntT : Int64.t data typ
    | IdT : id data typ
    | ListT : t list data typ
    | FuncT : (t list -> t) data typ
    | StxT : stx data typ

  and _ data =
    | Bool : bool -> bool data
    | Int : Int64.t -> Int64.t data
    | Id : id -> id data
    | List : 'a list -> 'a list data
    | Func : (t list -> t) -> (t list -> t) data
    (* specific to expansion *)
    | Stx : stx -> stx data

  and t = Dyn : 'a typ * 'a -> t

  type (_, _) eq = Eq : ('a, 'a) eq

  let is_id = function
    | Dyn (IdT, Id _) -> true
    | _ -> false
  let is_bool = function
    | Dyn (BoolT, Bool _) -> true
    | _ -> false
  let is_int = function
    | Dyn (IntT, Int _) -> true
    | _ -> false
  let is_list = function
    | Dyn (ListT, List _) -> true
    | _ -> false
  let is_func = function
    | Dyn (FuncT, Func _) -> true
    | _ -> false
  let is_stx = function
    | Dyn (StxT, Stx _) -> true
    | _ -> false

  let make_id id = Dyn (IdT, Id id)
  let make_int i = Dyn (IntT, Int i)
  let make_list l = Dyn (ListT, List l)
  let make_func f = Dyn (FuncT, Func f)
  let make_stx s = Dyn (StxT, Stx s)

  let rec unwrap_int = function
    | Dyn (IntT, Int i) -> i
    | s -> raise (Unexpected ("expected integer but got " ^ fmt s))
  and unwrap_id = function
    | Dyn (IdT, Id id) -> id
    | s -> raise (Unexpected ("expected identifier but got " ^ fmt s))

  and of_sexpr = function
    | SxprId i -> Dyn (IdT, Id i)
    | SxprInt i -> Dyn (IntT, Int i)
    | SxprBool b -> Dyn (BoolT, Bool b)
    | SxprList l -> Dyn (ListT, List (List.map of_sexpr l))

  and fmt s =
    let rec sd = function
      | Dyn (IdT, Id s) -> s
      | Dyn (IntT, Int i) ->
        Int64.to_string i
      | Dyn (BoolT, Bool b) ->
        if b then "#t" else "#f"
      | Dyn (FuncT, Func _) ->
        "#<procedure>"
      | Dyn (StxT, Stx (e, scopes)) ->
        Printf.sprintf "#<syntax %s>" e
      | Dyn (ListT, List ls) ->
        List.map sd ls
        |> foldl1 (fun a s ->
            a ^ " " ^ s)
        |> Printf.sprintf "'(%s)"
      | _ -> raise (Unexpected "fmt_dyn unexpected object")
    in sd s
end

let rec datum_to_syntax : Dyn.t -> Dyn.t
  = fun d -> match d with
    | Dyn(IdT, Id i) ->
      Dyn (StxT, Stx (i, Scopes.empty))
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map datum_to_syntax ls))
    | _ -> d

and syntax_to_datum : Dyn.t -> Dyn.t
  = fun stx -> match stx with
    | Dyn(StxT, Stx (id, _)) ->
      Dyn (IdT, Id id)
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map syntax_to_datum ls))
    | _ -> stx

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
