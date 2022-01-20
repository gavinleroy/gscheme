(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

exception Unexpected of string

module Identifier = struct
  type t = string
end

type id = Identifier.t
type symbol = Identifier.t

type _ typ = ..
type _ data = ..
type dyn = Dyn : 'a typ * 'a -> dyn

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

module Scopes = Set.Make(Scope)

type stx = symbol * Scopes.t

(* Types allowed in gscheme *)
type _ typ += IdT : id data typ
type _ typ += ListT : dyn list data typ
type _ typ += IntT : Int64.t data typ
type _ typ += FuncT : (dyn -> dyn) data typ
(* specific to expansion *)
type _ typ += StxT : stx data typ

(* Data shapes allowed in Gscheme *)
type _ data += Id : id -> id data
type _ data += Int : Int64.t -> Int64.t data
type _ data += List : 'a list -> 'a list data
type _ data += Func : (dyn -> dyn) -> (dyn -> dyn) data
(* specific to expansion *)
type _ data += Stx : stx -> stx data

let rec datum_to_syntax : dyn -> dyn
  = fun d -> match d with
    | Dyn(IdT, Id i) ->
      Dyn (StxT, Stx (i, Scopes.empty))
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map datum_to_syntax ls))
    | _ -> d

and syntax_to_datum : dyn -> dyn
  = fun stx -> match stx with
    | Dyn(StxT, Stx (id, _)) ->
      Dyn (IdT, Id id)
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map syntax_to_datum ls))
    | _ -> stx

let fmt_dyn s =
  let foldl1 f (l :: ls) =
    List.fold_left f l ls
  in
  let rec sd = function
    | Dyn (IdT, Id s) -> s
    | Dyn (IntT, Int i) ->
      Int64.to_string i
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

type sexp =
  | SxprId of id
  | SxprInt of Int64.t
  | SxprList of sexp list

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
