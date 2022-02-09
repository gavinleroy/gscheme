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

  let foldl1 : type a. (a -> a -> a) -> a list -> a
    = fun f l ->
      match l with
      | (l :: ls) ->
        List.fold_left f l ls
      | _ -> raise (Err.Unexpected ("foldl1 received empty list", void))

  let last : type a. a t -> a
    = fun ls -> List.rev ls |> List.hd

end

let rec unwrap_int : scheme_object -> int64 Err.t
  = fun s ->
    match s with
    | S_obj (NumT, Num (Number.Int i)) -> Err.ok i
    | s -> Err.error (Type_mismatch ("int?", s))

and unwrap_num : scheme_object -> Number.t Err.t
  = fun s ->
    match s with
    | S_obj (NumT, Num n) ->  Err.ok n
    | _ -> Err.error (Type_mismatch ("number?", s))

and unwrap_bool : scheme_object -> bool Err.t
  = fun s ->
    match s with
    | S_obj (BoolT, Bool b) -> Err.ok b
    | s -> Err.error (Type_mismatch ("bool?", s))

and unwrap_id : scheme_object -> id Err.t
  = fun s ->
    match s with
    | S_obj (IdT, Id id) -> Err.ok id
    | s -> Err.error (Type_mismatch ("identifier?", s))

and unwrap_list : scheme_object -> scheme_object list Err.t
  = fun s ->
    match s with
    | S_obj (ListT, List l) -> Err.ok l
    | s -> Err.error (Type_mismatch ("list?", s))

and unwrap_procedure : scheme_object -> (scheme_object list -> scheme_object Err.t) Err.t
  = fun s ->
    match s with
    | S_obj (ProcT, Proc f) -> Err.ok f
    | _ -> Err.error (Type_mismatch ("procedure?", s))

and is_void = function
  | S_obj (VoidT, Void) -> true
  | _ -> false

and is_id = function
  | S_obj (IdT, Id _) -> true
  | _ -> false

and is_char = function
  | S_obj (CharT, Char _) -> true
  | _ -> false

and is_string = function
  | S_obj (StringT, String _) -> true
  | _ -> false

and is_bool = function
  | S_obj (BoolT, Bool _) -> true
  | _ -> false

and is_number = function
  | S_obj (NumT, Num _) -> true
  | _ -> false

and is_integer = function
  | S_obj (NumT, Num (Number.Int _)) -> true
  | _ -> false

(* a pair is a tuple or non-empty list *)
and is_pair = function
  | S_obj (DottedT, Dotted _)
  | S_obj (ListT, List (_ :: _)) -> true
  | _ -> false

and is_list = function
  | S_obj (ListT, List _) -> true
  | _ -> false

and is_vector = function
  | S_obj (VecT, Vec _) -> true
  | _ -> false

and is_null = function
  | S_obj (ListT, List []) -> true
  | _ -> false

and is_procedure = function
  | S_obj (ProcT, Proc _) -> true
  | _ -> false

and is_lambda = function
  | S_obj (LambT, Lamb _) -> true
  | _ -> false

and is_func f =
  is_procedure f || is_lambda f

and is_syntax = function
  | S_obj (StxT, Stx _) -> true
  | _ -> false

and is_not = function
  | S_obj (BoolT, Bool false) -> true
  | _ -> false

and make_bool b = S_obj (BoolT, Bool b)

and make_id id = S_obj (IdT, Id id)

and make_string s = S_obj (StringT, String s)

and make_num n = S_obj (NumT, Num n)

and make_int i = S_obj (NumT, Num (Number.Int i))

and make_list l = S_obj (ListT, List l)

and make_vector v = S_obj (VecT, Vec v)

and make_dotted p = S_obj (DottedT, Dotted p)

and make_lambda f = S_obj (LambT, Lamb f)

and make_proc f = S_obj (ProcT, Proc f)

and make_syntax s = S_obj (StxT, Stx s)

and make_port p = S_obj (PortT, Port p)

(* these should be used, and XXX_id variants deprecated *)

let is_symbol = is_id

let unwrap_symbol = unwrap_id

let unwrap_symbol_exn =
  (Err.get_ok <.> unwrap_symbol)

let make_symbol = make_id

(* NOTE in racket keword? -> true when #:... *)
let is_keyword _ = false

let or_false = Err.value ~default:false

(* unsafe operations / INTERNAL USE ONLY *)

let unwrap_list_exn =
  (Err.get_ok <.> unwrap_list)

let list_map : (scheme_object -> scheme_object) -> scheme_object -> scheme_object Err.t
  = fun f s -> match s with
    | S_obj (ListT, List ls) ->
      make_list (List.map f ls) |> Err.ok
    | obj -> Err.error (Type_mismatch ("list?", obj))

let list_map_m : (scheme_object -> scheme_object Err.t) -> scheme_object -> scheme_object Err.t
  = fun f -> function
    | S_obj (ListT, List ls) ->
      Err.map (Err.map_m f ls) (fun rs -> make_list rs)
    | obj -> Err.error (Type_mismatch ("list?", obj))

(* Formatting utilities *)

let rec format_scheme_obj
  = fun fmt s ->
    let open Format in
    let open Types in
    let rec fso fmt s = match s with
      | s when is_void s ->
        fprintf fmt "#<void>"
      | s when is_func s ->
        fprintf fmt "#<procedure>" (* TODO add name if named ... *)
      | S_obj (BoolT, Bool true) ->
        fprintf fmt "#t"
      | S_obj (BoolT, Bool false) ->
        fprintf fmt "#f"
      | S_obj (StringT, String s) ->
        fprintf fmt "\"%s\"" s
      | S_obj (IdT, Id s) ->
        fprintf fmt "%s" s
      | S_obj (NumT, Num (Number.Int i)) ->
        fprintf fmt "%Li" i
      | S_obj (StxT, Stx s) ->
        fprintf fmt "#<syntax %a>"
          fso s.e
      (* different quoted forms *)
      | S_obj (ListT, List [S_obj (IdT, Id "quote"); rhs]) ->
        fprintf fmt "'%a" fso rhs
      | S_obj (ListT, List [S_obj (IdT, Id "quasiquote"); rhs]) ->
        fprintf fmt "`%a" fso rhs
      | S_obj (ListT, List [S_obj (IdT, Id "unquote"); rhs]) ->
        fprintf fmt ",%a" fso rhs
      | S_obj (ListT, List ls) ->
        fprintf fmt "(%a)"
          (pp_print_list ~pp_sep:pp_print_space fso)
          ls
      | S_obj (VecT, Vec vector) ->
        fprintf fmt "#(%a)"
          (pp_print_list ~pp_sep:pp_print_space fso)
          (Vector.to_list vector)
      | S_obj (DottedT, Dotted (ls, tl)) ->
        fprintf fmt "(%a . %a)"
          (pp_print_list ~pp_sep:pp_print_space
             fso) ls
          fso tl
      | _ -> raise (Err.Unexpected ("fmt_scheme_object unexpected object", void))
    in fprintf fmt "%a" fso s

(* I'm not very familiar with the Format module but I believe the <2>
 * within the nested boxes are unnecessary for indentations.
 ***)
(* FIXME the XXX within the messages should represent scope e.g. a function name *)
and format_runtime_exn
  = fun fmt exc ->
    let open Format in
    let open Types in
    let ind fmt s = match s with
      | Runtime_error (msg, id) ->
        fprintf fmt "@[<v 2>%s: %s;@,%s@,error identifier: %a@]"
          "XXX" "runtime error" msg
          format_scheme_obj id

      | Arity_mismatch (expected, given, objs) ->
        fprintf fmt "@[<v 2>%s: %s;@,expected: %d@,given: %d@,args: %a@]"
          "XXX" "arity mismatch"
          expected given
          (pp_print_list ~pp_sep:pp_print_space format_scheme_obj) objs

      | Type_mismatch (contract, obj) ->
        fprintf fmt "@[<v 2>%s: %s;@,predicate: %s@,unsatisfied by: %a@]"
          "XXX" "contract violation"
          contract
          format_scheme_obj obj

      | Free_var var ->
        fprintf fmt "@[<v 2>%s: %s;@,%s@]"
          var "undefined"
          "cannot reference an identifier before its definition"

      | Bad_form (msg, obj) ->
        fprintf fmt "@[<v 2>%s:@ %s;@,%s;@,found at: %a@]"
          "XXX" "bad form" msg
          format_scheme_obj obj

      | Parser str ->
        fprintf fmt "@[<v 2>%s: %s;@]@,@[<v 2>%s@]"
          "XXX" "syntax error" str
    in fprintf fmt "@[<v 2>%a@]" ind exc

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
