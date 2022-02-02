(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module T = Types

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
      | _ -> raise (T.Unexpected "foldl1 received empty list")

  let last : type a. a t -> a
    = fun ls -> List.rev ls |> List.hd

end

let rec unwrap_int : T.scheme_object -> int64 T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (NumT, Num (T.Number.Int i)) -> T.ok i
    | s -> error (T.Type_mismatch ("int?", s))

and unwrap_num : T.scheme_object -> T.Number.t T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (NumT, Num n) -> T. ok n
    | _ -> error (Type_mismatch ("number?", s))

and unwrap_bool : T.scheme_object -> bool T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (BoolT, Bool b) -> T.ok b
    | s -> T.error (T.Type_mismatch ("bool?", s))

and unwrap_id : T.scheme_object -> T.id T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (IdT, Id id) -> T.ok id
    | s -> error (Type_mismatch ("identifier?", s))

and unwrap_list : T.scheme_object -> T.scheme_object list T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (ListT, List l) -> T.ok l
    | s -> error (Type_mismatch ("list?", s))

and unwrap_procedure : T.scheme_object -> (T.scheme_object list -> T.scheme_object T.maybe_exn) T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (ProcT, Proc f) -> ok f
    | _ -> error (Type_mismatch ("procedure?", s))

and is_void = function
  | T.S_obj (VoidT, Void) -> true
  | _ -> false

and is_id = function
  | T.S_obj (IdT, Id _) -> true
  | _ -> false

and is_char = function
  | T.S_obj (CharT, Char _) -> true
  | _ -> false

and is_string = function
  | T.S_obj (StringT, String _) -> true
  | _ -> false

and is_bool = function
  | T.S_obj (BoolT, Bool _) -> true
  | _ -> false

and is_number = function
  | T.S_obj (NumT, Num _) -> true
  | _ -> false

and is_integer = function
  | T.S_obj (NumT, Num (T.Number.Int _)) -> true
  | _ -> false

(* a pair is a tuple or non-empty list *)
and is_pair = function
  | T.S_obj (DottedT, Dotted _)
  | T.S_obj (ListT, List (_ :: _)) -> true
  | _ -> false

and is_list = function
  | T.S_obj (ListT, List _) -> true
  | _ -> false

and is_vector = function
  | T.S_obj (VecT, Vec _) -> true
  | _ -> false

and is_null = function
  | T.S_obj (ListT, List []) -> true
  | _ -> false

and is_procedure = function
  | T.S_obj (ProcT, Proc _) -> true
  | _ -> false

and is_lambda = function
  | T.S_obj (LambT, Lamb _) -> true
  | _ -> false

and is_func f =
  is_procedure f || is_lambda f

and is_stx = function
  | T.S_obj (StxT, Stx _) -> true
  | _ -> false

and is_not = function
  | T.S_obj (BoolT, Bool false) -> true
  | _ -> false

and make_void = T.S_obj (VoidT, Void)

and make_bool b = T.S_obj (BoolT, Bool b)

and make_id id = T.S_obj (IdT, Id id)

and make_string s = T.S_obj (StringT, String s)

and make_num n = T.S_obj (NumT, Num n)

and make_int i = T.S_obj (NumT, Num (T.Number.Int i))

and make_list l = T.S_obj (ListT, List l)

and make_dotted p = T.S_obj (DottedT, Dotted p)

and make_lambda f = T.S_obj (LambT, Lamb f)

and make_proc f = T.S_obj (ProcT, Proc f)

and make_stx s = T.S_obj (StxT, Stx s)

and make_port p = T.S_obj (PortT, Port p)

(* unsafe operations / INTERNAL USE ONLY *)

let unwrap_list_exn =
  (Types.get_ok <.> unwrap_list)

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
      | S_obj (NumT, Num (T.Number.Int i)) ->
        fprintf fmt "%Li" i
      | S_obj (StxT, Stx s) ->
        fprintf fmt "#<syntax %s>" s.e

      (* different quoted forms *)
      | S_obj (ListT, List [S_obj (IdT, Id "quote"); rhs]) ->
        fprintf fmt "'%a" fso rhs
      | S_obj (ListT, List [S_obj (IdT, Id "quasiquote"); rhs]) ->
        fprintf fmt "`%a" fso rhs
      | S_obj (ListT, List [S_obj (IdT, Id "unquote"); rhs]) ->
        fprintf fmt ",%a" fso rhs

      | S_obj (ListT, List ls) ->
        fprintf fmt "(%a)"
          (pp_print_list ~pp_sep:pp_print_space
             fso) ls
      | S_obj (DottedT, Dotted (ls, tl)) ->
        fprintf fmt "(%a . %a)"
          (pp_print_list ~pp_sep:pp_print_space
             fso) ls
          fso tl
      | _ -> raise (T.Unexpected "fmt_scheme_object unexpected object")
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
      | Runtime_error str ->
        fprintf fmt "@[<2>%s:@ %s;@]@[<2>%s@]"
          "XXX" "runtime error" str

      | Arity_mismatch (expected, given, objs) ->
        fprintf fmt "@[<2>%s:@ %s;@]@[<2>expected: %d@]@[<2>given: %d@]@[<2>args: %a@]"
          "XXX" "arity mismatch"
          expected given
          (pp_print_list ~pp_sep:pp_print_space format_scheme_obj) objs

      | Type_mismatch (contract, obj) ->
        fprintf fmt "@[<2>%s:@ %s;@]@[<2>predicate: %s@]@[<2>unsatisfied by: %a@]"
          "XXX" "contract violation"
          contract
          format_scheme_obj obj

      | Free_var var ->
        fprintf fmt "@[<2>%s:@ %s;@]@[<2>%s@]"
          var "undefined"
          "cannot reference an identifier before its definition"

      | Bad_form (msg, obj) ->
        fprintf fmt "@[<2>%s:@ %s;@]@[<2>%s;@]@[found at: %a@]"
          "XXX" "bad form" msg
          format_scheme_obj obj

      | Parser str ->
        fprintf fmt "@[<2>%s:@ %s;@]@[<2>%s@]"
          "XXX" "syntax error" str
    in fprintf fmt "@[<2>%a@]" ind exc

module Test = struct

  let string_to_datum s =
    Parser.scheme_object_of_string s
    |> function
    | Ok ast ->
      (* FIXME choose a different name of fix functionality *)
      List.last ast
    | Error s ->
      raise (T.Unexpected __LOC__)

  let expect_exn : type a. Types.runtime_exn -> a Types.maybe_exn -> bool
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
