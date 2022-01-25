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
    | S_obj (IntT, Int i) -> T.ok i
    | s -> T.error (T.Type_mismatch ("int?", s))

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

and unwrap_proc : T.scheme_object -> (T.scheme_object list -> T.scheme_object T.maybe_exn) T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | S_obj (ProcT, Proc f) -> ok f
    | _ -> error (Type_mismatch ("procedure?", s))

and dyn_of_sexp : T.sexp -> T.scheme_object
  = function
    | SexpBool b -> make_bool b
    | SexpInt i -> make_int i
    | SexpId i -> make_id i
    | SexpString s -> make_string s
    | SexpList l ->
      make_list (List.map dyn_of_sexp l)
    | SexpDotted (hd, tl) ->
      let hd = List.map dyn_of_sexp hd in
      begin match dyn_of_sexp tl with
        | S_obj (DottedT, Dotted (hd', tl)) ->
          make_dotted (hd @ hd', tl)
        | S_obj (ListT, List ls) ->
          make_list (hd @ ls)
        | tl -> make_dotted (hd, tl)
      end

and is_void = function
  | T.S_obj (VoidT, Void) -> true
  | _ -> false

and is_id = function
  | T.S_obj (IdT, Id _) -> true
  | _ -> false

and is_string = function
  | T.S_obj (StringT, String _) -> true
  | _ -> false

and is_bool = function
  | T.S_obj (BoolT, Bool _) -> true
  | _ -> false

and is_int = function
  | T.S_obj (IntT, Int _) -> true
  | _ -> false

and is_list = function
  | T.S_obj (ListT, List _) -> true
  | _ -> false

and is_proc = function
  | T.S_obj (ProcT, Proc _) -> true
  | _ -> false

and is_lambda = function
  | T.S_obj (LambT, Lamb _) -> true
  | _ -> false

and is_func f =
  is_proc f || is_lambda f

and is_stx = function
  | T.S_obj (StxT, Stx _) -> true
  | _ -> false

and make_void = T.S_obj (VoidT, Void)

and make_bool b = T.S_obj (BoolT, Bool b)

and make_id id = T.S_obj (IdT, Id id)

and make_string s = T.S_obj (StringT, String s)

and make_int i = T.S_obj (IntT, Int i)

and make_list l = T.S_obj (ListT, List l)

and make_dotted p = T.S_obj (DottedT, Dotted p)

and make_lambda f = T.S_obj (LambT, Lamb f)

and make_proc f = T.S_obj (ProcT, Proc f)

and make_stx s = T.S_obj (StxT, Stx s)

and make_port p = T.S_obj (PortT, Port p)

and format_scheme_obj
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
      | S_obj (IntT, Int i) ->
        fprintf fmt "%Li" i
      | S_obj (StxT, Stx (e, scopes)) ->
        fprintf fmt "#<syntax %s>" e
      | S_obj (ListT, List [S_obj (IdT, Id "quote"); rhs]) ->
        fprintf fmt "'%a" fso rhs
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
    in fprintf fmt "@[%a@]" fso s

and format_runtime_exn
  = fun fmt exc ->
    let open Format in
    let open Types in
    let ind fmt s = match s with
      | Runtime_error str ->
        fprintf fmt "@[<2>%s:@ %s;@ @[<2>%s@]@]"
          "XXX" "runtime error" str
      | Arity_mismatch (expected, given, objs) ->
        fprintf fmt "@[<2>%s:@ %s;@ @[<2>expected: %d@;given: %d@;args: %a@]@]"
          "XXX" "arity mismatch"
          expected given
          (pp_print_list ~pp_sep:pp_print_space format_scheme_obj) objs
      | Type_mismatch (contract, obj) ->
        fprintf fmt "@[<2>%s:@ %s;@ @[<2>predicate: %s@;unsatisfied by: %a@]@]"
          "XXX" "contract violation"
          contract
          format_scheme_obj obj
      | Free_var (s1, s2) ->
        fprintf fmt "@[<2>%s:@ %s;@;@[%s@;%s@]@]"
          "XXX" "free variable"
          s1 s2
      | Parser str ->
        fprintf fmt "@[<2>%s:@ %s;@ @[<hov 2>%s@]@]"
          "XXX" "syntax error" str
    in fprintf fmt "@[%a@]" ind exc

module Test = struct

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
