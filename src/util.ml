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

let rec unwrap_int : T.dyn -> int64 T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (IntT, Int i) -> T.ok i
    | s -> T.error (T.Type_mismatch ("int?", s))

and unwrap_bool : T.dyn -> bool T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (BoolT, Bool b) -> T.ok b
    | s -> T.error (T.Type_mismatch ("bool?", s))

and unwrap_id : T.dyn -> T.id T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (IdT, Id id) -> T.ok id
    | s -> error (Type_mismatch ("identifier?", s))

and unwrap_proc : T.dyn -> (T.dyn list -> T.dyn T.maybe_exn) T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (ProcT, Proc f) -> ok f
    | _ -> error (Type_mismatch ("procedure?", s))

and dyn_of_sexp : T.sexp -> T.dyn
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
        | Dyn (DottedT, Dotted (hd', tl)) ->
          make_dotted (hd @ hd', tl)
        | Dyn (ListT, List ls) ->
          make_list (hd @ ls)
        | tl -> make_dotted (hd, tl)
      end

and is_void = function
  | T.Dyn (VoidT, Void) -> true
  | _ -> false

and is_id = function
  | T.Dyn (IdT, Id _) -> true
  | _ -> false

and is_string = function
  | T.Dyn (StringT, String _) -> true
  | _ -> false

and is_bool = function
  | T.Dyn (BoolT, Bool _) -> true
  | _ -> false

and is_int = function
  | T.Dyn (IntT, Int _) -> true
  | _ -> false

and is_list = function
  | T.Dyn (ListT, List _) -> true
  | _ -> false

and is_proc = function
  | T.Dyn (ProcT, Proc _) -> true
  | _ -> false

and is_lambda = function
  | T.Dyn (LambT, Lamb _) -> true
  | _ -> false

and is_func f =
  is_proc f || is_lambda f

and is_stx = function
  | T.Dyn (StxT, Stx _) -> true
  | _ -> false

and make_void = T.Dyn (VoidT, Void)

and make_bool b = T.Dyn (BoolT, Bool b)

and make_id id = T.Dyn (IdT, Id id)

and make_string s = T.Dyn (StringT, String s)

and make_int i = T.Dyn (IntT, Int i)

and make_list l = T.Dyn (ListT, List l)

and make_dotted p = T.Dyn (DottedT, Dotted p)

and make_lambda f = T.Dyn (LambT, Lamb f)

and make_proc f = T.Dyn (ProcT, Proc f)

and make_stx s = T.Dyn (StxT, Stx s)

and make_port p = T.Dyn (PortT, Port p)

and format_scheme_obj
  = fun fmt s ->
    let open Format in
    let open Types in
    let rec fso fmt s = match s with
      | s when is_void s ->
        fprintf fmt "#<void>"
      | s when is_func s ->
        fprintf fmt "#<procedure>" (* TODO add name if named ... *)
      | Dyn (BoolT, Bool true) ->
        fprintf fmt "#t"
      | Dyn (BoolT, Bool false) ->
        fprintf fmt "#f"
      | Dyn (StringT, String s) ->
        fprintf fmt "\"%s\"" s
      | Dyn (IdT, Id s) ->
        fprintf fmt "%s" s
      | Dyn (IntT, Int i) ->
        fprintf fmt "%Li" i
      | Dyn (StxT, Stx (e, scopes)) ->
        fprintf fmt "#<syntax %s>" e
      | Dyn (ListT, List [Dyn (IdT, Id "quote"); rhs]) ->
        fprintf fmt "'%a" fso rhs
      | Dyn (ListT, List ls) ->
        fprintf fmt "(%a)"
          (pp_print_list ~pp_sep:pp_print_space
             fso) ls
      | Dyn (DottedT, Dotted (ls, tl)) ->
        fprintf fmt "(%a . %a)"
          (pp_print_list ~pp_sep:pp_print_space
             fso) ls
          fso tl
      | _ -> raise (T.Unexpected "fmt_dyn unexpected object")
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
        fprintf fmt "@[<2>%s:@ %s;@ @[<2>%s@]@]"
          "XXX" "syntax error" str
    in fprintf fmt "@[%a@]" ind exc
