(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
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
    | s -> T.error (T.Type_mismatch ("int", s))

and unwrap_bool : T.dyn -> bool T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (BoolT, Bool b) -> T.ok b
    | s -> T.error (T.Type_mismatch ("bool", s))

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
    | SexpId i -> make_id i
    | SexpInt i -> make_int i
    | SexpBool b -> make_bool b
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

and fmt s =
  let open T in
  let b = ref 0 in
  let rec sd = function
    | s when is_void s -> "#<void>"
    | s when is_func s ->
      "#<procedure>" (* TODO add name if named ... *)
    | Dyn (BoolT, Bool true) -> "#t"
    | Dyn (BoolT, Bool false) -> "#f"
    | Dyn (IdT, Id s) -> s
    | Dyn (IntT, Int i) ->
      Int64.to_string i
    | Dyn (StxT, Stx (e, scopes)) ->
      Printf.sprintf "#<syntax %s>" e
    | Dyn (ListT, List [Dyn (IdT, Id "quote"); rhs]) ->
      sd rhs |> Printf.sprintf "'%s"
    | Dyn (ListT, List ls) ->
      incr b;
      many ls |> Printf.sprintf "%s(%s)"
        (if !b <> 1 then "" else "'")
    | Dyn (DottedT, Dotted (ls, tl)) ->
      incr b;
      let fs = many ls
      and l = sd tl in
      Printf.sprintf "%s(%s . %s)"
        (if !b <> 1 then "" else "'")
        fs l
    | _ -> raise (T.Unexpected "fmt_dyn unexpected object")

  and many ls = List.map sd ls |> List.foldl1 (fun a s ->
      a ^ " " ^ s)
  in sd s

and is_void = function
  | T.Dyn (VoidT, Void) -> true
  | _ -> false

and is_id = function
  | T.Dyn (IdT, Id _) -> true
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

and make_int i = T.Dyn (IntT, Int i)

and make_list l = T.Dyn (ListT, List l)

and make_dotted p = T.Dyn (DottedT, Dotted p)

and make_lambda f = T.Dyn (LambT, Lamb f)

and make_proc f = T.Dyn (ProcT, Proc f)

and make_stx s = T.Dyn (StxT, Stx s)

and datum_to_syntax : T.dyn -> T.dyn
  = fun d -> match d with
    | Dyn(IdT, Id i) ->
      Dyn (StxT, Stx (i, T.Scopes.empty))
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map datum_to_syntax ls))
    | _ -> d

and syntax_to_datum : T.dyn -> T.dyn
  = fun stx -> match stx with
    | Dyn(StxT, Stx (id, _)) ->
      Dyn (IdT, Id id)
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map syntax_to_datum ls))
    | _ -> stx
