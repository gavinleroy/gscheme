(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module T = Types

let foldl1 f l  =
  match l with
  | (l :: ls) ->
    List.fold_left f l ls
  | _ -> raise (T.Unexpected "foldl1 received empty list")

let rec unwrap_int : T.dyn -> int64 T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (IntT, Int i) -> T.ok i
    | s -> T.error (T.Type_mismatch ("int", s))

and unwrap_id : T.dyn -> T.id T.maybe_exn
  = fun s ->
    let open T in
    match s with
    | Dyn (IdT, Id id) -> T.ok id
    | s -> T.error (Type_mismatch ("string", s))

and dyn_of_sexpr : T.sexp -> T.dyn
  = function
    | SexpId i -> Dyn (IdT, Id i)
    | SexpInt i -> Dyn (IntT, Int i)
    | SexpBool b -> Dyn (BoolT, Bool b)
    | SexpList l -> Dyn (ListT, List (List.map dyn_of_sexpr l))

and fmt s =
  let open T in
  let rec sd = function
    | Dyn (IdT, Id s) -> s
    | Dyn (IntT, Int i) ->
      Int64.to_string i
    | Dyn (BoolT, Bool b) ->
      if b then "#t" else "#f"

    | s when is_func s ->
      "#<procedure>"

    | Dyn (StxT, Stx (e, scopes)) ->
      Printf.sprintf "#<syntax %s>" e
    | Dyn (ListT, List ls) ->
      List.map sd ls
      |> foldl1 (fun a s ->
          a ^ " " ^ s)
      |> Printf.sprintf "'(%s)"
    | _ -> raise (T.Unexpected "fmt_dyn unexpected object")
  in sd s

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

and make_id id = T.Dyn (IdT, Id id)

and make_int i = T.Dyn (IntT, Int i)

and make_list l = T.Dyn (ListT, List l)

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
