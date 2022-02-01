(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module U = Util
open Types

(* TODO restrict access with an interface *)

let rec eval ?env:(e = Namespace.base) ?kont:(kont = final_kont) s =
  let kontinue v = kont (Box.make v, e) in
  match s with
  | s when U.is_bool s -> kontinue s
  | s when U.is_number s -> kontinue s
  | s when U.is_func s -> kontinue s
  | s when U.is_string s -> kontinue s

  | S_obj (IdT, Id id) ->
    Namespace.lookup e id >>= fun b ->
    kont (b, e)

  (* different quoting types *)
  | S_obj (ListT, List [ S_obj (IdT, Id "quote"); v]) ->
    kontinue v

  | S_obj (ListT, List [ S_obj (IdT, Id "quasiquote"); ls ]) ->
    eval_unquoted
      (U.make_list [ U.make_id "quote"; ls ])
      ~env:e ~kont:kont

  | S_obj (ListT, List ( S_obj (IdT, Id "unquote") :: _)) ->
    (* maybe thid should be a bad form error *)
    error (Runtime_error "unquote: not in quasiquote")

  | S_obj (ListT, List [ S_obj (IdT, Id "if"); cond; te; fe ]) ->
    eval cond ~env:e ~kont:(fun (box_v, _) ->
        match Box.get box_v with
        | S_obj (BoolT, Bool false) ->
          (* NOTE the only false value in scheme is #f *)
          eval ~env:e ~kont:kont fe
        | _ ->
          eval ~env:e ~kont:kont te)


  | S_obj (ListT, List [ S_obj (IdT, Id "set!"); S_obj (IdT, Id sym); rhs ]) ->
    eval rhs ~env:e ~kont:(fun (ref_rhs, _) ->
        Namespace.lookup e sym >>= fun ref_v ->
        begin
          Box.copy_from ref_v ref_rhs;
          kontinue U.make_void
        end)

  | S_obj (ListT, List [ S_obj (IdT, Id "map"); func; ls ]) ->
    eval func ~env:e ~kont:(fun (box_f, _) ->
        eval ls ~env:e ~kont:(fun (box_ls, _) ->
            match Box.get box_ls with
            | S_obj (ListT, List ls) ->
              map_m (fun l ->
                  eval l ~env:e ~kont:(fun (l, _) ->
                      apply box_f [l] >>| fun v -> (v, e))) ls
              >>= fun rs ->
              List.map (fun (b, _) -> Box.get b) rs
              |> U.make_list
              |> (fun ls -> kontinue ls)
            | _ -> raise (Unexpected __LOC__)))


  | S_obj (ListT, List ( S_obj (IdT, Id "list") :: ls )) ->
    let open U in
    map_m (fun a ->
        eval a ~env:e
        >>= (ok <.> fst)) ls
    >>| (Box.make <.> U.make_list <.> List.map Box.get)
    >>= fun box_ls ->
    kont (box_ls, e)

  (* define / lambda forms *)
  | S_obj (ListT, List [ S_obj (IdT, Id "define"); S_obj (IdT, Id sym); rhs]) ->
    eval rhs ~env:e ~kont:(fun (box_v, _) ->
        (* XXX this is obviously simplistic but it should be
         * checked with the spec if this is correct (i.e. making a new ref)
         **)
        let new_box = Box.make_from box_v in
        Namespace.extend e sym new_box |> fun e ->
        kont (Box.make U.make_void, e))

  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (ListT, List (S_obj (IdT, Id fname) :: params))
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    Namespace.extend e fname (make_fix () e params body) |> fun e ->
    kont (Box.make U.make_void, e)

  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    U.unwrap_id varargs >>= fun va_id ->
    Namespace.extend e fname (make_va va_id e params body) |> fun e ->
    kont (Box.make U.make_void, e)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda")
                        :: S_obj (ListT, List params)
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    kont (make_fix () e params body, e)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    U.unwrap_id varargs >>= fun vararg ->
    kont (make_va vararg e params body, e)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda") :: S_obj (IdT, Id vararg) :: body)) ->
    kont (make_va vararg e [] body, e)

  (* function application *)
  | S_obj (ListT, List (S_obj (IdT, Id "#%app") :: func :: args))
  | S_obj (ListT, List (func :: args)) ->
    let open U in
    eval func ~env:e ~kont:(fun (box_f, _) ->
        map_m (fun a ->
            eval a ~env:e
            >>= (ok <.> fst)) args
        >>= fun args ->
        apply box_f args >>= fun box_v ->
        kont (box_v, e))

  | v -> raise (Unexpected __LOC__)

and eval_unquoted ?env:(e = Namespace.base) ?kont:(kont = final_kont) s =
  let open U in
  let kontinue v = kont (Box.make v, e) in
  match s with
  | S_obj (ListT, List [ S_obj (IdT, Id "unquote"); inner ]) ->
    eval ~env:e ~kont:kont inner

  | S_obj (ListT, List ls) ->
    map_m (eval_unquoted ~env:e) ls >>| (fun ls' ->
        List.map (Box.get <.> fst) ls'
        |> make_list |> Box.make) >>| fun first ->
    first, e

  | S_obj (DottedT, Dotted (ls, last)) ->
    map_m (eval_unquoted ~env:e) ls >>| (fun ls' ->
        List.map (Box.get <.> fst) ls') >>| (fun first ->
        eval_unquoted ~env:e last >>| fun (last, _) ->
        ((make_dotted (first, Box.get last) |> Box.make)
        , e)) |> join

  | other -> kontinue other

(** Evaluate the list of expressions passing the returned environment
    to the next eval *)
and thread_eval ?env:(env = Namespace.base) ?kont:(k = final_kont) es =
  match es with
  | [] -> raise (Unexpected "Calling 'thread_eval on an empty list")
  | [e] -> eval e ~env:env ~kont:k
  | e :: es ->
    (* FIXME it shouldn't be ignoring the returned result '_' *)
    eval e ~env:env ~kont:(fun (_, env') ->
        thread_eval es ~env:env' ~kont:k)

and eval_to_ref
  = fun env exp ->
    eval ~env:env exp >>= fun (dr, _) ->
    ok dr

and eval_to_value
  = fun env exp ->
    eval_to_ref env exp >>= fun box ->
    ok (Box.get box)

and apply : scheme_object Box.t -> scheme_object Box.t list -> scheme_object Box.t maybe_exn
  = fun f args ->
    let open U in
    let unboxed_args = List.map Box.get args in
    match Box.get f with
    | f when is_procedure f ->
      (f |> unwrap_procedure |> get_ok) unboxed_args >>= fun v ->
      ok (Box.make v)

    | S_obj (LambT, Lamb { params; varargs; body; closure }) ->

      let len = List.length in
      let remaining = List.filteri (fun i _ ->
          len params <= i) unboxed_args
                      |> make_list |> Box.make
      in
      let bind_var_args a e = match a with
        | None -> e
        | Some name -> Namespace.extend e name remaining
      in
      if len args <> len params && Option.is_none varargs
      then error (Arity_mismatch (len params, len args, unboxed_args))
      else begin
        Namespace.extend_many closure params args
        |> bind_var_args varargs
        |> fun e ->
        thread_eval ~env:e body
        >>= (ok <.> fst)
      end

    | s -> error (Type_mismatch ("procedure?", s))

(* small helper functions *)

and make_func
  : string option -> scheme_object Box.t Namespace.t -> id list -> scheme_object list -> scheme_object Box.t
  = fun va e ps bdy ->
    U.make_lambda { params = ps
                  ; varargs = va
                  ; body = bdy
                  ; closure = e
                  } |> Box.make

and make_va
  : string -> (scheme_object Box.t Namespace.t -> id list -> scheme_object list -> scheme_object Box.t)
  = fun va -> make_func (Some va)

and make_fix () = make_func None

(* the "id" function *)
and final_kont = ok

let eval_many (* rename out for external use *)
  = thread_eval

(* evaluation tests *)

let%test_module _ = (module struct

  open Types
  open Util
  open Util.Test

  let eval_from_str lines =
    Parser.sexpr_of_string lines
    >>| (fun sexps -> List.map scheme_object_of_sexp sexps)
    >>= thread_eval >>| (Box.get <.> fst)

  let%test _ = (
    let obj = (S_obj (NumT, Num (Number.Int 1L))) in
    eval_to_value Namespace.empty obj
    = Ok obj)

  let%test _ = (
    let obj = (S_obj (StringT, String "hello world")) in
    eval_to_value Namespace.empty obj
    = Ok obj)

  let%test _ = (
    expect_exn (Free_var ("", ""))
      (let obj = (S_obj (IdT, Id "atom")) in
       eval_to_value Namespace.empty obj))

  let%test _ = (
    eval_from_str "(if #f 1 0)"
    = Ok (make_int 0L))

  let%test _ = (
    eval_from_str "(if '() 1 0)"
    = Ok (make_int 1L))

  let%test _ = (
    eval_from_str "(if '(1 2 3) 1 0)"
    = Ok (make_int 1L))

  let%test _ = (
    eval_from_str "(if (lambda x x) 1 0)"
    = Ok (make_int 1L))

  let%test _ = (
    eval_from_str "(map (lambda (x) (* 2 x)) '(1 2 3 4))"
    = Ok (make_list [make_int 2L; make_int 4L; make_int 6L; make_int 8L]))

  let%test _ = (
    (eval_from_str "(define x 1)
                    (define y 2)
                    (define (foo z)
                            (define o x)
                            (set! x y)
                            (set! y z)
                            o)
                   (foo 3)")
    = Ok (make_int 1L))

  let%test _ = (
    (eval_from_str "(define x 1)
                        (define y 2)
                        (define (foo z)
                            (define o x)
                            (set! x y)
                            (set! y z)
                            o)
                        (foo 3)
                        x")
    = Ok (make_int 2L))

  let%test _ = (
    (eval_from_str "(define x 1)
                        (define y 2)
                        (define (foo z)
                            (define o x)
                            (set! x y)
                            (set! y z)
                            o)
                        (foo 3)
                        y")
    = Ok (make_int 3L))

  let%test _ = (
    (eval_from_str "(define (f x) (+ x 42))
                        (define (g p x) (p x))
                        (g f 23)")
    = Ok (make_int 65L))

  let%test _ = (
    (eval_from_str "`(1 2 ,(+ 1 2))")
    = Ok (make_list [ make_id "quote"
                    ; make_list [make_int 1L; make_int 2L; make_int 3L ] ]))

end)
