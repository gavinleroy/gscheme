(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module U = Util
open Types

let rec eval ?env:(e = Namespace.base) ?kont:(kont = final_kont) s =
  let kontinue v = kont (Box.make v, e) in
  match s with
  | s when U.is_bool s -> kontinue s
  | s when U.is_number s -> kontinue s
  | s when U.is_func s -> kontinue s
  | s when U.is_string s -> kontinue s
  | s when U.is_vector s -> kontinue s

  | S_obj (IdT, Id id) ->
    Namespace.lookup e id >>= fun b ->
    kont (b, e)

  | S_obj (ListT, List ( S_obj (IdT, Id "list") :: ls )) ->
    eval_many eval ~env:e ~kont:kont ls

  | S_obj (ListT, List ( S_obj (IdT, Id "vector") :: ls )) ->
    eval_many eval ~env:e ~kont:(fun (boxed_ls, e) ->
        let v = Box.get boxed_ls
                |> U.unwrap_list_exn
                |> Vector.of_list
                |> U.make_vector
                |> Box.make
        in
        kont (v, e)) ls

  (* different quoting types *)
  | S_obj (ListT, List [ S_obj (IdT, Id "quote"); v]) ->
    kontinue v

  | S_obj (ListT, List [ S_obj (IdT, Id "quasiquote"); ls ]) ->
    eval_unquoted
      (U.make_list [ U.make_id "quote"; ls ])
      ~env:e ~kont:kont

  | S_obj (ListT, List ( S_obj (IdT, Id "unquote") :: _)) ->
    error (Bad_form ("unquote note in quasiquote", s))

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
        set_bang e sym ref_rhs >>= fun _ ->
        kontinue void)

  | S_obj (ListT, List [ S_obj (IdT, Id "map"); func; ls ]) ->
    eval func ~env:e ~kont:(fun (box_f, _) ->
        eval ls ~env:e ~kont:(fun (box_ls, _) ->
            match Box.get box_ls with
            | S_obj (ListT, List ls) ->
              begin
                eval_many eval ls ~env:e ~kont:(fun (boxed_ls, e') ->
                    let ls_vals = Box.get boxed_ls |> U.unwrap_list_exn in
                    List.map (fun l -> apply box_f [ Box.make l ]) ls_vals
                    |> (fun bs -> List.fold_right (fun mv acc ->
                        acc >>= fun accs ->
                        mv >>= fun mvu ->
                        ok (Box.get mvu :: accs)) bs (Ok []))
                    >>| U.make_list >>= (fun o ->
                        kont (Box.make o, e')))
              end
            | obj -> error (Type_mismatch ("list?", obj))))

  | S_obj (ListT, List [ S_obj (IdT, Id "define"); S_obj (IdT, Id sym); rhs]) ->
    (* reserve a heap location for sym *)
    let env = Namespace.extend e sym (Box.make void) in
    eval rhs ~env:env ~kont:(fun (box_v, _) ->
        set_bang env sym box_v |> get_ok;
        kont (Box.make void, env))

  (* define / lambda forms
   * NOTE that the 'define forms of the structure (define (foo arg1 arg2 ...) ...)
   * can be implemented as sugar for a (define foo (lambda (arg1 arg2 ...) ...))
   * This should be done once macros are at a stable state.
   ***)
  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (ListT, List (S_obj (IdT, Id fname) :: params))
                        :: body)) ->
    let env = Namespace.extend e fname (Box.make void) in
    map_m U.unwrap_id params >>= fun params ->
    set_bang env fname (make_fix () env params body) |> get_ok;
    kont (Box.make void, env)

  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    let env = Namespace.extend e fname (Box.make void) in
    map_m U.unwrap_id params >>= fun params ->
    U.unwrap_id varargs >>= fun va_id ->
    set_bang env fname (make_va va_id e params body) |> get_ok;
    kont (Box.make void, e)

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
        eval_many eval args ~env:e ~kont:(fun (box_results, e') ->
            let results = Box.get box_results
                          |> U.unwrap_list_exn
                          |> List.map Box.make in
            apply box_f results >>= fun box_v ->
            kont (box_v, e')))
  | v -> raise (Unexpected (__LOC__, void))

and eval_unquoted ?env:(e = Namespace.base) ?kont:(kont = final_kont) s =
  let open U in
  let kontinue v = kont (Box.make v, e) in
  match s with
  | S_obj (ListT, List [ S_obj (IdT, Id "unquote"); inner ]) ->
    eval ~env:e ~kont:kont inner

  | S_obj (ListT, List ls) ->
    eval_many eval_unquoted ls ~env:e ~kont:kont

  | S_obj (DottedT, Dotted (ls, last)) ->
    eval_many eval_unquoted ls ~env:e ~kont:(fun (boxed_ls, env') ->
        eval_unquoted last ~env:env' ~kont:(fun (boxed_last, env') ->
            let ls = Box.get boxed_ls |> U.unwrap_list_exn in
            kontinue (make_dotted (ls, Box.get boxed_last))))

  | other -> kontinue other

and eval_many ?env:(e = Namespace.base) ?kont:(k = final_kont) evaluator es =
  (** Evaluate the list of expressions passing the new environment to intermediate evaluations.
   *  'eval_many returns a boxed list object with the value of each expression in the input list.
   ***)
  let rec loop acc es env k =
    match es with
    | [] -> k ((U.make_list [] |> Box.make), env)
    | [e] -> evaluator e ~env:env ~kont:(fun (box_res, env') ->
        let results = ((Box.get box_res) :: acc)
                      |> List.rev |> U.make_list |> Box.make in
        k (results, env'))
    | e :: es ->
      evaluator e ~env:env ~kont:(fun (box_res, env') ->
          loop ((Box.get box_res) :: acc) es env' k)
  in loop [] es e k

and apply : scheme_object Box.t -> scheme_object Box.t list -> scheme_object Box.t maybe_exn
  = fun f args -> let open U in
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
      begin match len params, len args with
        | lps, las when lps > las ->
          error (Arity_mismatch (lps, las, unboxed_args))
        | lps, las when lps <> las && Option.is_none varargs ->
          error (Arity_mismatch (lps, las, unboxed_args))
        | lps, las ->
          begin
            Namespace.extend_many closure params args
            |> bind_var_args varargs
            |> fun e ->
            eval_many eval ~env:e body
            >>| (Box.make <.> List.last <.> U.unwrap_list_exn <.> Box.get <.> fst)
          end
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

and set_bang env sym new_ref_v =
  Namespace.lookup env sym >>| fun ref_v ->
  begin
    Box.copy_from ref_v new_ref_v;
    ()
  end

and make_fix () = make_func None

(* the "id" function *)
and final_kont = ok

(* evaluation tests *)

let%test_module _ = (module struct

  open Types
  open Util
  open Util.Test

  let eval_from_str lines =
    Parser.scheme_object_of_string lines
    >>= eval_many eval >>| (List.last <.> unwrap_list_exn <.> Box.get <.> fst)

  let eval_to_ref env exp =
    eval ~env:env exp >>= fun (dr, _) ->
    ok dr

  let eval_to_value env exp =
    eval_to_ref env exp >>= fun box ->
    ok (Box.get box)

  let%test _ = (
    let obj = (S_obj (NumT, Num (Number.Int 1L))) in
    eval_to_value Namespace.empty obj
    = Ok obj)

  let%test _ = (
    let obj = (S_obj (StringT, String "hello world")) in
    eval_to_value Namespace.empty obj
    = Ok obj)

  let%test _ = (
    expect_exn (Free_var "")
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

  (* test recursive functions *)

  let%test _ = (
    (eval_from_str "(define (fib n)
                      (if (= n 0)
                          1
                          (* n (fib (- n 1)))))
                    (fib 5) ")
    = Ok (make_int 120L))

  let%test _ = (
    (eval_from_str "(define fib (lambda (n)
                      (if (= n 0)
                          1
                          (* n (fib (- n 1))))))
                    (fib 5) ")
    = Ok (make_int 120L))

  let%test _ = (
    (eval_from_str "(vector-ref (make-vector 1 0) 0)")
    = Ok (make_int 0L))

  let%test _ = (
    (eval_from_str "(vector-ref (make-vector 10 \"hello\") 4)")
    = Ok (make_string "hello"))

  let%test _ = (
    (eval_from_str "(vector-ref (make-vector 10 'hello) 4)")
    = Ok (make_id "hello"))

  let%test _ = (
    (eval_from_str "(define vec #(1 2 'scheme #t (lambda (x) x)))
                    (vector-set! vec 4 #f)
                    (if (vector-ref vec 4) 'wrong 'correct)")
    = Ok (make_id "correct"))

end)
