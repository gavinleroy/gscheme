(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module U = Util
open Types

open struct
  let ( >>= ), ( >>| ) = Err.( >>= ), Err.( >>| )
end

let rec eval ?(nmspc = Namespace.base ()) ?(kont = final_kont) s =
  let kontinue v = kont (Box.make v) in
  match s with
  | s when U.is_bool s -> kontinue s
  | s when U.is_number s -> kontinue s
  | s when U.is_func s -> kontinue s
  | s when U.is_string s -> kontinue s
  | s when U.is_vector s -> kontinue s

  | S_obj (IdT, Id id) ->
    Namespace.lookup nmspc id
    >>= kont

  (* different quoting types *)
  | S_obj (ListT, List [ S_obj (IdT, Id "quote"); v]) ->
    kontinue v

  | S_obj (ListT, List [ S_obj (IdT, Id "quasiquote"); ls ]) ->
    eval_unquoted
      (U.make_list [ U.make_id "quote"; ls ])
      ~nmspc:nmspc ~kont:kont

  | S_obj (ListT, List ( S_obj (IdT, Id "unquote") :: _)) ->
    Err.error (Bad_form ("unquote note in quasiquote", s))

  | S_obj (ListT, List [ S_obj (IdT, Id "if"); cond; te; fe ]) ->
    eval cond ~nmspc:nmspc ~kont:(fun box_v ->
        match Box.get box_v with
        | S_obj (BoolT, Bool false) ->
          (* NOTE the only false value in scheme is #f *)
          eval ~nmspc:nmspc ~kont:kont fe
        | _ ->
          eval ~nmspc:nmspc ~kont:kont te)

  | S_obj (ListT, List [ S_obj (IdT, Id "set!"); S_obj (IdT, Id sym); rhs ]) ->
    eval rhs ~nmspc:nmspc ~kont:(fun ref_rhs ->
        set_bang nmspc sym ref_rhs
        >>= fun _ ->
        kontinue void)

  | S_obj (ListT, List [ S_obj (IdT, Id "map"); func; ls ]) ->
    eval func ~nmspc:nmspc ~kont:(fun box_f ->
        eval ls ~nmspc:nmspc ~kont:(fun box_ls ->
            match Box.get box_ls with
            | S_obj (ListT, List ls) ->
              begin
                eval_many eval ls ~nmspc:nmspc ~kont:(fun boxed_ls ->
                    let ls_vals = Box.get boxed_ls |> U.unwrap_list_exn in
                    List.map (fun l -> apply (Box.get box_f) [ l ]) ls_vals
                    |> (fun bs ->
                        List.fold_right (fun mv acc ->
                            acc
                            >>= fun accs -> mv
                            >>= fun mvu ->
                            Err.ok (Box.get mvu :: accs)) bs (Ok []))
                    >>| U.make_list
                    >>= (fun o -> kont (Box.make o)))
              end
            | obj -> Err.error (Type_mismatch ("list?", obj))))

  | S_obj (ListT, List [ S_obj (IdT, Id "define"); S_obj (IdT, Id sym) ]) ->
    (* reserve a heap location for sym *)
    Namespace.extend nmspc sym (Box.make void);
    kont (Box.make void)

  | S_obj (ListT, List [ S_obj (IdT, Id "define"); S_obj (IdT, Id sym); rhs]) ->
    (* reserve a heap location for sym *)
    Namespace.extend nmspc sym (Box.make void);
    eval rhs ~nmspc:nmspc ~kont:(fun box_v ->
        set_bang nmspc sym box_v |> Err.get_ok;
        kont (Box.make void))

  (* define / lambda forms
   * NOTE that the 'define forms of the structure (define (foo arg1 arg2 ...) ...)
   * can be implemented as sugar for a (define foo (lambda (arg1 arg2 ...) ...))
   * This should be done once macros are at a stable state.
   ***)
  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (ListT, List (S_obj (IdT, Id fname) :: params))
                        :: body)) ->
    Namespace.extend nmspc fname (Box.make void);
    Err.map_m U.unwrap_id params
    >>= fun params ->
    set_bang nmspc fname (make_fix (Some fname) nmspc params body) |> Err.get_ok;
    kont (Box.make void)

  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    Namespace.extend nmspc fname (Box.make void);
    Err.map_m U.unwrap_id params
    >>= fun params -> U.unwrap_id varargs
    >>= fun va_id ->
    set_bang nmspc fname (make_va va_id (Some fname) nmspc params body) |> Err.get_ok;
    kont (Box.make void)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda")
                        :: S_obj (ListT, List params)
                        :: body)) ->
    Err.map_m U.unwrap_id params
    >>= fun params ->
    kont (make_fix None nmspc params body)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    Err.map_m U.unwrap_id params
    >>= fun params -> U.unwrap_id varargs
    >>= fun vararg ->
    kont (make_va vararg None nmspc params body)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda") :: S_obj (IdT, Id vararg) :: body)) ->
    kont (make_va vararg None nmspc [] body)

  (* function application *)
  | S_obj (ListT, List (S_obj (IdT, Id "#%app") :: func :: args))
  | S_obj (ListT, List (func :: args)) ->
    let open U in
    eval func ~nmspc:nmspc ~kont:(fun box_f ->
        eval_many eval args ~nmspc:nmspc ~kont:(fun box_results ->
            let results = Box.get box_results
                          |> U.unwrap_list_exn in
            apply (Box.get box_f) results >>= fun box_v ->
            kont box_v))
  | v -> raise (Err.Unexpected (__LOC__, void))

and eval_unquoted ?(nmspc = Namespace.base ()) ?(kont = final_kont) s =
  let open U in
  let kontinue v = kont (Box.make v) in
  match s with
  | S_obj (ListT, List [ S_obj (IdT, Id "unquote"); inner ]) ->
    eval ~nmspc:nmspc ~kont:kont inner

  | S_obj (ListT, List ls) ->
    eval_many eval_unquoted ls ~nmspc:nmspc ~kont:kont

  | S_obj (DottedT, Dotted (ls, last)) ->
    eval_many eval_unquoted ls ~nmspc:nmspc ~kont:(fun boxed_ls ->
        eval_unquoted last ~nmspc:nmspc ~kont:(fun boxed_last ->
            let ls = Box.get boxed_ls |> U.unwrap_list_exn in
            kontinue (make_dotted (ls, Box.get boxed_last))))

  | other -> kontinue other

and eval_many ?(nmspc = Namespace.base ()) ?(kont = final_kont) evaluator es =
  (** Evaluate the list of expressions passing the new environment to intermediate evaluations.
   *  'eval_many returns a boxed list object with the value of each expression in the input list.
   ***)
  let rec loop acc es env k =
    match es with
    | [] -> k (U.make_list [] |> Box.make)
    | [e] -> evaluator e ~nmspc:nmspc ~kont:(fun box_res ->
        let results = ((Box.get box_res) :: acc)
                      |> List.rev |> U.make_list |> Box.make in
        k results)
    | e :: es ->
      evaluator e ~nmspc:nmspc ~kont:(fun box_res ->
          loop ((Box.get box_res) :: acc) es nmspc k)
  in loop [] es nmspc kont

and apply : scheme_object -> scheme_object list -> scheme_object Box.t maybe_exn
  = fun f args -> let open U in
    match f with
    | f when is_procedure f ->
      (f |> unwrap_procedure |> Err.get_ok) args
      >>= fun v -> Err.ok (Box.make v)

    | S_obj (LambT, Lamb { name; params; varargs; body; closure }) ->
      let closure = Namespace.open_scope closure in
      let len = List.length in
      let remaining = List.filteri (fun i _ ->
          len params <= i) args
                      |> make_list |> Box.make
      in
      let bind_var_args a e = match a with
        | None -> e
        | Some name ->
          Namespace.extend e name remaining;
          e
      in
      begin match len params, len args with
        | lps, las when lps > las ->
          Err.error (Arity_mismatch (lps, las, args))
        | lps, las when lps <> las && Option.is_none varargs ->
          Err.error (Arity_mismatch (lps, las, args))
        | lps, las ->
          begin
            Namespace.extend_many_unboxed closure params args;
            bind_var_args varargs closure
            |> fun e -> eval_many eval ~nmspc:e body
            >>| (Box.make <.> List.last <.> U.unwrap_list_exn <.> Box.get)
          end
      end

    | s -> Err.error (Type_mismatch ("procedure?", s))

(* small helper functions *)

and make_func
  : string option
    -> string option
    -> scheme_object Box.t Namespace.t
    -> id list
    -> scheme_object list
    -> scheme_object Box.t
  = fun va nm e ps bdy ->
    U.make_lambda { name = nm
                  ; params = ps
                  ; varargs = va
                  ; body = bdy
                  ; closure = e
                  } |> Box.make

and make_va
  : string
    -> id option
    -> (scheme_object Box.t Namespace.t
        -> id list
        -> scheme_object list
        -> scheme_object Box.t)
  = fun va -> make_func (Some va)

and set_bang env sym new_ref_v =
  Namespace.lookup env sym >>| fun ref_v ->
  begin
    Box.copy_from ref_v new_ref_v;
    ()
  end

and make_fix name = make_func None name

(* the "id" kontinuation *)
and final_kont : scheme_object Box.t -> scheme_object Box.t Err.t
  = Err.ok

(* evaluation tests *)

let%test_module _ = (module struct

  open Types
  open Err
  open Util
  open Util.Test

  let eval_from_str lines =
    Parser.scheme_object_of_string lines
    >>= eval_many eval
    >>| (List.last <.> unwrap_list_exn <.> Box.get)

  let eval_to_value env exp =
    eval ~nmspc:env exp
    >>| Box.get

  let%test _ = (
    let obj = (S_obj (NumT, Num (Number.Int 1L))) in
    eval_to_value (Namespace.empty ()) obj
    = Ok obj)

  let%test _ = (
    let obj = (S_obj (StringT, String "hello world")) in
    eval_to_value (Namespace.empty ()) obj
    = Ok obj)

  let%test _ = (
    expect_exn (Free_var ("", None))
      (let obj = (S_obj (IdT, Id "atom")) in
       eval_to_value (Namespace.empty ()) obj))

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

  let%test _ = (
    (eval_from_str "(define x 0)
                    (define (foo x) (+ x 1))
                    (foo 42)
                    (foo 43)
                    (foo 44)
                    (foo 45)
                    (foo 46)
                    x")
    = Ok (make_int 0L))

end)
