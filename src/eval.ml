(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module U = Util
open Types

let rec eval ?env:(e = Env.base) s =
  match s with
  | s when U.is_bool s -> ret s e
  | s when U.is_int s -> ret s e
  | s when U.is_func s -> ret s e
  | s when U.is_string s -> ret s e

  | S_obj (IdT, Id id) ->
    Env.lookup e id >>= fun v ->
    ok (v, e)

  | S_obj (ListT, List [ S_obj (IdT, Id "quote"); v]) ->
    ret v e

  | S_obj (ListT, List [ S_obj (IdT, Id "if"); cond; te; fe ]) ->
    eval ~env:e cond >>= fun (box_v, _) ->
    begin match Box.get box_v with
      | S_obj (BoolT, Bool true) -> eval ~env:e te
      | _ -> eval ~env:e fe
    end

  | S_obj (ListT, List [ S_obj (IdT, Id "set!"); S_obj (IdT, Id sym); rhs ]) ->
    eval ~env:e rhs >>= fun (ref_rhs, _) ->
    Env.lookup e sym >>= fun ref_v ->
    begin
      Box.copy_from ref_v ref_rhs;
      ret U.make_void e
    end

  | S_obj (ListT, List [ S_obj (IdT, Id "map"); func; ls ]) ->
    eval ~env:e func >>= fun (f, _) ->
    eval ~env:e ls >>= fun (ls, _) ->
    begin match Box.get ls with
      | S_obj (ListT, List ls) ->
        map_m (fun l ->
            eval ~env:e l >>= fun (l, _) ->
            apply f [l])
          ls >>= fun rs ->
        List.map Box.get rs
        |> U.make_list
        |> (fun ls -> ret ls e)
      | _ -> raise (Unexpected __LOC__)
    end

  (* define / lambda forms *)
  | S_obj (ListT, List [ S_obj (IdT, Id "define"); S_obj (IdT, Id sym); rhs]) ->
    eval ~env:e rhs >>= fun (v, _) ->
    Env.extend e sym v |> fun e ->
    ret U.make_void e

  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (ListT, List (S_obj (IdT, Id fname) :: params))
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    define_func e fname (make_fix () e params body)

  | S_obj (ListT, List (S_obj (IdT, Id "define")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    U.unwrap_id varargs >>= fun va_id ->
    define_func e fname (make_va va_id e params body)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda")
                        :: S_obj (ListT, List params)
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    ok (make_fix () e params body, e)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda")
                        :: S_obj (DottedT, Dotted
                                    (S_obj (IdT, Id fname) :: params, varargs))
                        :: body)) ->
    map_m U.unwrap_id params >>= fun params ->
    U.unwrap_id varargs >>= fun vararg ->
    ok (make_va vararg e params body, e)

  | S_obj (ListT, List (S_obj (IdT, Id "lambda") :: S_obj (IdT, Id vararg) :: body)) ->
    ok (make_va vararg e [] body, e)

  (* function application *)
  | S_obj (ListT, List (func :: args)) ->
    let open U in
    eval ~env:e func >>= fun (f, _) ->
    map_m (fun arg ->
        eval ~env:e arg >>= (ok <.> fst))
      args >>= fun args ->
    apply f args >>= fun ret_v ->
    ok (ret_v, e)

  | v -> raise (Unexpected __LOC__)

and eval_to_ref
  = fun env exp ->
    eval ~env:env exp >>= fun (dr, _) ->
    ok dr

and eval_to_value
  = fun env exp ->
    eval_to_ref env exp >>= fun box ->
    ok (Box.get box)

and define_func
  : scheme_object Box.t Env.t -> id -> scheme_object Box.t -> (scheme_object Box.t * scheme_object Box.t Env.t) maybe_exn
  = fun e name obj ->
    Env.extend e name obj
    |> ret U.make_void

and make_func
  : string option -> scheme_object Box.t Env.t -> id list -> scheme_object list -> scheme_object Box.t
  = fun va e ps bdy ->
    U.make_lambda { params = ps
                  ; varargs = va
                  ; body = bdy
                  ; closure = e
                  } |> Box.make

and make_va
  : string -> (scheme_object Box.t Env.t -> id list -> scheme_object list -> scheme_object Box.t)
  = fun va -> make_func (Some va)

and make_fix () = make_func None

and apply : scheme_object Box.t -> scheme_object Box.t list -> scheme_object Box.t maybe_exn
  = fun f args ->
    let open U in
    let unboxed_args = List.map Box.get args in
    match Box.get f with
    | f when is_proc f ->
      (f |> unwrap_proc |> get_ok) unboxed_args
      >>= fun v -> ok (Box.make v)

    | S_obj (LambT, Lamb { params; varargs; body; closure }) ->

      let len = List.length in
      let remaining = List.filteri (fun i _ ->
          len params <= i) unboxed_args
                      |> make_list |> Box.make
      in
      let bind_var_args a e = match a with
        | None -> e
        | Some name -> Env.extend e name remaining
      in
      if len args <> len params && Option.is_none varargs
      then error (Arity_mismatch (len params, len args, unboxed_args))
      else begin
        Env.extend_many closure params args
        |> bind_var_args varargs
        |> fun e ->

        (* XXX FIXME using map_m like this does not
         * update the environment between expressions
         * like it should e.g.
         *   ((lambda (x) (define y 3) (+ x y)) 2)
         *    => 5
         * NOTE this is used in several places not just here
         **)
        map_m (eval ~env:e) body
        >>= (ok <.> fst <.> List.last)
      end

    | s -> error (Type_mismatch ("procedure?", s))

and ret : scheme_object -> scheme_object Box.t Env.t -> (scheme_object Box.t * scheme_object Box.t Env.t) maybe_exn
  = fun v e -> ok ((Box.make v), e)
