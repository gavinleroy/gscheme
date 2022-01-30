(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* Hygienic Racket-like Macro Expander     *)
(* Based off Matthew Flatt's:              *)
(*  "Let's Build a Hygenic Macro Expander" *)
(*      Strange Loop 2016                  *)
(*                                         *)
(*******************************************)

(* NOTE FIXME for implementation.
 * I've used a 'scheme_object' object for all function, which I dont' like.
 * This essentially has taken a good static type system from OCaml
 * and rendered it useless.
 * GOAL: improve the types defined in types.ml to reflect better scheme
 * types, allowing for safer macro expansion and evaluation.
 **)

[@@@ocaml.warning "-8"]
[@@@ocaml.warning "-11"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Types
module U = Util

let hsh_size =
  1000

module Gensym : sig
  type t = string
  val gensym : ?sym:string -> unit -> t
  val compare : t -> t -> int
end = struct
  include String
  type t = string
  let counter = ref 0
  let to_string v = v
  let compare a b = String.compare a b
  let gensym ?sym:(c = "g") () =
    begin
      incr counter;
      string_of_int !counter
      |> (^) c
    end
end

(********************************************
 ** Syntax objects *)

let rec datum_to_syntax : scheme_object -> scheme_object
  = fun d -> match d with
    | S_obj (IdT, Id i) ->
      S_obj (StxT, Stx { e = i; scopes = Scopes.empty})
    | S_obj (ListT, List ls) ->
      S_obj (ListT, List (List.map datum_to_syntax ls))
    | _ -> d

and syntax_to_datum : scheme_object -> scheme_object
  = fun stx -> match stx with
    | S_obj (StxT, Stx s) ->
      S_obj (IdT, Id s.e)
    | S_obj (ListT, List ls) ->
      S_obj (ListT, List (List.map syntax_to_datum ls))
    | _ -> stx

and is_identifier = function
  | S_obj (StxT, Stx _) -> true
  | _ -> false

let%test_module "datum->syntax->datum" = (module struct
  open Util
  open Test
  let%test _ = (datum_to_syntax (string_to_datum "a")
                = make_stx { e = "a"; scopes = Scopes.empty })
  let%test _ = (datum_to_syntax (string_to_datum "1")
                = make_int 1L)
  let%test _ = (datum_to_syntax (string_to_datum "(a b c)")
                = make_list [ make_stx { e = "a"; scopes = Scopes.empty }
                            ; make_stx { e = "b"; scopes = Scopes.empty }
                            ; make_stx { e = "c"; scopes = Scopes.empty } ])
  let%test _ = (syntax_to_datum (datum_to_syntax (string_to_datum "1"))
                = make_int 1L)
  let%test _ = (syntax_to_datum (datum_to_syntax (string_to_datum "a"))
                = make_id "a")
  let%test _ = (syntax_to_datum (datum_to_syntax (string_to_datum "(a b c)"))
                = (string_to_datum "(a b c)"))
end)

let is_bound_identifier a b =
  match a, b with
  | S_obj (StxT, Stx s1), S_obj (StxT, Stx s2) ->
    s1.e = s2.e && Scopes.equal s1.scopes s2.scopes
  | _ -> false

let%test_module "is_bound_identifier" = (module struct
  let%test _ = (
    is_bound_identifier
      (U.make_stx { e = "a"; scopes = Scopes.empty } )
      (U.make_stx { e = "a"; scopes = Scopes.empty }))
  let%test _ = (
    is_bound_identifier
      (U.make_stx { e = "a"; scopes = Scopes.empty } )
      (U.make_stx { e = "b"; scopes = Scopes.empty })
    |> not)
  let%test _ = (
    is_bound_identifier
      (U.make_stx { e = "a"; scopes = Scopes.empty } )
      (U.make_stx { e = "a"; scopes = Scopes.singleton (Scope.fresh ()) })
    |> not)
  let%test _ = (
    is_bound_identifier
      (U.make_stx { e = "a"; scopes = Scopes.singleton (Scope.fresh ()) })
      (U.make_stx { e = "a"; scopes = Scopes.empty } )
    |> not)
end)

(********************************************
 ** Scopes *)

let rec adjust_scope
  : scheme_object -> Scope.t -> (Scope.t ->  Scopes.t -> Scopes.t) -> scheme_object
  = fun s sc op -> match s with
    | S_obj (StxT, Stx s) ->
      S_obj (StxT, Stx { s with scopes = op sc s.scopes })
    | S_obj (ListT, List ls) ->
      S_obj (ListT, List (List.map (fun e ->
          adjust_scope e sc op) ls))
    | v -> v

and add_scope s sc =
  adjust_scope s sc Scopes.add

and flip_scope s sc =
  let scope_flip e s =
    if Scopes.mem e s then
      Scopes.remove e s
    else Scopes.add e s
  in
  adjust_scope s sc scope_flip

let%test_module "scope operations" = (module struct
  open Util
  open Test
  (* Scopes tests *)
  let nl = fun () -> S_obj (IdT, Id (Gensym.gensym ()))
  let sc1 = Scope.fresh ()
  let sc2 = Scope.fresh ()
  let loc_a = nl ()
  let loc_b_out = nl ()
  let loc_b_in = nl ()
  let loc_c1 = nl ()
  let loc_c2 = nl ()

  let%test _ = (sc1 = sc1)
  let%test _ = (sc1 = sc2 |> not)
  let%test _ = ((add_scope
                   (datum_to_syntax (string_to_datum "x"))
                   sc1)
                = make_stx { e = "x"; scopes = Scopes.singleton sc1 })
  let%test _ = ((add_scope (datum_to_syntax (string_to_datum "(x (y))")) sc1)
                = make_list [ make_stx { e = "x"; scopes = Scopes.singleton sc1 }
                            ; make_list [ make_stx { e = "y"; scopes = Scopes.singleton sc1 } ]])
  let%test _ = (add_scope (add_scope (datum_to_syntax (string_to_datum "x")) sc1) sc2
                = make_stx { e = "x"; scopes = Scopes.of_list [sc1; sc2] })
  let%test _ = (add_scope (add_scope (datum_to_syntax (string_to_datum "x")) sc1) sc1
                = make_stx { e = "x"; scopes = Scopes.singleton sc1 })
  let%test _ = (flip_scope (make_stx { e = "x"; scopes = Scopes.singleton sc1 }) sc2
                = (make_stx { e = "x"; scopes = Scopes.of_list [sc1; sc2] } ))
  let%test _ = (flip_scope (make_stx { e= "x"; scopes = Scopes.of_list [sc1; sc2] }) sc2
                = make_stx { e = "x"; scopes = Scopes.singleton sc1 })
end)

(********************************************
 ** Global binding table *)

exception Ambiguous_candidate_exn of string

let all_bindings =
  Hashtbl.create ~random:false hsh_size

let add_binding (id : syntax) binding =
  Hashtbl.add all_bindings id binding

let rec resolve (* : TODO add type *)
  = fun s -> match s with
    | S_obj (StxT, Stx id) ->
      begin match find_all_matching_bindings id with
        | [] -> None
        | candidate_ids ->
          let max_id = argmax (fun s ->
              (Scopes.cardinal s.scopes)) candidate_ids
          in
          check_unambiguous max_id candidate_ids;
          Some (Hashtbl.find all_bindings max_id)
      end
    | v -> raise (Unexpected __LOC__)

and argmax f xs = match xs with
  | [] -> raise (Unexpected __LOC__)
  | x :: xs ->
    List.fold_left (fun acc b ->
        if f acc > f b then
          acc
        else b) x xs

and find_all_matching_bindings stx : syntax list =
  Hashtbl.fold (fun stx' _ acc ->
      if stx.e = stx'.e && Scopes.subset stx'.scopes stx.scopes
      then stx' :: acc
      else acc) all_bindings []

and check_unambiguous s =
  List.iter (fun s' ->
      if not (Scopes.subset s'.scopes s.scopes) then
        raise (Ambiguous_candidate_exn
                 "some usefull message :)"))

and is_free_identifier a b =
  resolve a = resolve b

let%test_module _ = (module struct
  open Util
  open Test
  (* FIXME is there a way to let these bindings carry over between modules *)
  let sc1 = Scope.fresh ()
  let sc2 = Scope.fresh ()
  let loc_a = make_id (Gensym.gensym ())
  let loc_b_out = make_id (Gensym.gensym ())
  let loc_b_in = make_id (Gensym.gensym ())
  let loc_c1 = make_id (Gensym.gensym ())
  let loc_c2 = make_id (Gensym.gensym ())
  let a = { e = "a"; scopes = Scopes.singleton sc1 }
  let b_out = { e = "b"; scopes = Scopes.singleton sc1 }
  let b_in = { e = "b"; scopes = Scopes.of_list [sc1; sc2] }
  let c1 = { e = "c"; scopes = Scopes.singleton sc1 }
  let c2 = { e = "c"; scopes = Scopes.singleton sc2 }
  let _ = add_binding a loc_a
  let _ = add_binding b_out loc_b_out
  let _ = add_binding b_in loc_b_in
  let _ = add_binding c1 loc_c1
  let _ = add_binding c2 loc_c2

  let%test _ = (resolve (make_stx a)
                = Some loc_a)
  let%test _ = (resolve (make_stx { e = "a"; scopes = Scopes.of_list [sc1; sc2] })
                = Some loc_a)
  let%test _ = (resolve (make_stx { e = "b"; scopes = Scopes.of_list [sc2] })
                = None)
  let%test _ = (resolve (make_stx { e = "a"; scopes = Scopes.of_list [sc2] })
                = None)
  let%test _ = (resolve (make_stx { e = "b"; scopes = Scopes.of_list [sc1] })
                = Some loc_b_out)
  let%test _ = (resolve (make_stx { e = "b"; scopes = Scopes.of_list [sc1; sc2] })
                = Some loc_b_in)
  let%test _ = (find_all_matching_bindings a
                = [a])
  let%test _ = (try ignore(resolve (make_stx { e = "c"; scopes = Scopes.of_list [sc1; sc2] }));
                  false
                with Ambiguous_candidate_exn _ -> true)
  let%test _ = (find_all_matching_bindings { e = "a"; scopes = Scopes.singleton sc2 }
                = [])
  let%test _ = (is_free_identifier (make_stx { e = "a"; scopes = Scopes.of_list [ sc1 ] })
                  (make_stx { e = "a"; scopes = Scopes.of_list [ sc1; sc2 ] }))
  let%test _ = (is_free_identifier (make_stx b_out)
                  (make_stx b_in)
                |> not)

  module S = Set.Make(struct
      type t = syntax
      let compare s s' =
        if s.e = s'.e then
          Scopes.compare s.scopes s'.scopes
        else String.compare s.e s'.e end)

  let%test _ = (let open S in
                equal (find_all_matching_bindings b_in |> of_list)
                  (of_list [b_in; b_out]))
  let%test _ = (let open S in
                equal (find_all_matching_bindings
                         { e = "c"; scopes = Scopes.of_list [sc1; sc2] }
                       |> of_list)
                  (of_list [c1; c2]))
  let%test _ = (check_unambiguous b_in [b_out; b_in]
                = ())
  let%test _ = (try check_unambiguous c2 [c1; c2];
                  false
                with Ambiguous_candidate_exn _ ->
                  true)
end)

(********************************************
 ** Core syntax and primitives *)

module CoreIDSet = Set.Make(String)

let core_scope = Scope.fresh ()

let core_forms =
  CoreIDSet.of_list core_forms

let core_primitives =
  CoreIDSet.of_list core_primitives

let bind_core_forms_primitives
  = fun () ->
    CoreIDSet.union core_forms core_primitives
    |> CoreIDSet.iter (fun str ->
        add_binding
          { e = str; scopes = Scopes.singleton core_scope }
          (U.make_id str))

let namespace_syntax_introduce s =
  add_scope s core_scope

let%test_module "core syntax tests" = (module struct
  let _ = bind_core_forms_primitives ()
  let%test _ = (resolve (datum_to_syntax (U.make_id "lambda"))
                = None)
  let%test _ = (resolve (namespace_syntax_introduce
                           (datum_to_syntax
                              (U.make_id "lambda")))
  (* = None) *)
                = Some (U.make_id "lambda"))
end)

(********************************************
 ** Compile time environment *)

(* NOTE from the talk, we learn that the comile time env
 * maps a binding (in this case Gensym) to either
 * 1. the constatnt variable
 * 2. a macro transforming function
 * FIXME make these changes
 ****)

module Env = Map.Make(Gensym)

let empty_env =
  Env.empty

let variable =
  U.make_id (Gensym.gensym ~sym:"variable" ())

let missing =
  U.make_id (Gensym.gensym ~sym:"missing" ())

let env_extend (* : TODO add type *)
  = fun env k v -> match k with
    | S_obj (IdT, Id key) ->
      Env.add key v env
    | _ -> raise (Unexpected __LOC__)

let env_lookup (* : TODO add type *)
  = fun env bnd -> match bnd with
    | S_obj (IdT, Id binding) ->
      begin match Env.find_opt binding env with
        | Some v -> v
        | None -> missing
      end
    | _ -> raise (Unexpected __LOC__)

let add_local_binding id =
  let key = U.make_id (Gensym.gensym ~sym:id.e ()) in
  add_binding id key;
  key

let%test_module "compile time env tests" = (module struct
  let sc1 = Scope.fresh ()
  let sc2 = Scope.fresh ()
  let loc_d = add_local_binding { e = "d"; scopes = Scopes.of_list [ sc1; sc2 ] }
  let%test _ = (
    resolve (U.make_stx { e = "d"; scopes = Scopes.of_list [ sc1; sc2 ] })
    = Some loc_d)
end)

(********************************************
 ** Expansion dispatch *)

exception Bad_syntax of string

let rec expand ?env:(e = empty_env) s =
  match s with
  | s when is_identifier s ->
    expand_identifier s e
  | S_obj (ListT, List (car :: _))
    when is_identifier car ->
    expand_id_application_form s e
  | s when (U.is_pair s || U.is_null s) ->
    expand_app s e
  | v ->
    (* if not an identifier or parens: this gets implicitly quoted *)
    U.make_list [ U.make_stx { e = "quote"; scopes = Scopes.singleton core_scope }
                ; s ]

and expand_identifier s env =
  match resolve s with
  | None ->
    raise (Bad_syntax ("free variable: "))
  | Some (S_obj (IdT, Id binding) as d) ->
    if CoreIDSet.mem binding core_primitives then
      s
    else if CoreIDSet.mem binding core_forms then
      raise (Bad_syntax ("'expand_identifier bad syntax: "))
    else begin match env_lookup env d with
      | v when v = missing ->
        raise (Bad_syntax ("out of context: "))
      | v when v = variable ->
        s
      | v when U.is_proc v ->
        expand ~env:env (apply_transformer v s)
      | _ (* else *) ->
        begin
          Printf.eprintf "What threw the error: '%s'\n" binding;
          U.format_scheme_obj Format.err_formatter s;
          Format.print_flush ();
          Printf.eprintf "\n\n";
          raise (Bad_syntax "illegal use of syntax:")
        end
    end

and expand_id_application_form (* : TODO add type *)
  = fun s env -> match s with
    | S_obj (ListT, List (id :: _)) ->
      let binding = resolve id in
      begin match binding with
        | Some (S_obj (IdT, Id "lambda")) ->
          expand_lambda s env
        | Some (S_obj (IdT, Id "let-syntax")) ->
          expand_let_syntax s env

        (* | Some (S_obj (IdT, Id "#%app")) ->
         *   let S_obj (ListT, List (app_id :: es)) in
         *   expand_app es env *)

        | Some (S_obj (IdT, Id "quote"))
        | Some (S_obj (IdT, Id "quote-syntax")) ->
          s
        | Some binding ->
          begin match env_lookup env binding with
            | v when U.is_proc v ->
              expand ~env:env (apply_transformer v s)
            | v ->
              expand_app s env
          end
        | None -> raise (Unexpected __LOC__)
      end
    | _ -> raise (Unexpected __LOC__)

and apply_transformer : scheme_object -> scheme_object -> scheme_object
  = fun o s ->
    (* FIXME the transformer should use the same types *)
    (* print_endline "\nUnwrapping the transformer"; *)
    let t = fun os ->
      (U.unwrap_proc o |> get_ok) os
      |> get_ok in
    (****)
    let intro_scope = Scope.fresh () in
    (* U.format_scheme_obj Format.std_formatter s;
     * Format.print_newline ();
     * Format.print_flush ();
     * print_endline "Unwrapping the arg list"; *)
    let intro_s =
      add_scope s intro_scope
      |> (fun ol ->
          U.unwrap_list ol
          |> (function
              | Ok l -> l
              | Error _ -> [ ol ])) (* NOTE FIXME this is strange, if a symbol 'a is bound to a transformer
                                     *            the transformer will get invoked, even if we are expanding
                                     *            just 'a and not '(a). I am confused about this.
                                     *            If the argument list was not a scheme_object list then we
                                     *            put it in a list explicitly -- per the situation described above
                                     ***)
    in
    (* print_endline "transforming..."; *)
    (* NOTE FIXME here we are applying the raw procedure to the
     *            list of arguments. However, real macros are defined
     *            using the 'lambda form, and are therefore not procedures
     *            as described in the type system of this implementation.
     *            This needs to then use the internal 'apply function which
     *            will handle the case for both a primitive procedure and a
     *            user-defined lambda.
     ***)
    let transformed_s = t intro_s in
    (* print_endline "done\n"; *)
    flip_scope transformed_s intro_scope

and expand_lambda (* : TODO add type *)
  = fun s env -> match s with
    | S_obj (ListT, List [ lambda_id
                         ; S_obj (ListT, List arg_ids)
                         ; body]) ->
      let sc = Scope.fresh () in
      let ids = List.map (fun id -> add_scope id sc) arg_ids in
      let bindings = List.map (function
          | S_obj (StxT, Stx s) ->
            add_local_binding s
          | _ -> raise (Unexpected __LOC__)) ids
      in
      let body_env = List.fold_left (fun env bnd ->
          env_extend env bnd variable) env bindings
      in
      let exp_body = expand ~env:body_env (add_scope body sc) in
      U.make_list [ lambda_id; U.make_list ids; exp_body ]
    | _ -> raise (Unexpected __LOC__)

and expand_let_syntax (* : TODO add type *)
  = fun s env ->
    match s with
    | S_obj (ListT, List [ let_syntax_id
                         ; S_obj (ListT, List
                                    (* inner list of (lhs; rhs) bindings *)
                                    trans_bnds
                                 )
                         ; body]) ->
      let (trans_ids, trans_rhss) = List.map (function
          | S_obj (ListT, List [ id; rhs ]) -> (id, rhs)
          | _ -> raise (Unexpected __LOC__)) trans_bnds
                                    |> List.split
      in
      let sc = Scope.fresh () in
      let ids = List.map (fun id ->
          add_scope id sc) trans_ids in
      let bindings = List.map (function
          | S_obj (StxT, Stx id) ->
            add_local_binding id
          | _ -> raise (Unexpected __LOC__)) trans_ids in
      let trans_vals = List.map eval_for_syntax_binding trans_rhss in
      let body_env = List.fold_left2 (fun env bnd vl ->
          env_extend env bnd vl) env bindings trans_vals in
      expand ~env:body_env (add_scope body sc)
    | _ -> raise (Unexpected __LOC__)

and expand_app (* : TODO add type *)
  = fun s env -> match s with
    | S_obj (ListT, List (rator :: rands)) ->
      U.make_list (
        (U.make_stx { e = "#%app"; scopes = Scopes.singleton core_scope })
        :: (expand ~env:env rator)
        :: (List.map (fun rand -> expand ~env:env rand) rands))
    | _ -> raise (Unexpected __LOC__)

(*********************************************)

and eval_for_syntax_binding rhs =
  expand rhs ~env:empty_env
  |> compile |> eval_compiled


(*********************************************)

(* return a data expr that can be evaluated *)
and compile : scheme_object -> scheme_object
  = fun s -> match s with
    | S_obj (ListT, List ( ((S_obj (StxT, Stx id) as s') :: ls) as ls')) ->
      begin match resolve s' with
        | Some (S_obj (IdT, Id "lambda") as lambda) ->
          let (S_obj (ListT, List ids)) :: _ = ls in
          S_obj (ListT, List [ lambda
                             ; S_obj (ListT, List
                                        (List.map (fun id ->
                                             resolve id |> Option.get) ids))
                             ; List.tl ls |> List.hd |> compile ])
        | Some (S_obj (IdT, Id "quote") as quote) ->
          S_obj (ListT, List [ quote
                             ; List.hd ls |> syntax_to_datum ])
        | Some (S_obj (IdT, Id "quote-syntax")) ->
          S_obj (ListT, List [ S_obj (IdT, Id "quote")
                             ; List.hd ls ])
        | _ -> S_obj (ListT, List (List.map compile ls'))
      end
    | S_obj (ListT, List ls) ->
      S_obj (ListT, List (List.map compile ls))
    | (S_obj (StxT, Stx _) as id) ->
      begin match resolve id with
        | Some v -> v
        | None -> raise (Unexpected __LOC__)
      end
    | _ -> raise (Bad_syntax ("bad syntax after expansion: "))

and eval_compiled s =
  Eval.eval s
  |> function
  | Result.Ok (v, _) ->
    Box.get v
  | Result.Error _ ->
    raise (Unexpected __LOC__)

let%test_module "expansion dispatch tests" = (module struct

  open Util
  open Test
  (* FIXME is there a way to let these bindings carry over between modules *)
  let _ = bind_core_forms_primitives ()
  let sc1 = Scope.fresh ()
  let sc2 = Scope.fresh ()
  let loc_a = make_id (Gensym.gensym ())
  let loc_b_out = make_id (Gensym.gensym ())
  let loc_b_in = make_id (Gensym.gensym ())
  let loc_c1 = make_id (Gensym.gensym ())
  let loc_c2 = make_id (Gensym.gensym ())
  let a = { e = "a"; scopes = Scopes.singleton sc1 }
  let b_out = { e = "b"; scopes = Scopes.singleton sc1 }
  let b_in = { e = "b"; scopes = Scopes.of_list [sc1; sc2] }
  let c1 = { e = "c"; scopes = Scopes.singleton sc1 }
  let c2 = { e = "c"; scopes = Scopes.singleton sc2 }
  let _ = add_binding a loc_a
  let _ = add_binding b_out loc_b_out
  let _ = add_binding b_in loc_b_in
  let _ = add_binding c1 loc_c1
  let _ = add_binding c2 loc_c2

  let%test _ = (env_lookup (empty_env) loc_a
                = missing)

  let%test _ = (env_lookup (env_extend (empty_env) loc_a (make_id "some-variable")) loc_a
                = (make_id "some-variable"))

  let%test _ = (let loc_d = add_local_binding { e = "d"; scopes = Scopes.of_list [sc1; sc2] } in
                resolve (U.make_stx { e = "d"; scopes = Scopes.of_list [sc1; sc2] })
                = Some loc_d)

  let%test _ = (
    (expand ~env:empty_env (datum_to_syntax (string_to_datum "1")))
    = make_list [ make_stx { e = "quote"; scopes = Scopes.singleton core_scope }
                ; make_int 1L])

  (* START THE TESTS HERE ---> 350 in tmp rkt *)

  let%test _ = (
    (syntax_to_datum
       (expand
          (namespace_syntax_introduce
             (datum_to_syntax (string_to_datum "(lambda (x) x)")))))
    = (string_to_datum "(lambda (x) x)"))

  let%test _ = (
    (expand (make_stx { e = "cons"; scopes = Scopes.singleton core_scope }))
    = (make_stx { e = "cons"; scopes = Scopes.singleton core_scope }))

  let%test _ = ( (* a locally bound variable expands to itself *)
    (expand (make_stx a)
       ~env:(env_extend empty_env loc_a variable))
    = (make_stx a))

  let%test _ = (
    try ignore((expand (make_stx { e = "a"; scopes =  Scopes.empty })
                  ~env:(env_extend empty_env loc_a variable)));
      false (* a bad syntax error should have been thrown *)
    with Bad_syntax _ -> true)

  let%test _ = (
    (expand (make_list [ make_stx a
                       ; make_int 1L
                       ])
       ~env:(env_extend empty_env loc_a variable))
    = make_list [make_stx { e = "#%app"; scopes = Scopes.of_list [ core_scope ] }
                ; make_stx a
                ; make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [ core_scope ] }
                            ; make_int 1L
                            ]])

  let%test _ = (
    (expand (datum_to_syntax (string_to_datum "(0 1)")))
    = make_list [ make_stx { e = "#%app"; scopes = Scopes.of_list [core_scope] }
                ; make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [core_scope] }; make_int 0L ]
                ; make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [core_scope] }; make_int 1L ]
                ])

  let%test _ = (
    (syntax_to_datum
       (expand (make_stx a)
          ~env:(env_extend empty_env loc_a
                  (make_proc (fun _ ->
                       ok (datum_to_syntax (string_to_datum "1")))))))
    = string_to_datum "(quote 1)")

  let%test _ = (
    (syntax_to_datum
       (expand
          (let s = (datum_to_syntax (string_to_datum "(a (lambda (x) x))")) in
           (add_scope (add_scope s sc1) core_scope))
          ~env:(env_extend empty_env loc_a
                  (make_proc (function
                       | _ :: obj1 :: _ -> ok obj1
                       | objs ->
                         Printf.eprintf "Test transformer didn't receive enough arguments";
                         List.iter (fun o ->
                             format_scheme_obj Format.err_formatter o;
                             Format.print_flush ()) objs;
                         raise (Unexpected __LOC__))))))
    = string_to_datum "(lambda (x) x)")

  (* larger expansion tests *)
  (* let%test _ = (let dtm = (string_to_datum "(lambda (x) x)") in
   *               (syntax_to_datum
   *                  (expand
   *                     (namespace_syntax_introduce
   *                        (datum_to_syntax dtm))))
   *               = dtm)
   *
   * let%test _ = (
   *   (syntax_to_datum
   *      (expand
   *         (namespace_syntax_introduce
   *            (datum_to_syntax
   *               (string_to_datum "(let-syntax ((one (lambda (stx)
   *                                         (quote-syntax (quote 1)))))
   *                       (one))")))))
   *   = make_list [ make_id "quote"; make_int 1L ])
   *
   * let%test _ = (
   *   expand (make_stx { e = "cons"; scopes = Scopes.singleton core_scope })
   *   = make_stx { e = "cons"; scopes = Scopes.singleton core_scope })
   *
   * let%test _ = (
   *   expand (make_stx { e = "a"; scopes = Scopes.of_list [sc1] }) ~env:(env_extend empty_env loc_a variable)
   *   = make_stx { e = "a"; scopes = Scopes.of_list [sc1] })
   * let%test _ = (
   *   try let _ = expand (make_stx { e = "a"; scopes = Scopes.empty }) in
   *     false
   *   with Bad_syntax _ -> true)
   * let%test _ = (
   *   expand (make_list [
   *       make_stx { e = "a"; scopes = Scopes.of_list [sc1] }
   *     ; make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [core_scope] }
   *                 ; make_int 1L ]]) ~env:(env_extend empty_env loc_a variable)
   *   = make_list [ make_stx { e = "a"; scopes = Scopes.of_list [sc1] }
   *               ; make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [core_scope] }
   *                           ; make_int 1L ]]) *)

  (* macro transformers *)
  (* let%test _ = (
   *   expand (namespace_syntax_introduce
   *             (datum_to_syntax (string_to_datum "((quote 0) (quote 1))")))
   *   = make_list [ make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [core_scope] }
   *                           ; make_int 0L ]
   *               ; make_list [ make_stx { e = "quote"; scopes = Scopes.of_list [core_scope] }
   *                           ; make_int 1L ]])
   * let transformed_s =
   *   apply_transformer (make_proc (fun [s] ->
   *       match s with
   *       | S_obj (ListT, List (_ :: v :: _)) ->
   *         ok (make_list [v; make_stx { e = "x"; scopes = Scopes.empty }])))
   *     (make_list [ make_stx { e = "m"; scopes = Scopes.empty }
   *                ; make_stx { e = "f"; scopes = Scopes.of_list [sc1] } ])
   * let%test _ = (
   *   syntax_to_datum transformed_s
   *   = make_list [ make_id "f"
   *               ; make_id "x" ])
   * let%test _ = (
   *   let (S_obj (ListT, List (f :: _))) = transformed_s in
   *   f = make_stx { e = "f"; scopes = Scopes.of_list [sc1] })
   * let%test _ = (
   *   let (S_obj (ListT, List (_ :: S_obj (StxT, Stx s) :: _))) = transformed_s in
   *   Scopes.cardinal s.scopes = 1) *)
end)

[@@@ocaml.warning "+8"]
[@@@ocaml.warning "+11"]
[@@@ocaml.warning "+26"]
[@@@ocaml.warning "+27"]
[@@@ocaml.warning "+32"]
[@@@ocaml.warning "+33"]
