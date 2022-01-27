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

open Types
module U = Util

let hsh_size =
  1000

module Gensym : sig
  type t = string
  val gensym : ?sym:string -> unit -> t
end = struct
  include String
  type t = string
  let counter = ref 0
  let to_string v = v
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
  let%test _ = ((syntax_to_datum (datum_to_syntax (string_to_datum "1"))
                 = make_int 1L))
  let%test _ = ((syntax_to_datum (datum_to_syntax (string_to_datum "a"))
                 = make_id "a"))
  let%test _ = ((syntax_to_datum (datum_to_syntax (string_to_datum "'(a b c)"))
                 = make_list [ make_id "a"; make_id "b"; make_id "c" ]))
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
  (* adding flipping scopes *)
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
  = fun s ->
    let argmax f xs = match xs with
      | [] -> raise (Unexpected __LOC__)
      | x :: xs ->
        List.fold_left (fun acc b ->
            if f acc > f b then
              acc
            else b) x xs
    in
    match s with
    | S_obj (StxT, Stx id) ->
      (let candidates = find_all_matching_bindings id in
       match candidates with
       | [] -> None
       | _ :: _ ->
         let max_id = argmax (fun s ->
             (Scopes.cardinal s.scopes)) candidates
         in
         begin
           check_unambiguous max_id candidates;
           Some (Hashtbl.find all_bindings max_id)
         end)
    | v -> raise (Unexpected __LOC__)

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

let%test_module _ = (module struct
  open Util
  open Test
  (* FIXME is there a way to let these bindings carry over between modules *)
  let nl = fun () -> S_obj (IdT, Id (Gensym.gensym ()))
  let sc1 = Scope.fresh ()
  let sc2 = Scope.fresh ()
  let loc_a = nl ()
  let loc_b_out = nl ()
  let loc_b_in = nl ()
  let loc_c1 = nl ()
  let loc_c2 = nl ()
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

  let%test _ = (resolve (S_obj (StxT, Stx a))
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
          (str, Scopes.singleton core_scope)
          (S_obj (IdT, Id str)))

let introduce s =
  add_scope s core_scope

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
  S_obj (IdT, Id (Gensym.gensym ()))

let env_extend (* : TODO add type *)
  = fun env k v -> match k with
    | S_obj (IdT, Id key) ->
      Env.add key v env
    | _ -> raise_unexpected __LOC__ k

let env_lookup (* : TODO add type *)
  = fun env bnd -> match bnd with
    | S_obj (IdT, Id binding) ->
      Env.find_opt binding env
    | _ -> raise_unexpected __LOC__ bnd

let add_local_binding ((e, scopes) as id) =
  let key = S_obj (IdT, Id (Gensym.gensym ~sym:e ())) in
  add_binding id key;
  key

(********************************************
 ** Expansion dispatch *)

exception Bad_syntax of string

let rec expand ?env:(e = empty_env) s =
  match s with
  | S_obj (StxT, _) ->
    expand_identifier s e
  | S_obj (ListT, List (S_obj (StxT, _) :: _)) ->
    expand_id_application_form s e
  | S_obj (ListT, _) ->
    expand_app s e
  | v -> raise (Bad_syntax ("'expand bad syntax: "))

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
      | None ->
        raise (Bad_syntax ("out of context: "))
      | Some v ->
        if v = variable then
          s
        else
          raise (Bad_syntax ("'expand_identifier bad syntax: "))
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
        | Some (S_obj (IdT, Id "quote"))
        | Some (S_obj (IdT, Id "quote-syntax")) ->
          s
        | Some binding ->
          begin match env_lookup env binding with
            | Some (S_obj (LambT, Lamb f)) ->
              expand (apply_transformer f s) ~env:env
            | _ -> expand_app s env
          end
        | None -> raise_unexpected __LOC__ id
      end
    | _ -> raise_unexpected __LOC__ s

and apply_transformer : (scheme_object list -> scheme_object) -> scheme_object -> scheme_object
  = fun t s ->
    let intro_scope = Scope.fresh () in
    let intro_s = add_scope s intro_scope in
    let transformed_s = t [intro_s] in
    flip_scope transformed_s intro_scope

and expand_lambda (* : TODO add type *)
  = fun s env -> match s with
    | S_obj (ListT, List [ lambda_id
                         ; S_obj (ListT, List [arg_id])
                         ; body]) ->
      let sc = Scope.fresh () in
      let id = add_scope arg_id sc in
      begin match id with
        | (S_obj (StxT, Stx id) as stx_id) ->
          let binding = add_local_binding id in
          let body_env = env_extend env binding variable in
          let exp_body = expand (add_scope body sc) ~env:body_env in
          S_obj (ListT, List [ lambda_id
                             ; S_obj (ListT, List [stx_id])
                             ; exp_body])
        | _ -> raise_unexpected __LOC__ id
      end
    | v -> raise_unexpected __LOC__ v

and expand_let_syntax (* : TODO add type *)
  = fun s env ->
    match s with
    | S_obj (ListT, List [ let_syntax_id
                         ; S_obj (ListT, List [
                               S_obj (ListT, List [
                                   lhs_id ; rhs
                                 ])
                             ])
                         ; body]) ->
      let sc = Scope.fresh () in
      begin match add_scope lhs_id sc with
        | S_obj (StxT, Stx id) ->
          let binding = add_local_binding id in
          let rhs_val = eval_for_syntax_binding rhs in
          let body_env = env_extend env binding rhs_val in
          expand (add_scope body sc) ~env:body_env
      end
    | _ -> raise_unexpected __LOC__ s

and expand_app (* : TODO add type *)
  = fun s env -> match s with
    | S_obj (ListT, List ls) ->
      S_obj (ListT, List (List.map (fun sub_s ->
          expand sub_s ~env:env) ls))
    | _ -> raise_unexpected __LOC__ s

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
                             ; List.hd ls |> U.syntax_to_datum ])
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
        | None -> raise_unexpected __LOC__ id
      end
    | _ -> raise (Bad_syntax ("bad syntax after expansion: "))

and eval_compiled s =
  Eval.eval s
  |> function
  | Result.Ok v -> v
  | Result.Error _ ->
    raise_unexpected __LOC__ s

(* short cheap tests to keep everything incrementally working *)
(* TODO once a real dune project is started move these to a testing dir *)
let%test_module _ = (module struct

  let _ = bind_core_forms_primitives ()

  module S = Set.Make(struct
      type t = (string * Scopes.t)
      let compare (s, ss) (s', ss') =
        if s = s' then
          Scopes.compare ss ss'
        else String.compare s s' end)

  let%test _ = (let open S in
                equal (find_all_matching_bindings b_in |> of_list)
                  (of_list [b_in; b_out]))
  let%test _ = (let open S in
                equal (find_all_matching_bindings ("c", Scopes.of_list [sc1; sc2])
                       |> of_list)
                  (of_list [c1; c2]))
  let%test _ = (check_unambiguous b_in [b_out; b_in]
                = ())
  let%test _ = (try check_unambiguous c2 [c1; c2];
                  false
                with Ambiguous_candidate_exn _ ->
                  true)
  let%test _ = (resolve (U.datum_to_syntax (S_obj (IdT, Id "lambda")))
                = None)
  let%test _ = (resolve (introduce (U.datum_to_syntax (S_obj (IdT, Id "lambda"))))
                = Some (S_obj (IdT, Id "lambda")))

  let%test _ = (env_lookup (empty_env) loc_a
                = None)

  let%test _ = (env_lookup (env_extend (empty_env) loc_a (S_obj (IdT, Id "variable"))) loc_a
                = Some (S_obj (IdT, Id "variable")))

  let%test _ = (let loc_d = add_local_binding ("d", Scopes.of_list [sc1; sc2]) in
                resolve (S_obj (StxT, Stx ("d", Scopes.of_list [sc1; sc2])))
                = Some loc_d)

  (* larger expansion tests *)
  let%test _ = (let dtm = (s2d "(lambda (x) x)") in
                (U.syntax_to_datum
                   (expand
                      (add_scope
                         (U.datum_to_syntax dtm) core_scope)))
                = dtm)

  let%test _ = (
    (U.syntax_to_datum
       (expand
          (add_scope
             (U.datum_to_syntax
                (s2d "(let-syntax ((one (lambda (stx)
                                          (quote-syntax (quote 1)))))
                        (one))")) core_scope))
     |> (fun scheme_object -> begin U.fmt scheme_object |> print_endline; scheme_object end)
    )
    = S_obj (ListT, List [ S_obj (IdT, Id "quote") ; S_obj (IntT, Int 1L) ]))

  let%test _ = (
    expand (S_obj (StxT, Stx ("cons", Scopes.singleton core_scope)))
    = S_obj (StxT, Stx ("cons", Scopes.singleton core_scope)))

  let%test _ = (
    expand (S_obj (StxT, Stx ("a", Scopes.of_list [sc1]))) ~env:(env_extend empty_env loc_a variable)
    = (S_obj (StxT, Stx ("a", Scopes.of_list [sc1]))))
  let%test _ = (
    try let _ = expand (S_obj (StxT, Stx ("a", Scopes.empty))) in
      false
    with Bad_syntax _ -> true)
  let%test _ = (
    expand (S_obj (ListT, List [
        S_obj (StxT, Stx ("a", Scopes.of_list [sc1]))
      ; S_obj (ListT, List [ S_obj (StxT, Stx ("quote", Scopes.of_list [core_scope]))
                           ; S_obj (IntT, Int 1L) ])
      ])) ~env:(env_extend empty_env loc_a variable)
    = (S_obj (ListT, List [
        S_obj (StxT, Stx ("a", Scopes.of_list [sc1]))
      ; S_obj (ListT, List [ S_obj (StxT, Stx ("quote", Scopes.of_list [core_scope]))
                           ; S_obj (IntT, Int 1L) ])
      ])))

  (* macro transformers *)
  let%test _ = (
    expand (introduce (U.datum_to_syntax (s2d "((quote 0) (quote 1))")))
    = S_obj (ListT, List [ S_obj (ListT, List [ S_obj (StxT, Stx ("quote", Scopes.of_list [core_scope])); S_obj (IntT, Int 0L) ])
                         ; S_obj (ListT, List [ S_obj (StxT, Stx ("quote", Scopes.of_list [core_scope])); S_obj (IntT, Int 1L) ]) ]))
  let transformed_s =
    apply_transformer (fun [s] ->
        match s with
        | S_obj (ListT, List (_ :: v :: _)) ->
          S_obj (ListT, List [v; S_obj (StxT, Stx ("x", Scopes.empty))]))
      (U.make_list [ S_obj (StxT, Stx ("m", Scopes.empty))
                   ; S_obj (StxT, Stx ("f", Scopes.of_list [sc1])) ])

  let%test _ = (
    U.syntax_to_datum transformed_s
    = S_obj (ListT, List [ S_obj (IdT, Id "f")
                         ; S_obj (IdT, Id "x") ]))
  let%test _ = (
    let (S_obj (ListT, List (f :: _))) = transformed_s in
    f = S_obj (StxT, Stx ("f", Scopes.of_list [sc1])))
  let%test _ = (
    let (S_obj (ListT, List (_ :: S_obj (StxT, Stx (_, scopes)) :: _))) = transformed_s in
    Scopes.cardinal scopes = 1)
end)

[@@@ocaml.warning "+8"]
[@@@ocaml.warning "+11"]
[@@@ocaml.warning "+26"]
[@@@ocaml.warning "+27"]
[@@@ocaml.warning "+32"]
