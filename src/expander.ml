(*******************************************)
(*                                         *)
(* Gavin Gray 01.2021                      *)
(*                                         *)
(* Hygienic Racket-like Macro Expander     *)
(* Based off Matthew Flatt's:              *)
(*  "Let's Build a Hygenic Macro Expander" *)
(*      Strange Loop 2016                  *)
(*                                         *)
(*******************************************)

[@@@ocaml.warning "-8"]
[@@@ocaml.warning "-11"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

open Types

module Scope = struct
  type t = int
  let compare = Int.compare
  let count = ref 0
  let fresh () =
    begin
      incr count;
      !count
    end
end

module Scopes = Set.Make(Scope)

let hsh_size =
  1000

module Gensym = struct
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

type stx = symbol * Scopes.t

(* specific to expansion *)
type _ typ += LocT : Gensym.t data typ
type _ typ += StxT : stx data typ

(* specific to expansion *)
type _ data += Stx : stx -> stx data
type _ data += Loc : Gensym.t -> Gensym.t data

(********************************************
 ** Pretty printing *)

let print_dyn s =
  let rec sd = function
    | Dyn (IdT, Id s) -> s
    | Dyn (IntT, Int i) ->
      Int64.to_string i
    | Dyn (StxT, Stx (e, scopes)) ->
      Printf.sprintf "#<syntax %s>" e
    | Dyn (LocT, Loc i) ->
      Gensym.to_string i
    | Dyn (ListT, List ls) ->
      List.map sd ls
      |> List.fold_left (fun a s ->
          a ^ " " ^ s) ""
      |> Printf.sprintf "(%s)"
  in print_endline (sd s)

(********************************************
 ** Changing types *)

let rec datum_to_syntax : dyn -> dyn
  = fun d -> match d with
    | Dyn(IdT, Id i) ->
      Dyn (StxT, Stx (i, Scopes.empty))
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map datum_to_syntax ls))
    | _ -> d

and syntax_to_datum : dyn -> dyn
  = fun stx -> match stx with
    | Dyn(StxT, Stx (id, _)) ->
      Dyn (IdT, Id id)
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map syntax_to_datum ls))
    | _ -> stx

and sexpr_to_dyn = function
  | SxprId i -> Dyn (IdT, Id i)
  | SxprInt i -> Dyn (IntT, Int i)
  | SxprList l -> Dyn (ListT, List (List.map sexpr_to_dyn l))

(********************************************
 ** Scopes *)

let rec adjust_scope
  : dyn -> Scope.t -> (Scope.t ->  Scopes.t -> Scopes.t) -> dyn
  = fun s sc op -> match s with
    | Dyn(StxT, Stx (e, scopes)) ->
      Dyn(StxT, Stx (e, op sc scopes))
    | Dyn(ListT, List ls) ->
      Dyn (ListT, List (List.map (fun e ->
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

(********************************************
 ** Global binding table *)

exception Ambiguous_candidate_exn of string
exception Unexpected of string

let all_bindings =
  Hashtbl.create ~random:false hsh_size

let add_binding id binding =
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
    | Dyn(StxT, Stx id) ->
      (let candidates = find_all_matching_bindings id in
       match candidates with
       | [] -> None
       | _ :: _ ->
         let max_id = argmax (fun (_, s) ->
             (Scopes.cardinal s)) candidates
         in
         begin
           check_unambiguous max_id candidates;
           Some (Hashtbl.find all_bindings max_id)
         end)
    | v -> raise (Unexpected __LOC__)

and find_all_matching_bindings (e, scopes) =
  Hashtbl.fold (fun (e', scopes') _ acc ->
      if e = e' && Scopes.subset scopes' scopes then
        (e', scopes') :: acc
      else acc) all_bindings []

and check_unambiguous (_, scopes) =
  List.iter (fun (_, scopes') ->
      if not (Scopes.subset scopes' scopes) then
        raise (Ambiguous_candidate_exn
                 "some usefull message :)"))

(********************************************
 ** Core syntax and primitives *)

module CoreIDSet = Set.Make(String)

let core_scope = Scope.fresh ()
let core_forms = CoreIDSet.of_list
    [ "lambda"
    ; "let-syntax"
    ; "quote"
    ; "quote-syntax" ]

let core_primitives = CoreIDSet.of_list
    [ "datum->syntax"
    ; "syntax->datum"
    ; "syntax-e"
    ; "list"
    ; "cons"
    ; "car"
    ; "cdr"
    ; "map" ]

let bind_core_forms_primitives
  = fun () ->
    CoreIDSet.union core_forms core_primitives
    |> CoreIDSet.iter (fun str ->
        add_binding
          (str, Scopes.singleton core_scope)
          (Dyn(IdT, Id str)))

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
  Dyn (LocT, Loc (Gensym.gensym ()))

let env_extend (* : TODO add type *)
  = fun env k v -> match k with
    | Dyn(LocT, Loc key) ->
      Env.add key v env
    | _ -> raise (Unexpected __LOC__)

let env_lookup (* : TODO add type *)
  = fun env bnd -> match bnd with
    | Dyn(LocT, Loc binding) ->
      Env.find_opt binding env
    | _ -> raise (Unexpected __LOC__)

let add_local_binding ((e, scopes) as id) =
  let key = Dyn(LocT, Loc (Gensym.gensym ~sym:e ())) in
  add_binding id key;
  key

(********************************************
 ** Expansion dispatch *)

exception Bad_syntax of string

let rec expand ?env:(e = empty_env) s =
  match s with
  | Dyn(StxT, _) ->
    expand_identifier s e
  | Dyn(ListT, List (Dyn(StxT, _) :: _)) ->
    expand_id_application_form s e
  | Dyn(ListT, _) ->
    expand_app s e
  | v -> raise (Bad_syntax "bad syntax: todo")

and expand_identifier s env =
  match resolve s with
  | None -> raise (Bad_syntax "free variable: todo")
  | Some (Dyn(IdT, Id binding)) ->
    if CoreIDSet.mem binding core_primitives then
      s
    else if CoreIDSet.mem binding core_forms then
      raise (Bad_syntax "bad syntax: todo")
    else
      raise (Unexpected __LOC__)
  | Some (Dyn(LocT, Loc binding) as d) ->
    begin match env_lookup env d with
      | None ->
        raise (Bad_syntax "out of context: todo")
      | Some v ->
        if v = variable then
          s
        else
          raise (Bad_syntax "bad syntax: todo")
    end

and expand_id_application_form (* : TODO add type *)
  = fun s env -> match s with
    | Dyn(ListT, List (((Dyn(StxT, Stx id)) as d) :: _)) ->
      begin match resolve d with
        | Some (Dyn(IdT, Id "lambda")) ->
          expand_lambda s env
        | Some (Dyn(IdT, Id "let-syntax")) ->
          expand_let_syntax s env
        | Some (Dyn(IdT, Id "quote"))
        | Some (Dyn(IdT, Id "quote-syntax")) ->
          s
        | Some (Dyn(IdT, Id s)) ->
          raise (Unexpected __LOC__)
        | Some (Dyn(LocT, Loc k)) ->
          raise (Unexpected __LOC__)
        (* begin match env_lookup env (`Loc k) with
         *   | Some (`Func f) ->
         *     expand (apply_transformer v s) env
         *   | _ -> expand_app s env
         * end *)
        | _ -> raise (Unexpected __LOC__)
      end
    | _ -> raise (Unexpected __LOC__)


and apply_transformer t s =
  let intro_scope = Scope.fresh () in
  let intro_s = add_scope s intro_scope in
  let transformed_s = t intro_s in
  flip_scope transformed_s intro_scope

and expand_lambda (* : TODO add type *)
  = fun s env -> match s with
    | Dyn(ListT, List [ lambda_id
                      ; Dyn(ListT, List [arg_id])
                      ; body]) ->
      let sc = Scope.fresh () in
      let id = add_scope arg_id sc in
      begin match id with
        | (Dyn(StxT, Stx id) as stx_id) ->
          let binding = add_local_binding id in
          let body_env = env_extend env binding variable in
          let exp_body = expand (add_scope body sc) ~env:body_env in
          Dyn (ListT, List [ lambda_id
                           ; Dyn (ListT, List [stx_id])
                           ; exp_body])
        | _ -> raise (Unexpected __LOC__)
      end
    | v -> raise (Unexpected __LOC__)

and expand_let_syntax (* : TODO add type *)
  = fun s env ->
    match s with
    | Dyn(ListT, List [ let_syntax_id
                      ; Dyn(ListT, List [
                            Dyn(ListT, List [
                                lhs_id ; rhs
                              ])
                          ])
                      ; body]) ->
      let sc = Scope.fresh () in
      begin match add_scope lhs_id sc with
        | Dyn(StxT, Stx id) ->
          let binding = add_local_binding id in
          let rhs_val = eval_for_syntax_binding rhs in
          let body_env = env_extend env binding rhs_val in
          expand (add_scope body sc) ~env:body_env
      end
    | _ -> raise (Unexpected __LOC__)

and expand_app (* : TODO add type *)
  = fun s env -> match s with
    | Dyn(ListT, List ls) ->
      Dyn(ListT, List (List.map (fun sub_s ->
          expand sub_s ~env:env) ls))
    | _ -> raise (Unexpected __LOC__)

(*********************************************)

and eval_for_syntax_binding rhs =
  eval_compiled (compile (expand rhs ~env:empty_env))

(*********************************************)

(* return a data expr that can be evaluated *)
and compile (* : type a. a typ -> dyn -> a data *)
  = fun s -> match s with
    | Dyn(ListT, List ( (Dyn (StxT, Stx id) as s') :: _)) ->
      begin match resolve s' with
        | None ->
          raise (Unexpected __LOC__) (* TODO *)
        | Some (Dyn(IdT, Id "lambda")) ->
          raise (Unexpected __LOC__) (* TODO *)
        | Some (Dyn(IdT, Id "quote")) ->
          raise (Unexpected __LOC__) (* TODO *)
        | Some (Dyn(IdT, Id "quote-syntax")) ->
          raise (Unexpected __LOC__) (* TODO *)
        | _ -> (* Application *)
          raise (Unexpected __LOC__) (* TODO *)
      end
    | Dyn(ListT, List ls) ->
      raise (Unexpected __LOC__) (* TODO *)
    | Dyn(StxT, Stx id) ->
      raise (Unexpected __LOC__) (* TODO resolve and compile result *)
    | _ -> raise (Bad_syntax "bad syntax after expansion: todo")

and eval_compiled s =
  Eval.eval s
  |> function
  | Result.Ok v -> v
  | Result.Error _ ->
    raise (Unexpected __LOC__)

(* short cheap tests to keep everything incrementally working *)
(* TODO once a real dune project is started move these to a testing dir *)
let%test_module _ = (module struct

  let s2d s =
    Parser.sexpr_of_string s
    |> function
    | Ok ast -> sexpr_to_dyn ast
    | Error s ->
      print_endline s;
      raise (Unexpected __LOC__)

  let _ = bind_core_forms_primitives ()

  (* Datum / syntax tests *)
  let%test _ = (datum_to_syntax (s2d "a")
                = (Dyn(StxT, Stx ("a", Scopes.empty))))
  let%test _ = (datum_to_syntax (s2d "1")
                = (Dyn(IntT, Int 1L)))
  let%test _ = (datum_to_syntax (s2d "(a b c)")
                = (Dyn(ListT, List[ (Dyn(StxT, Stx ("a", Scopes.empty)))
                                  ; (Dyn(StxT, Stx ("b", Scopes.empty)))
                                  ; (Dyn(StxT, Stx ("c", Scopes.empty)))])))

  (* Scopes tests *)
  let nl = fun () -> Dyn (LocT, Loc (Gensym.gensym ()))

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
                   (datum_to_syntax (s2d "x"))
                   sc1)
                = (Dyn(StxT, Stx ("x", Scopes.singleton sc1))))
  let%test _ = ((add_scope (datum_to_syntax (s2d "(x (y))")) sc1)
                = Dyn (ListT, List [Dyn (StxT, Stx ("x", Scopes.singleton sc1))
                                   ; Dyn (ListT, List [ Dyn(StxT, Stx ("y", Scopes.singleton sc1))])]))
  let%test _ = (add_scope (add_scope (datum_to_syntax (s2d "x")) sc1) sc2
                = Dyn (StxT, Stx ("x", Scopes.of_list [sc1; sc2])))
  let%test _ = (add_scope (add_scope (datum_to_syntax (s2d "x")) sc1) sc1
                = Dyn (StxT, Stx ("x", Scopes.singleton sc1)))
  let%test _ = (flip_scope (Dyn (StxT, Stx ("x", Scopes.singleton sc1))) sc2
                = (Dyn (StxT, Stx ("x", Scopes.of_list [sc1; sc2]))))
  let%test _ = (flip_scope (Dyn (StxT, Stx ("x", Scopes.of_list [sc1; sc2]))) sc2
                = Dyn (StxT, Stx ("x", Scopes.singleton sc1)))

  let a = ("a", Scopes.singleton sc1)
  let b_out = ("b", Scopes.singleton sc1)
  let b_in = ("b", Scopes.of_list [sc1; sc2])
  let c1 = ("c", Scopes.singleton sc1)
  let c2 = ("c", Scopes.singleton sc2)

  let _ = add_binding a loc_a
  let _ = add_binding b_out loc_b_out
  let _ = add_binding b_in loc_b_in
  let _ = add_binding c1 loc_c1
  let _ = add_binding c2 loc_c2

  let%test _ = (resolve (Dyn(StxT, Stx a))
                = Some loc_a)
  let%test _ = (resolve (Dyn(StxT, Stx ("a", Scopes.of_list [sc1; sc2])))
                = Some loc_a)
  let%test _ = (resolve (Dyn(StxT, Stx ("b", Scopes.of_list [sc2])))
                = None)

  let%test _ = (resolve (Dyn(StxT, Stx ("a", Scopes.of_list [sc2])))
                = None)
  let%test _ = (resolve (Dyn(StxT, Stx ("b", Scopes.of_list [sc1])))
                = Some loc_b_out)
  let%test _ = (resolve (Dyn(StxT, Stx ("b", Scopes.of_list [sc1; sc2])))
                = Some loc_b_in)

  let%test _ = (find_all_matching_bindings a
                = [a])

  let%test _ = (try ignore(resolve (Dyn(StxT, Stx ("c", Scopes.of_list [sc1; sc2]))));
                  false
                with Ambiguous_candidate_exn _ -> true)
  let%test _ = (find_all_matching_bindings ("a", Scopes.singleton sc2)
                = [])

  module S = Set.Make(struct
      type t = (string * Scopes.t)
      let compare (s, ss) (s', ss') =
        if s = s' then
          compare ss ss'
        else compare s s' end)

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
  let%test _ = (resolve (datum_to_syntax (Dyn(IdT, Id "lambda")))
                = None)
  let%test _ = (resolve (introduce (datum_to_syntax (Dyn(IdT, Id "lambda"))))
                = Some (Dyn(IdT, Id "lambda")))

  let%test _ = (env_lookup (empty_env) loc_a
                = None)

  let%test _ = (env_lookup (env_extend (empty_env) loc_a (Dyn (IdT, Id "variable"))) loc_a
                = Some (Dyn (IdT, Id "variable")))

  let%test _ = (let loc_d = add_local_binding ("d", Scopes.of_list [sc1; sc2]) in
                resolve (Dyn(StxT, Stx ("d", Scopes.of_list [sc1; sc2])))
                = Some loc_d)

  let%test _ = (let dtm = (s2d "(lambda (x) x)") in
                (syntax_to_datum
                   (expand
                      (add_scope
                         (datum_to_syntax dtm) core_scope)))
                = dtm)

  let%test _ = (
    (try (syntax_to_datum
            (expand
               (add_scope
                  (datum_to_syntax
                     (s2d "(let-syntax ((one (lambda (stx) (quote-syntax '1)))) (one))")) core_scope)))
     with Unexpected loc ->
       begin
         print_endline loc;
         raise (Unexpected loc)
       end)
    = Dyn (ListT, List [ Dyn (IdT, Id "quote") ; Dyn (IntT, Int 1L) ]))

end)

[@@@ocaml.warning "+8"]
[@@@ocaml.warning "+11"]
[@@@ocaml.warning "+26"]
[@@@ocaml.warning "+27"]
[@@@ocaml.warning "+32"]
