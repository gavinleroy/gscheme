(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

include Types.Scope
module U = Util

include struct
  open Types
  let ( >> ) = ( >> )
end

let rec adjust_scope
  : Types.scheme_object -> t
    -> (t -> Types.Scopes.t -> Types.Scopes.t)
    -> Types.scheme_object
  = fun s_e sc op -> match s_e with
    | s when U.is_syntax s ->
      let e = Syntax.syntax_e s |> Types.get_ok in
      let scps = Syntax.syntax_scopes s |> Types.get_ok in
      U.make_syntax { e = adjust_scope e sc op
                    ; scopes = op sc scps
                    }
    | s when U.is_list s ->
      List.map (fun s -> adjust_scope s sc op) (U.unwrap_list_exn s)
      |> U.make_list
    | s -> s

let add_scope
  = fun s sc ->
    adjust_scope s sc Types.Scopes.add

let set_flip e s =
  if Types.Scopes.mem e s then
    Types.Scopes.remove e s
  else Types.Scopes.add e s

let flip_scope
  = fun s sc ->
    adjust_scope s sc set_flip

(* Global binding table *)

exception Ambiguous_candidate_exn of string

let hsh_size = 1000 (* Estimated maximum number of bindings *)

let all_bindings =
  Hashtbl.create ~random:false hsh_size

let add_binding_bang : Types.scheme_object
  -> Types.binding_variant
  -> unit Types.maybe_exn
  = fun id binding ->
    match id with
    | s when Syntax.is_identifier s ->
      Hashtbl.add all_bindings id binding
      |> Types.ok
    | s -> Types.error (Types.Type_mismatch ("identifier?", s))

let rec resolve (* : TODO add type *)
  : Types.scheme_object -> (Types.binding_variant option) Types.maybe_exn
  = fun id ->
    match find_all_matching_bindings id with
    | [] -> Types.ok None
    | candidate_ids ->
      let max_id = argmax (fun s ->
          (Types.Scopes.cardinal
             (Syntax.syntax_scopes s
              |> Types.get_ok))) candidate_ids
      in
      check_unambiguous max_id candidate_ids id >>
      (Some (Hashtbl.find all_bindings max_id) |> Types.ok)

and argmax
  = fun f xs -> match xs with
    | [] -> raise (Types.Unexpected (__LOC__, Types.void))
    | x :: xs ->
      List.fold_left (fun acc b ->
          if f acc > f b then
            acc
          else b) x xs

and find_all_matching_bindings
  = fun stx ->
    Hashtbl.fold (fun stx' _ acc ->
        if Syntax.syntax_e stx = Syntax.syntax_e stx' &&
           Types.Scopes.subset
             (Syntax.syntax_scopes stx' |> Types.get_ok)
             (Syntax.syntax_scopes stx |> Types.get_ok)
        then stx' :: acc
        else acc) all_bindings []

and check_unambiguous
  = fun mx_c_id c_ids err_id ->
    Types.map_m (fun c_id ->
        if not (Types.Scopes.subset
                  (Syntax.syntax_scopes c_id |> Types.get_ok)
                  (Syntax.syntax_scopes mx_c_id |> Types.get_ok)) then
          Types.error (Types.Runtime_error ("ambigious", err_id))
        else Types.ok ()) c_ids
