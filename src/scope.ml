(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
module U = Util

include struct
  open Types.Err
  let ( >> ) = ( >> )
end

include Types.Scope

let rec adjust_scope
  = fun s_e sc op -> match s_e with
    | s when U.is_syntax s ->
      let e = Syntax.syntax_e s |> Err.get_ok in
      let scps = Syntax.syntax_scopes s |> Err.get_ok in
      U.make_syntax { e = adjust_scope e sc op
                    ; scopes = op sc scps
                    }
    | s when U.is_list s ->
      List.map (fun s -> adjust_scope s sc op) (U.unwrap_list_exn s)
      |> U.make_list
    | s -> s

let add_scope
  = fun s sc ->
    adjust_scope s sc Scopes.add

let set_flip e s =
  if Scopes.mem e s then
    Scopes.remove e s
  else Scopes.add e s

let flip_scope
  = fun s sc ->
    adjust_scope s sc set_flip

(* Global binding table *)

exception Ambiguous_candidate_exn of string

let hsh_size = 1000 (* Estimated maximum number of bindings *)

let all_bindings =
  Hashtbl.create ~random:false hsh_size

let add_binding : scheme_object
  -> scheme_object
  -> unit Err.t
  = fun id binding ->
    match id with
    | s when Syntax.is_identifier s ->
      Hashtbl.add all_bindings id binding
      |> Err.ok
    | s -> Err.error (Type_mismatch ("identifier?", s))

let rec resolve (* : TODO add type *)
  : scheme_object -> (scheme_object option) Err.t
  = fun id ->
    match find_all_matching_bindings id with
    | [] -> Err.ok None
    | candidate_ids ->
      let max_id = argmax (fun s ->
          (Scopes.cardinal
             (Syntax.syntax_scopes s
              |> Err.get_ok))) candidate_ids
      in
      check_unambiguous max_id candidate_ids id >>
      (Some (Hashtbl.find all_bindings max_id) |> Err.ok)

and argmax
  = fun f xs -> match xs with
    | [] -> raise (Err.Unexpected (__LOC__, void))
    | x :: xs ->
      List.fold_left (fun acc b ->
          if f acc > f b then
            acc
          else b) x xs

and find_all_matching_bindings
  = fun stx ->
    Hashtbl.fold (fun stx' _ acc ->
        if Syntax.syntax_e stx = Syntax.syntax_e stx' &&
           Scopes.subset
             (Syntax.syntax_scopes stx' |> Err.get_ok)
             (Syntax.syntax_scopes stx |> Err.get_ok)
        then stx' :: acc
        else acc) all_bindings []

and check_unambiguous
  = fun mx_c_id c_ids err_id ->
    Err.map_m (fun c_id ->
        if not (Scopes.subset
                  (Syntax.syntax_scopes c_id |> Err.get_ok)
                  (Syntax.syntax_scopes mx_c_id |> Err.get_ok)) then
          Err.error (Runtime_error ("ambigious", err_id))
        else Err.ok ()) c_ids
