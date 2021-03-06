(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Types
open Err
open Util

(* NOTE library functions should operate on scheme objects
 * these are then wrapped in the namespace module for different
 * argument lenghts
 **)

(* TODO FIXME equality *)

let is_equal a b =
  a = b

let eqv a b =
  a = b

let eq a b =
  a = b

(* predicates can be found in the Util library, contract functions below *)

let rec list_of
  = fun p a2 -> match a2 with
    | List ls -> List.for_all p ls
    | other -> false

and assoc ?equality:(e = is_equal) v ls =
  match ls with
  | ls when is_list ls && list_of is_pair ls ->
    List.find_opt (fun pair ->
        (car pair >>| fun c -> e c v)
        |> value ~default:false) (unwrap_list_exn ls)
    |> ok
  | ls when is_list ls -> error (Type_mismatch ("pair? in-list", ls))
  | other -> error (Type_mismatch ("list?", ls))

and assv v ls =
  assoc ~equality:eqv v ls

and assq v ls =
  assoc ~equality:eq v ls

(* pair / list primitives *)

and car : scheme_object -> scheme_object maybe_exn
  = function
    | List (v :: _)
    | Dotted ((v :: _), _) ->
      ok v
    | arg -> error (Type_mismatch ("pair?", arg))

and cdr : scheme_object -> scheme_object maybe_exn
  = function
    | List (_ :: ls) ->
      make_list ls |> ok
    | Dotted ([_], tl) ->
      ok tl
    | Dotted ((_ :: ls), tl) ->
      make_dotted (ls, tl) |> ok
    | arg -> error (Type_mismatch ("pair?", arg))

let caar : scheme_object -> scheme_object maybe_exn
  = fun a1 -> car a1 >>= car (* (car (car a1)) *)

let cddr : scheme_object -> scheme_object maybe_exn
  = fun a1 -> cdr a1 >>= cdr (* (cdr (cdr a1)) *)

let cadr : scheme_object -> scheme_object maybe_exn
  = fun a1 -> cdr a1 >>= car (* (car (cdr a1)) *)

let cdar : scheme_object -> scheme_object maybe_exn
  = fun a1 -> car a1 >>= cdr (* (cdr (car a1)) *)

let cons : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | x, List [] ->
      make_list [x] |> ok
    | x, List ls ->
      make_list (x :: ls) |> ok
    | x, Dotted (ls, tl) ->
      make_dotted (x :: ls, tl) |> ok
    | x, y -> make_dotted ([x], y) |> ok

let list : scheme_object list -> scheme_object maybe_exn
  = fun ls -> make_list ls |> ok

let list_ref : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | List ls, Num (Number.Int idx) ->
      let idx = (Int64.to_int idx) in
      if idx < List.length ls then
        List.nth ls idx |> ok
      else error (Runtime_error (
          Printf.sprintf "index %d out of range" idx, a2))
    | ls, intgr when is_list ls ->
      error (Type_mismatch ("integer?", intgr))
    | ls, _ -> error (Type_mismatch ("list?", ls))

let list_length : scheme_object -> scheme_object maybe_exn
  = fun a1 -> match a1 with
    | List ls ->
      List.length ls |> Int64.of_int
      |> Util.make_int |> ok
    | ls -> error (Type_mismatch ("list?", ls))

let list_map : (scheme_object -> scheme_object maybe_exn) -> scheme_object -> scheme_object maybe_exn
  = fun f a2 -> match a2 with
    | List ls ->
      map_m f ls >>| (fun results -> make_list results)
    | ls  -> error (Type_mismatch ("list?", ls))

let pair_append : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | List firstl, List secondl ->
      List.append firstl secondl |> make_list |> ok
    | List firstl, Dotted (secondl, rest) ->
      List.append firstl secondl |> (fun stem ->
          make_dotted (stem, rest)) |> ok
    | ls, other when is_list ls ->
      error (Type_mismatch ("pair?", other))
    | ls, _ -> error (Type_mismatch ("list?", ls))

let vector : scheme_object list -> scheme_object maybe_exn
  = fun ls -> Vector.of_list ls |> make_vector |> ok

let vector_ref : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | Vec v, Num (Number.Int idx) ->
      let idx = (Int64.to_int idx) in
      if idx < Vector.length v then
        Vector.get v idx |> ok
      else error (Runtime_error
                    (Printf.sprintf "index %d out of range" idx, a2))
    | v, intgr when is_vector v ->
      error (Type_mismatch ("integer?", intgr))
    | ls, _ -> error (Type_mismatch ("vector?", ls))

let vector_set : scheme_object -> scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 obj -> match a1, a2 with
    | Vec v, Num (Number.Int idx)  ->
      let idx = (Int64.to_int idx) in
      if idx < Vector.length v then
        begin
          Vector.set v idx obj;
          ok void
        end
      else error (Runtime_error
                    (Printf.sprintf "index %d out of range" idx, a2))
    | v, intgr when is_vector v ->
      error (Type_mismatch ("integer?", intgr))
    | ls, _ -> error (Type_mismatch ("vector?", ls))

let vector_make
  = fun ls -> match ls with
    | [ Num (Number.Int size); fill ] ->
      let size = Int64.to_int size in
      ok (make_vector (Vector.make size fill))
    | [ Num (Number.Int size) ] ->
      let size = Int64.to_int size in
      ok (make_vector (Vector.make size void))
    | [ arg ] -> error (Type_mismatch ("integer?", arg))
    | ls -> error (Arity_mismatch (1, List.length ls, ls))

let apply
  = fun f args -> match f, args with
    | f, args when Util.is_procedure f && Util.is_list args ->
      Util.unwrap_procedure f
      >>= fun f ->
      f (Util.unwrap_list_exn args)
    | f, args when Util.is_list args ->
      error (Type_mismatch ("procedure?", f))
    | _, oth -> error (Type_mismatch ("list?", oth))

let map
  = fun f ls ->
    match f, ls with
    | f, ls when is_procedure f && is_list ls ->
      list_map_m (fun v ->
          make_list [ v ] |> apply f) ls
    | f, ls when is_list ls ->
      error (Type_mismatch ("procedure?", f))
    | _, ls -> error (Type_mismatch ("list?", ls))

(* IO primitives *)

let open_input_file : scheme_object -> scheme_object maybe_exn
  = function
    | String fn ->
      ReadPort (open_in fn)
      |> make_port |> ok
    | bad -> error (Type_mismatch ("string?", bad))

let open_output_file : scheme_object -> scheme_object maybe_exn
  = function
    | String fn ->
      WritePort (open_out fn)
      |> make_port |> ok
    | bad -> error (Type_mismatch ("string?", bad))

let close_input_port : scheme_object -> scheme_object maybe_exn
  = function
    | Port (ReadPort p) ->
      begin
        close_in p;
        ok void
      end
    | bad -> error (Type_mismatch ("input-port?", bad))

let close_output_port : scheme_object -> scheme_object maybe_exn
  = function
    | Port (WritePort p) ->
      begin
        close_out p;
        ok void
      end
    | bad -> error (Type_mismatch ("output-port?", bad))

(* *********************** *)

(* FIXME if a macro expansion causes 'transpose to fail
 * the error message will be wildly unhelpfull *)
let transpose ll =
  let hd = function
    | [] -> error (Runtime_error ("'transpose: internal hd failure", void))
    | (h :: _) -> ok h
  in
  let tl = function
    | [] -> error (Runtime_error ("'transpose: internal tl failure", void))
    | (_ :: tl) -> ok tl
  in
  let rec transpose' acc = function
    | [] -> ok acc
    | [] :: _ -> ok acc
    | m ->
      map_m hd m >>= fun hds ->
      map_m tl m >>= fun tls ->
      transpose' (hds :: acc) tls
  in match ll with
  | List ll ->
    map_m unwrap_list ll >>= fun ll ->
    transpose' [] ll >>| fun t ->
    (List.rev_map make_list t |> make_list)
  | other -> error (Type_mismatch ("list?", ll))
