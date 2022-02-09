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
    | S_obj (ListT, List ls) ->
      List.for_all p ls
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
    | S_obj (ListT, List (v :: _))
    | S_obj (DottedT, Dotted ((v :: _), _)) ->
      ok v
    | arg -> error (Type_mismatch ("pair?", arg))

and cdr : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (ListT, List (_ :: ls)) ->
      make_list ls |> ok
    | S_obj (DottedT, Dotted ([_], tl)) ->
      ok tl
    | S_obj (DottedT, Dotted ((_ :: ls), tl)) ->
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
    | x, S_obj (ListT, List []) ->
      make_list [x] |> ok
    | x, S_obj (ListT, List ls) ->
      make_list (x :: ls) |> ok
    | x, S_obj (DottedT, Dotted (ls, tl)) ->
      make_dotted (x :: ls, tl) |> ok
    | x, y -> make_dotted ([x], y) |> ok

let list_ref : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | S_obj (ListT, List ls), S_obj (NumT, Num (Number.Int idx)) ->
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
    | S_obj (ListT, List ls) ->
      List.length ls |> Int64.of_int
      |> Util.make_int |> ok
    | ls -> error (Type_mismatch ("list?", ls))

let list_map : (scheme_object -> scheme_object maybe_exn) -> scheme_object -> scheme_object maybe_exn
  = fun f a2 -> match a2 with
    | S_obj (ListT, List ls) ->
      map_m f ls >>| (fun results -> make_list results)
    | ls  -> error (Type_mismatch ("list?", ls))

let pair_append : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | S_obj (ListT, List firstl), S_obj (ListT, List secondl) ->
      List.append firstl secondl |> make_list |> ok
    | S_obj (ListT, List firstl), S_obj (DottedT, Dotted (secondl, rest)) ->
      List.append firstl secondl |> (fun stem ->
          make_dotted (stem, rest)) |> ok
    | ls, other when is_list ls ->
      error (Type_mismatch ("pair?", other))
    | ls, _ -> error (Type_mismatch ("list?", ls))

let vector_ref : scheme_object -> scheme_object -> scheme_object maybe_exn
  = fun a1 a2 -> match a1, a2 with
    | S_obj (VecT, Vec v), S_obj (NumT, Num (Number.Int idx)) ->
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
    | S_obj (VecT, Vec v), S_obj (NumT, Num (Number.Int idx))  ->
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
    | [ S_obj (NumT, Num (Number.Int size)); fill ] ->
      let size = Int64.to_int size in
      ok (make_vector (Vector.make size fill))
    | [ S_obj (NumT, Num (Number.Int size)) ] ->
      let size = Int64.to_int size in
      ok (make_vector (Vector.make size void))
    | [ arg ] -> error (Type_mismatch ("integer?", arg))
    | ls -> error (Arity_mismatch (1, List.length ls, ls))

(* IO primitives *)

let open_input_file : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (StringT, String fn) ->
      ReadPort (open_in fn)
      |> make_port |> ok
    | bad -> error (Type_mismatch ("string?", bad))

let open_output_file : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (StringT, String fn) ->
      WritePort (open_out fn)
      |> make_port |> ok
    | bad -> error (Type_mismatch ("string?", bad))

let close_input_port : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (PortT, Port (ReadPort p)) ->
      begin
        close_in p;
        ok void
      end
    | bad -> error (Type_mismatch ("input-port?", bad))

let close_output_port : scheme_object -> scheme_object maybe_exn
  = function
    | S_obj (PortT, Port (WritePort p)) ->
      begin
        close_out p;
        ok void
      end
    | bad -> error (Type_mismatch ("output-port?", bad))

(* *********************** *)

(* let apply : (scheme_object -> scheme_object maybe_exn) -> scheme_object -> scheme_object maybe_exn
 *   = fun f o -> match o with
 *     | S_obj (ListT, List ls) ->
 *       map_m f ls >>| make_list
 *     | other -> error (Type_mismatch ("list?", o)) *)

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
  | S_obj (ListT, List ll) ->
    map_m unwrap_list ll >>= fun ll ->
    transpose' [] ll >>| fun t ->
    (List.rev_map make_list t |> make_list)
  | other -> error (Type_mismatch ("list?", ll))
