(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

open Gscm

module List = Util.List

open struct
  let ( >>= ), ( >>=? ) = Types.Err.( >>= ), Types.Err.( >>=? )
end

let repl_fmt = Format.std_formatter

let format_scheme_obj
  = fun fmt s ->
    let open Format in
    let open Types in
    (* NOTE attributes can be used to indicate that the object being printed
     * is nested within a certain environment.
     **)
    let rec fso attributes fmt s =
      let recur = fso attributes
      and recur_with a = fso (a :: attributes)
      and recur_without a = fso (List.remove a attributes) in
      match s with
      | s when Util.is_void s ->
        fprintf fmt "#<void>"
      | S_obj (LambT, Lamb rc) ->
        fprintf fmt "#<procedure %s>"
          (Option.value rc.name ~default:"anonymous")
      | S_obj (ProcT, Proc (name, _)) ->
        fprintf fmt "#<procedure %s>" name
      | S_obj (BoolT, Bool true) ->
        fprintf fmt "#t"
      | S_obj (BoolT, Bool false) ->
        fprintf fmt "#f"
      | S_obj (StringT, String s) ->
        fprintf fmt "\"%s\"" s
      | S_obj (IdT, Id s) ->
        fprintf fmt "%s" s
      | S_obj (NumT, Num (Number.Int i)) ->
        fprintf fmt "%Li" i
      | S_obj (StxT, Stx s) ->
        if List.mem `Syntax attributes then
          fprintf fmt "%a" recur s.e
        else fprintf fmt "#<syntax %a>" (recur_with `Syntax) s.e
      (* different quoted forms *)
      | S_obj (ListT, List [S_obj (IdT, Id "quote"); rhs]) ->
        fprintf fmt "'%a" (recur_with `Quote) rhs
      | S_obj (ListT, List [S_obj (IdT, Id "quasiquote"); rhs]) ->
        fprintf fmt "`%a" (recur_with `Quote) rhs
      | S_obj (ListT, List [S_obj (IdT, Id "unquote"); rhs]) ->
        fprintf fmt ",%a" (recur_without `Quote) rhs
      | S_obj (ListT, List ls) ->
        fprintf fmt "(%a)"
          (pp_print_list ~pp_sep:pp_print_space recur)
          ls
      | S_obj (VecT, Vec vector) ->
        fprintf fmt "#(%a)"
          (pp_print_list ~pp_sep:pp_print_space recur)
          (Vector.to_list vector)
      | S_obj (DottedT, Dotted (ls, tl)) ->
        fprintf fmt "(%a . %a)"
          (pp_print_list ~pp_sep:pp_print_space
             recur) ls
          recur tl
      | _ -> raise (Err.Unexpected ("fmt_scheme_object unexpected object", void))
    in fprintf fmt "%a" (fso []) s

(* I'm not very familiar with the Format module but I believe the <2>
 * within the nested boxes are unnecessary for indentations.
 ***)
(* FIXME the XXX within the messages should represent scope e.g. a function name *)
let format_runtime_exn
  = fun fmt exc ->
    let open Format in
    let open Types in
    let ind fmt s = match s with
      | Runtime_error (msg, id) ->
        fprintf fmt "@[<v 2>%s: %s;@,%s@,error identifier: %a@]"
          "XXX" "runtime error" msg
          format_scheme_obj id

      | Arity_mismatch (expected, given, objs) ->
        fprintf fmt "@[<v 2>%s: %s;@,expected: %d@,given: %d@,args: %a@]"
          "XXX" "arity mismatch"
          expected given
          (pp_print_list ~pp_sep:pp_print_space format_scheme_obj) objs

      | Type_mismatch (contract, obj) ->
        fprintf fmt "@[<v 2>%s: %s;@,predicate: %s@,unsatisfied by: %a@]"
          "XXX" "contract violation"
          contract
          format_scheme_obj obj

      | Free_var (var, None) ->
        fprintf fmt "@[<v 2>%s: %s;@,%s@]"
          var "undefined"
          "cannot reference an identifier before its definition"

      | Free_var (var, Some obj) ->
        fprintf fmt "@[<v 2>%s: %s;@,%s@,%a@]"
          var "undefined"
          "cannot reference an identifier before its definition"
          format_scheme_obj obj

      | Bad_form (msg, obj) ->
        fprintf fmt "@[<v 2>%s:@ %s;@,%s;@,found at: %a@]"
          "XXX" "bad form" msg
          format_scheme_obj obj

      | Parser str ->
        fprintf fmt "@[<v 2>%s: %s;@]@,@[<v 2>%s@]"
          "XXX" "syntax error" str
    in fprintf fmt "@[<v 2>%a@]" ind exc

let format_internal_exn
  = fun tup ->
    let open Format in
    let fie fmt (msg, obj) =
      fprintf fmt
        "@[<v 2>%s: %s;@,%s@, scheme_object: %a@]"
        "INTERNAL"
        "unexpected internal error"
        msg
        format_scheme_obj obj
    in
    begin
      fprintf repl_fmt "%a" fie tup;
      print_newline ();
      print_flush ()
    end

let display_result v =
  if not (Util.is_void v) then
    begin
      Format.fprintf repl_fmt "@[%a@]"
        format_scheme_obj v;
      Format.print_flush ();
      print_newline ()
    end

let display_exn e =
  format_runtime_exn repl_fmt e;
  Format.print_flush ();
  print_newline ()

(* MAIN LOOP *)

let start () =

  let do_eval_print scm_obj =
    try
      (Expand_main.expand_expression scm_obj
       >>= Expand_main.compile
       >>=? (Cfg.is_eval_all ()))
        Expand_main.eval
      |> begin function
        | Ok vl ->
          display_result vl
        | Error e ->
          display_exn e
      end
    with
      Types.Err.Unexpected tup ->
      format_internal_exn tup
  in

  let rec loop () =
    print_string "> ";
    (* TODO instead of reading a single line, allow the user
     * to type an expression accross multiple lines and only capture
     * once all parens are close
     ***)
    begin match read_line () |> Parser.scheme_object_of_string with
      | Ok asts ->
        List.iter do_eval_print asts;
        loop ()
      | Error e ->
        display_exn e;
        loop ()
    end

  in
  Format.pp_set_geometry ~max_indent:6 ~margin:25 repl_fmt;
  Printf.printf "Welcome to GScheme v0.0.1\n";
  Expand_main.register ();
  Cfg.turn_eval_off (); (* FIXME remove after testing *)
  loop ()
