(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

module List = Util.List

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

      | Proc (name, _) ->
        fprintf fmt "#<procedure %s>"
          (Option.value ~default:"anonymous" name)

      | Bool true -> fprintf fmt "#t"
      | Bool false -> fprintf fmt "#f"
      | String s -> fprintf fmt "\"%s\"" s
      | Id s -> fprintf fmt "%s" s
      | Num (Number.Int i) ->
        fprintf fmt "%Li" i
      | Stx s ->
        if List.mem `Syntax attributes then
          fprintf fmt "%a" recur s.e
        else fprintf fmt "#<syntax %a>" (recur_with `Syntax) s.e
      (* different quoted forms *)
      | List [Id "quote"; rhs] ->
        fprintf fmt "'%a" (recur_with `Quote) rhs
      | List [Id "quasiquote"; rhs] ->
        fprintf fmt "`%a" (recur_with `Quote) rhs
      | List [Id "unquote"; rhs] ->
        fprintf fmt ",%a" (recur_without `Quote) rhs
      | List ls ->
        fprintf fmt "(%a)"
          (pp_print_list ~pp_sep:pp_print_space recur)
          ls
      | Vec vector ->
        fprintf fmt "#(%a)"
          (pp_print_list ~pp_sep:pp_print_space recur)
          (Vector.to_list vector)
      | Dotted (ls, tl) ->
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

let format_internal_exn_endline
  = fun tup ->
    let fie fmt (msg, obj) =
      Format.fprintf fmt
        "@[<v 2>%s: %s;@,%s@, scheme_object: %a@]"
        "INTERNAL"
        "unexpected internal error"
        msg
        format_scheme_obj obj
    in
    begin
      Format.fprintf repl_fmt "%a" fie tup;
      Format.print_newline ();
      Format.print_flush ()
    end

let display_scm_obj o =
  Format.fprintf repl_fmt "@[%a@]"
    format_scheme_obj o

let display_scm_obj_endline o =
  display_scm_obj o;
  Format.print_newline ();
  Format.print_flush ()

let display_exn_endline e =
  format_runtime_exn repl_fmt e;
  Format.print_flush ();
  Format.print_newline ()

let display_result ?(ignore = fun _ -> false) = function
  | Ok v ->
    if not (Util.is_void v) then
      Format.fprintf repl_fmt "@[%a@]"
        format_scheme_obj v
  | Error e ->
    display_exn_endline e

let display_result_endline ?(ignore = fun _ -> false) v =
  display_result ~ignore:ignore v;
  Format.print_flush ();
  print_newline ()

let print s =
  Format.fprintf repl_fmt "%s" s

let print_flush s =
  print s;
  Format.print_flush ()

let print_endline s =
  print_flush s;
  Format.print_newline ()

let force_newline () =
  Format.print_newline ()

let read_expr () =
  read_line ()

let init () =
  Format.pp_set_geometry ~max_indent:6 ~margin:25 repl_fmt;
