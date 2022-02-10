(*******************************************)
(*                                         *)
(* Gavin Gray 02.2022                      *)
(*                                         *)
(* Hygienic Racket-like Macro Expander     *)
(* Based off Matthew Flatt's:              *)
(*  "Let's Build a Hygenic Macro Expander" *)
(*      Strange Loop 2016                  *)
(*                                         *)
(*******************************************)

module Err = Types.Err

open struct
  let ( >>= ),( >>| ) = Err.( >>= ), Err.( >>| )
end

let key_to_symbol k =
  Util.make_symbol k

let local_to_symbol id =
  Scope.resolve id >>= (function
      | None ->
        Err.error (Types.Bad_form ("bad binding", id))
      | Some b when Binding.is_local_binding b ->
        Binding.local_binding_key b |> key_to_symbol |> Err.ok
      | Some _ ->
        Err.error (Types.Bad_form ("bad binding", id)))

let rec compile
  = fun s -> match s with
    | s when (Syntax.syntax_e s >>| Util.is_pair) |> Util.or_false ->
      begin match Core.core_form_sym s with
        | None ->
          raise (Err.Unexpected ("not a core form", s))

        | Some "lambda" ->
          let lambda = Util.make_symbol "lambda"
          and id = Util.make_symbol "id"
          and body = Util.make_symbol "body" in
          Match.match_syntax s
            (* '(lambda (id ...) body) *)
            Util.(make_list [ lambda
                            ; make_list [ id; make_symbol "..." ]
                            ; body ]) >>= fun m ->
          m id
          >>= Util.list_map_m local_to_symbol
          >>= fun args ->
          m body >>= compile >>| fun compiled_bdy ->
          Util.(make_list [ lambda
                          ; args
                          ; compiled_bdy
                          ])

        | Some "#%app" ->
          let rest = Util.make_symbol "reset" in
          Match.match_syntax s
            Util.(make_dotted ([ make_symbol "#%app" ], rest)
                 ) >>= fun m ->
          m rest >>= Util.unwrap_list
          >>= Err.map_m compile
          >>| Util.make_list

        | Some "quote" ->
          let quote = Util.make_symbol "quote"
          and datum = Util.make_symbol "datum" in
          Match.match_syntax s Util.(make_list [ quote; datum ]) >>= fun m ->
          m datum >>= Syntax.syntax_to_datum >>| fun dtm ->
          Util.make_list [ quote; dtm ]

        | Some "quote-syntax" ->
          let quote = Util.make_symbol "quote"
          and datum = Util.make_symbol "datum" in
          Match.match_syntax s Util.(make_list [ quote; datum ]) >>= fun m ->
          m datum >>| fun stx ->
          Util.make_list [ quote; stx ]

        | Some core_sym ->
          Err.error (Types.Bad_form
                       ("unrecognized core form", Util.make_symbol core_sym))
      end

    | s when Syntax.is_identifier s ->
      Scope.resolve s >>= (function
          | None ->
            Err.error (Types.Bad_form ("not a reference to a local binding", s))
          | Some b when Binding.is_local_binding b ->
            Binding.local_binding_key b |> key_to_symbol |> Err.ok
          | Some b when Binding.is_core_binding b ->
            let sym = Binding.core_binding_sym b |> Util.make_symbol in
            begin match Hashtbl.find_opt Core.core_primitives sym with
              | None -> raise (Err.Unexpected ("core-binding unbound in namespace", sym))
              |  Some sym -> Err.ok sym
            end
          | Some other -> raise (Err.Unexpected ("unexpected binding resolution", Types.void)))

    | other ->
      Err.error (Types.Bad_form ("bad syntax after expansion", other))

let expand_time_namespace =
  Namespace.base

let run_time_namespace =
  Namespace.base

let expand_time_eval compiled =
  Eval.eval compiled ~env:expand_time_namespace

let run_time_eval compiled =
  Eval.eval compiled ~env:run_time_namespace
