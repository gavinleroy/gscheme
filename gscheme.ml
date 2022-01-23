(*******************************************)
(*                                         *)
(* Gavin Gray 01.2022                      *)
(*                                         *)
(* GScheme                                 *)
(*                                         *)
(*******************************************)

let () =
  match Sys.argv with
  | [|_|] -> Repl.start ()
  | _ -> print_endline "unsupported arg"


