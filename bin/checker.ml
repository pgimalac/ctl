open Lib.Formule
open Lib.Marqueur
open Lib.Parser

let string_of_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; ""
  with End_of_file ->
    close_in chan;
    String.concat "\n" (List.rev !lines)
    
(* page 29 *)
let fig2D1 =
  let lexbuf = Lexing.from_string (string_of_file "graphs/g1.ctl") in
  graph_main Lib.Lexer.token lexbuf
  
  
let phi1 = Binop(Impl, P "erreur", Not (P "chaud"))
let phi2 = TempBinop(EU, P "ok", P "erreur")
let phi3 = TempUnop(EX, P "erreur")
let phi4 = TempUnop(AG, TempUnop(EF, P "ok"))

let main () =
  print_endline (string_of_bool (T.check phi1 fig2D1 0));
  print_endline (string_of_bool (T.check phi2 fig2D1 0));
  print_endline (string_of_bool (T.check phi3 fig2D1 0));
  print_endline (string_of_bool (T.check phi4 fig2D1 0))

let _ = main ()
