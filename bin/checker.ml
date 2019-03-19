open Lib.Pretty_formule
open Lib.Formule
open Lib.Marqueur

let strings_of_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* page 29 *)
let fig2D1 =
  let lexbuf = Lexing.from_string (String.concat "\n" (strings_of_file "graphs/g1.ctl")) in
  Lib.Parser.graph_main Lib.Lexer.token lexbuf

let fig2D1_formules =
  strings_of_file "graphs/f1.ctl"

let phi1 = Lib.Parser.main Lib.Lexer.token (Lexing.from_string (List.nth fig2D1_formules 0))
let phi2 = Lib.Parser.main Lib.Lexer.token (Lexing.from_string (List.nth fig2D1_formules 1))
let phi3 = Lib.Parser.main Lib.Lexer.token (Lexing.from_string (List.nth fig2D1_formules 2))
let phi4 = Lib.Parser.main Lib.Lexer.token (Lexing.from_string (List.nth fig2D1_formules 3))

let main () =
  print_formule (fun x -> x) phi1;
  print_endline (string_of_bool (T.check (formule_from_pretty phi1) fig2D1 0));
  print_formule (fun x -> x) phi2;
  print_endline (string_of_bool (T.check (formule_from_pretty phi2) fig2D1 0));
  print_formule (fun x -> x) phi3;
  print_endline (string_of_bool (T.check (formule_from_pretty phi3) fig2D1 0));
  print_formule (fun x -> x) phi4;
  print_endline (string_of_bool (T.check (formule_from_pretty phi4) fig2D1 0))

let _ = main ()
