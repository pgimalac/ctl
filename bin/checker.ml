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
let extract x =
  let lexbuf = Lexing.from_string (String.concat "\n" (strings_of_file x)) in
  Lib.Parser.graph_main Lib.Lexer.token lexbuf

let parse_formule x = Lib.Parser.main Lib.Lexer.token (Lexing.from_string x)

let check_forumla_in kripke start x =
  print_endline (string_of_bool (T.check (formule_from_pretty (parse_formule x)) kripke start))

let main () =
  let fig2D1 = extract "graphs/g1.ctl" in
  List.iter (check_forumla_in fig2D1 0) (strings_of_file "graphs/f1.ctl");
  print_endline "\ngraph 2:";
  let g2 = extract "graphs/g2.ctl" in
  List.iter (check_forumla_in g2 1) (strings_of_file "graphs/f2.ctl")

let _ = main ()
