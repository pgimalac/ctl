open Lib.Formule
open Lib.Marqueur
open Lib.Kripke

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

let check_formula_in kripke start x =
  let form = parse_formule x in
  print_string ("[" ^ Lib.Pretty_formule.string_of_formule (fun x -> x) form ^ "]");
  print_string " -> ";
  print_string ("[" ^ string_of_formule (fun x -> x) (formule_from_pretty form) ^"]");
  print_endline ":";
  print_endline
    ("* Marquage : " ^ string_of_bool (MarqueurS.check (formule_from_pretty form) kripke start));
  print_endline
    ("* Jeu      : " ^ string_of_bool (Lib.Fpg.check (formule_from_pretty form) kripke start))

let main () =
  print_endline "graph 1:";
  let fig2D1 = extract "graphs/g1.ctl" in
  List.iter (check_formula_in fig2D1 0) (strings_of_file "graphs/f1.ctl");
  print_endline "\ngraph 2:";
  let g2 = extract "graphs/g2.ctl" in
  List.iter (check_formula_in g2 1) (strings_of_file "graphs/f2.ctl")

let _ = main ()

let _ =
  let tr1 = S.of_list [0; 2] in
  let tr2 = S.of_list [0; 3] in
  let tr3 = S.of_list [1; 2; 3] in
  let tr4 = S.of_list [0; 1; 2] in
  let set = Lib.Fpg.GS.empty in
  let cfc = List.map (fun x -> set, x) [tr1; tr2; tr3; tr4] in
  Lib.Fpg.write_cfc_into_file "graphs/graphviz_test" cfc
