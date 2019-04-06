open Lib.Formule
open Lib.Kripke

module MarqueurS = Lib.Marqueur.Make(KripkeS)
module FpgS = Lib.Fpg.Make(KripkeS)

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

let check_formula_in kripke start form =
  print_string ("[" ^ Lib.Pretty_formule.string_of_formule (fun x -> x) form ^ "]");
  print_string " -> ";
  print_string ("[" ^ string_of_formule (fun x -> x) (formule_from_pretty form) ^"]");
  print_endline ":";
  print_endline
    ("* Marquage : " ^ string_of_bool (MarqueurS.check (formule_from_pretty form) kripke start));
  print_endline
    ("* Jeu      : " ^ string_of_bool (FpgS.check (formule_from_pretty form) kripke start))

let tests = [("graphs/g1.ctl", "graphs/f1.ctl", 0); ("graphs/g2.ctl", "graphs/f2.ctl", 1); ("graphs/g3.ctl", "graphs/f3.ctl", 0)]

let main () =
  List.iteri (fun i (g, f, start) ->
    let num = string_of_int (i + 1) in
    let s = "graph " ^ num ^ ":" in
    print_endline s;
    let fig = extract g in
    let check = check_formula_in fig start in
    let file = List.map parse_formule (strings_of_file f) in
    List.iter check file;
    print_newline ()
  ) tests

let _ = main ()
