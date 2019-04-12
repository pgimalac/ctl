open Lib.Formule
open Lib.Poor_formule
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
  let poorf = to_poor form in
  print_string ("[" ^ string_of_formule (fun x -> x) form ^ "]");
  print_string " ---> ";
  print_string ("[" ^ string_of_formule (fun x -> x) poorf ^"]");
  print_endline ":";
  let marq = MarqueurS.check poorf kripke start in
  print_endline
    ("* Marquage : " ^ string_of_bool marq);
  let fpg = FpgS.export_game_checked poorf kripke start (fun x -> x) "test" in
  print_endline
    ("* Jeu      : " ^ string_of_bool fpg);
  assert (marq == fpg)

let tests = [("graphs/g1.ctl", "graphs/f1.ctl", 0); ("graphs/g2.ctl", "graphs/f2.ctl", 1); ("graphs/g3.ctl", "graphs/f3.ctl", 0)]

let main () =
  List.iteri (fun i (g, f, start) ->
    let num = string_of_int (i + 1) in
    let s = "graph " ^ num ^ ":" in
    print_endline s;
    let fig = extract g in
    let labels = KripkeS.get_labels fig in
    let check = check_formula_in fig start in
    let file = List.map parse_formule (strings_of_file f) in
    let random_formulas = generate_formulas 5 1000 labels in
    List.iter check (file @ random_formulas);
    print_newline ()
  ) tests

let _ = main ()
