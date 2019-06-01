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

let tests = [("graphs/g1.ctl", "graphs/f1.fctl", 0); ("graphs/g2.ctl", "graphs/f2.fctl", 1); ("graphs/g3.ctl", "graphs/f3.fctl", 0)]

let print_help () =
  print_endline
    "USAGE:\n\
     run   graph [number [prof]]\n\
     check graph start file\
     "

exception Found of int

let main () =
  let len = Array.length Sys.argv in
  if len < 2 || Sys.argv.(1) = "help"
  then print_help ()
  else
    Random.self_init ();
    match Sys.argv.(1) with
    | "run"   ->
       if len < 3
       then print_help ()
       else
         let fig = extract Sys.argv.(2) in
         let labels = KripkeS.get_labels fig in
         let start_ind = Random.int (M.cardinal fig) in
         let start =
           let u = ref 0 in
           try
             M.iter (fun e _ -> if !u = start_ind then raise (Found e) else u := !u + 1) fig;
             0
           with
           | Found e -> e in
         let check = check_formula_in fig start in
         let number = if len > 3 then int_of_string (Sys.argv.(3)) else 100 in
         let prof = if len > 4 then int_of_string (Sys.argv.(4)) else 4 in
         let random_formulas = generate_formulas prof number labels in
         List.iter check (random_formulas)
    | "check" ->
       if len < 5
       then print_help ()
       else
         let fig = extract Sys.argv.(2) in
         let file = List.map parse_formule (strings_of_file Sys.argv.(4)) in
         let check = check_formula_in fig (int_of_string Sys.argv.(3)) in
         List.iter check file
    | _ -> print_help ()

let _ = main ()
