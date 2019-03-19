open Pretty_formule

let ex1 = "e->-c" (* erreur implique non chaud *)
let ex2 = "EoUe" (* il existe un chemin pour lequel on a ok jusqu'à avoir erreur*)
let ex3 = "EXe" (* il existe un chemin pour lequel le prochain état est erreur*)
let ex4 = "AGEFo" (* à tout moment de tout chemin il y a moyen d'atteindre ok *)

let main =
  List.iter (fun ex ->
    let lexbuf = Lexing.from_string ex in
    let result = Parser.main Lexer.token lexbuf in
        print_formule (fun x -> x) result
  ) [ex1; ex2; ex3; ex4]
