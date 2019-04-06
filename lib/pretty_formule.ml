type binop = And | Or | Xor | Impl | Eq
type tempUnop = AX | EX | AF | EF | AG | EG
type tempBinop = EU | AU | EW | AW

type 'a pretty_formule =
  (* Logique propositionnelle *)
  | B of bool
  | P of 'a
  | Not of 'a pretty_formule
  | Binop of binop * 'a pretty_formule * 'a pretty_formule
  (* Combinateurs temporels *)
  | TempUnop of tempUnop * 'a pretty_formule
  | TempBinop of tempBinop * 'a pretty_formule * 'a pretty_formule

let get_string_temp c =
    match c with
    | AX -> "AX"
    | EX -> "EX"
    | AF -> "AF"
    | EF -> "EF"
    | AG -> "AG"
    | EG -> "EG"

let get_string c =
    match c with
    | And -> "∧"
    | Or -> "∨"
    | Impl -> "⇒"
    | Xor -> "⊕"
    | Eq -> "⇔"

let string_of_formule printer f =
  let rec to_string f =
    match f with
    | B b -> if b then "⊤" else "⊥"
    | P p -> printer p
    | Not f -> "¬ (" ^ (to_string f) ^")"
    | Binop(b, phi, psi) ->
       "(" ^ (to_string phi) ^ ") " ^ (get_string b) ^ " (" ^ (to_string psi) ^")"
    | TempUnop(u, phi) -> (get_string_temp u) ^ " (" ^ (to_string phi) ^")"
    | TempBinop(b, phi, psi) ->
       let phi = to_string phi in
       let psi = to_string psi in
       match b with
       | EU -> "E (" ^ phi ^ ") U (" ^ psi ^")"
       | AU -> "A (" ^ phi ^ ") U (" ^ psi ^")"
       | EW -> "E (" ^ phi ^ ") W (" ^ psi ^")"
       | AW -> "A (" ^ phi ^ ") W (" ^ psi ^")"
  in to_string f

let print_formule printer f = print_endline (string_of_formule printer f)

(* prend un tableau pour limiter le temps de génération... *)
let generate_formulas number labels =
  Random.self_init ();
  let len = Array.length labels in
  let rec aux x =
(* plus x est élevé, plus la probabilité de tirer une variable ou un booléen est grande (évite trop de récursion) *)
    let rec constructors = [|
      (fun () -> Binop(And, aux (x + 1), aux (x + 1)));
      (fun () -> Binop(Or, aux (x + 1), aux (x + 1)));
      (fun () -> Binop(Xor, aux (x + 1), aux (x + 1)));
      (fun () -> Binop(Impl, aux (x + 1), aux (x + 1)));
      (fun () -> Binop(Eq, aux (x + 1), aux (x + 1)));
      (fun () -> TempUnop(AX, aux (x + 1)));
      (fun () -> TempUnop(EX, aux (x + 1)));
      (fun () -> TempUnop(AF, aux (x + 1)));
      (fun () -> TempUnop(EF, aux (x + 1)));
      (fun () -> TempUnop(AG, aux (x + 1)));
      (fun () -> TempUnop(EG, aux (x + 1)));
      (fun () -> TempBinop(EU, aux (x + 1), aux (x + 1)));
      (fun () -> TempBinop(AU, aux (x + 1), aux (x + 1)));
      (fun () -> TempBinop(EW, aux (x + 1), aux (x + 1)));
      (fun () -> TempBinop(AW, aux (x + 1), aux (x + 1)));
      (fun () -> Not (aux (x + 1)))
    |] in
    let size = Array.length constructors in
    let n = Random.int (size + x) in
    if n < x then begin
      let v = Random.int (len + 2) in
      if v = 0 then B false
      else if v = 1 then B true
      else P labels.(v - 2)
    end
    else constructors.(n - x) ()
  in List.init number (fun _ -> aux 3)
