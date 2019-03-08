open Lib.Formule
open Lib.Marqueur

type etat = Chaud | Ok | Erreur

let int_of_etat x =
  match x with
    Chaud -> 1
  | Ok -> 2
  | Erreur -> 3

let compare_etat x y = compare (int_of_etat x) (int_of_etat y)

module T =
  Lib.Marqueur.Make(
      struct
        type t = etat
        let compare = compare_etat
      end) 
  
(* page 29 *)
let fig2D1 =
  let adj =
    [ (0,([Chaud;Ok],[1]))
    ; (1,([Ok],[0;2]))
    ; (2,([Erreur],[0;2]))
    ]
  in List.fold_left (fun acc (x,(y1,y2)) -> M.add x (T.SV.of_list y1, S.of_list y2) acc) M.empty adj

let phiT = impliesS (P Erreur) (SNot (P Chaud))
let psiT = EU (P Ok,P Erreur)
let gammaT = EX (P Erreur)

let main () =
  print_endline (string_of_bool (T.check (State phiT) fig2D1 0));
  print_endline (string_of_bool (T.check psiT fig2D1 0));
  print_endline (string_of_bool (T.check gammaT fig2D1 0))

let _ = main ()
