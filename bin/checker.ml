open Lib.Formule
open Lib.Marqueur

(* page 29 *)
(* chaud=1; ok=2; erreur=3 *)
let fig2D1 =
  let adj =
    [ (0,([1],[1;2]))
    ; (1,([0;1],[2]))
    ; (2,([0;2],[3]))
    ]
  in List.fold_left (fun acc (x,(y1,y2)) -> M.add x (S.of_list y1, S.of_list y2) acc) M.empty adj

let phiT = impliesS (P 3) (SNot (P 1))

let main () =
  print_endline (string_of_bool (check (State phiT) fig2D1 1))

let _ = main ()
