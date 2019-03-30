open Formule
open Automata
open Marqueur

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

type game_state =
  int * (T.SV.elt formule, (int * T.SV.elt formule) pbf) either

let deg s (m : T.kripke) =
  S.cardinal (snd (M.find s m))

exception Found_elem of int

let set_nth n s =
  try
    let _ = S.fold (fun x acc -> if acc = n then raise (Found_elem x) else acc+1) s 0 in
    failwith "elem not in set"
  with
  | Found_elem i -> i

let gsphi (m : T.kripke)  ((s,qt) : game_state) : game_state list =
  match qt with
  | Left q ->
     [(s, Right (tau (deg s m) (q, fst (M.find s m))))]
  | Right t ->
     match t with
     | Et_pbf (a,b) | Ou_pbf (a,b) ->
        [(s, Right a); (s, Right b)]
     | P_pbf (c,q) ->
        [(set_nth c (snd (M.find s m)),Left q)]
     | B_pbf _ -> []

module T =
  struct
    type t = game_state
    let compare = compare
  end

module GM = Map.Make(T)
module GS = Set.Make(T)

let get_coul ((_,c) : game_state) =
  match c with
  | Left x -> Some (poids x)
  | Right _ -> None

type winner = Adam | Eve

let get_player x =
  match x with
  | Left _ -> Eve
  | Right q ->
     match q with
     | B_pbf false | Ou_pbf (_,_) -> Eve
     | _ -> Adam

let fromSome x =
  match x with
  | None -> failwith "fromSome"
  | Some x -> x

let get_other x =
  match x with
  | Eve -> Adam
  | Adam -> Eve

(*
- cfc représente les cfc calculées DANS L'ORDRE TOPOLOGIQUE INVERSE
- ind est l'indice de la cfc que l'on regarde.
- computed représente une array avec les réponses.
- num est l'indice de actual dans cfc.
 *)
let get_win (m : Marqueur.T.kripke) (cfc : (GS.t (* états *) * S.t (* succ *)) list) : winner GM.t =
  let aux computed ind =
    (* Regarde si il y a une transition gagnante pour e dans une liste *)
    let exists_in_succ e =
      List.exists (fun x -> GM.find x computed = Some e) in
    let gamma' = get_coul (GS.min_elt (fst ind)) in (* Le poids d'un état au hasard, valide car tous les états ont la même couleur dans la CFC *)
    match gamma' with
    | None -> (* Il n'y a pas de boucles, c'est un état seul *)
       assert (GS.cardinal (fst ind) = 1); (* on vérifie *)
       let elem = GS.min_elt (fst ind) in
       let player = get_player (snd elem) in
       let xs = gsphi m elem in
       GM.add
         elem
         (Some (if exists_in_succ player xs then player else (get_other player)))
         computed
    | Some x ->
       let gamma = if x mod 2 = 0 then Eve else Adam in
       let gammabarre = get_other gamma in
       let combi (v : game_state) acc =
         if get_player (snd v) = gammabarre
         then
           let xs = gsphi m v in
           let b = exists_in_succ gammabarre xs in
           let res = GM.add v (Some (if b then gammabarre else gamma)) acc in
           res
         else
           GM.add v (Some gamma) acc
       in GS.fold combi (fst ind) computed
  in GM.map fromSome (List.fold_left aux GM.empty cfc)

let check phi m i = failwith "todo"
