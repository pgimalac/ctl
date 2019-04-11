open Formule
open Poor_formule
open Automata
open Kripke
open Either
open Cfc

module Make (K : Kripke.K) = struct

  (* Le type de l'automate du jeu *)
  module Autom = Automata.Make(K)

  (* Les états du jeu *)
  type game_state =
    int * (K.SV.elt poor_formule, (int * K.SV.elt poor_formule) pbf) either

  module T =
    struct
      type t = game_state
      let compare = compare
    end

  module CFC = Cfc.Make(K)(T)

  module GM = CFC.GM
  (* Un type de Set avec comme clé des états du jeu *)
  module GS = CFC.GS
            
  (* Le type du jeu *)
  type game = GS.t GM.t

  type winner = Adam | Eve

  (* Convertit un état du jeu en string *)
  let string_of_state i f =
    let s = match f with
      | Left f -> string_of_formule (fun x -> x) f
      | Right f ->
         string_of_pbf
           (fun (i,j) -> "(" ^ string_of_int i ^ ", " ^  string_of_formule (fun x -> x) j ^")") f
    in string_of_int i ^ ", " ^ s

  (* Renvoit le n-ième élément d'un Set *)
  exception Found_elem of int
  let set_nth n s =
    try
      let _ = S.fold (fun x acc -> if acc = n then raise (Found_elem x) else acc+1) s 0 in
      failwith "elem not in set"
    with
    | Found_elem i -> i

  (* La fonction de transition du jeu*)
  let gsphi (m : K.kripke)  ((s,qt) : game_state) : game_state list =
    match qt with
    | Left q ->
       [(s, Right (Autom.tau (K.deg s m) (q, K.etiquettes s m)))]
    | Right t ->
       match t with
       | Et_pbf (a,b) | Ou_pbf (a,b) ->
          [(s, Right a); (s, Right b)]
       | P_pbf (c,q) ->
          [(set_nth (c-1) (K.succ s m),Left q)]
       | B_pbf _ -> []

  (* Renvoit peut-être la couleur de l'état *)
  let get_coul ((_,c) : game_state) =
    match c with
    | Left x -> Some (poids x)
    | Right _ -> None

  (* Renvoit le joueur correspondant à l'état *)
  let get_player x =
    match x with
    | Left _ -> Eve
    | Right q ->
       match q with
       | B_pbf false | Ou_pbf (_,_) -> Eve
       | _ -> Adam

  (* Renvoit l'autre joueur *)
  let get_other x =
    match x with
    | Eve -> Adam
    | Adam -> Eve

  (* Aux func *)
  let use_pred f computed e = f (fun x -> GM.find_opt x computed = Some e)

  (* Regarde si il y a une transition gagnante pour e dans une liste *)
  let exists_in_succ = use_pred List.exists

  (* Regarde si toutes les transitions sont gagnantes pour e dans une liste *)
  let all_succ = use_pred List.for_all

  (* Fonction de recherche de point fixe où l'on essaye d'attribuer les états à gammabarre *)
  let propagate_gammabare m ind gammabarre  =
    let rec aux computed =
      (* Les états de la CFC où gammabarre va forcément gagner *)
      let interesting_states =
        GS.fold
          (fun v acc ->
            if GM.mem v computed
            then acc
            else
              let xs = gsphi m v in
              let b = (* Si j'ai le droit de jouer et qu'un de mes successeurs est gagnant pour moi OU que je n'ai pas le droit mais tous les sucesseurs sont gagnants pour moi *)
                (get_player (snd v) = gammabarre && exists_in_succ computed gammabarre xs)
                || all_succ computed gammabarre xs in
              if b then v::acc else acc
          )
          ind
          [] in
      match interesting_states with
      | [] -> computed (* Point fixe atteint *)
      | _  ->
         aux
           (List.fold_left (fun acc v -> GM.add v gammabarre acc) computed interesting_states)
    in aux

  let update_option v x =
    match x with
    | None -> Some v
    | _ -> x

  (*
    - m représente la structure de Kripke
    - cfc représente les cfc calculées DANS L'ORDRE TOPOLOGIQUE INVERSE

   Renvoit une Map indexé par les états et pointant vers le gagnant
   *)
  let get_win (m : K.kripke) (cfc : (GS.t (* états *) * S.t (* succ *)) list) : winner GM.t =
    (* Fonction appelée sur chaque CFC avec un accumulateur représentant la réponse "partielle" *)
    let aux computed (ind,_) =
      let gamma' =
        if GS.cardinal ind = 1
        then None
        else get_coul (GS.min_elt ind) in (* Le poids d'un état au hasard, valide car tous les états ont la même couleur dans la CFC *)
      match gamma' with
      | None -> (* Il n'y a pas de boucles, c'est un état seul *)
         let elem = GS.min_elt ind in
         let player = get_player (snd elem) in
         let xs = gsphi m elem in
         GM.add
           elem
           (if exists_in_succ computed player xs then player else get_other player)
           computed
      | Some x ->
         let gamma = if x mod 2 = 0 then Eve else Adam in
         let gammabarre = get_other gamma in
         GS.fold
           (fun v acc -> GM.update v (update_option gamma) acc)
           ind
           (propagate_gammabare m ind gammabarre computed)
    in List.fold_left aux GM.empty cfc

  (* Génère totalement un jeu fini *)
  let gen_all_game (m : K.kripke) (phi : K.SV.elt poor_formule) (start : int) : game =
    let rec insert res gs =
      if GM.mem gs res
      then res
      else
        let xs = gsphi m gs in
        let res = GM.add gs (GS.of_list xs) res in
        List.fold_left insert res xs in
    insert GM.empty (start,Left phi)

  (* Permet d'écrire un jeu dans un fichier au format DOT *)
  let write_game_into_file file printer (game : game) (sol : (winner GM.t) option) =
    let get_ind x g =
      snd (GM.fold (fun v _ ((b,j) as acc)-> if b then acc else (x=v,j+1)) g (false,0)) in
    let st_out = open_out file in
    Printf.fprintf st_out "digraph {\n";
    let all_states = ref GS.empty in
    GM.iter (fun x v ->
        if v != GS.empty
        then
          all_states := GS.add x !all_states;
        let left = string_of_int (get_ind x game) in
        let right =
          GS.fold
            (fun x acc ->
              all_states := GS.add x !all_states;
              (string_of_int (get_ind x game) ^ " ") ^ acc)
            v "" in
        Printf.fprintf st_out "   %s -> { %s};\n" left right
      ) game;
    GS.iter
      (fun ((i,j) as x) ->
        let left = string_of_int (get_ind x game) in
        let right = string_of_state i (printer j) in
        match sol with
        | None -> Printf.fprintf st_out "   %s [label=\"%s\"];\n" left right
        | Some sol ->
           let right' =
             if GM.find x sol = Eve
             then "green"
             else "red" in
           Printf.fprintf st_out "   %s [label=\"%s\", color=%s];\n" left right right'
      )
      !all_states;
    Printf.fprintf st_out "}\n";
    close_out st_out

  (* Permet de créer un jeu de partié faible, le résout et l'écrit au format DOT dans un fichier *)
  let export_game_checked phi m start printer filename =
    let g = gen_all_game m phi start in
    let win = get_win m (CFC.to_cfc gsphi m (start, Left phi)) in
    write_game_into_file filename printer g (Some win)

  (* La fonction de model-checking *)
  let check phi m start =
    Eve = GM.find (start, Left phi) (get_win m (CFC.to_cfc gsphi m (start, Left phi)))
end
