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
    int * ((int * K.SV.elt poor_formule) pbf)

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
    let s =
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

  let automtau m s q = Autom.tau (K.deg s m) (q, K.etiquettes s m)

  (* La fonction de transition du jeu*)
  let gsphi (m : K.kripke) ((s,qt) : game_state) : game_state list =
    match qt with
    | Et_pbf (a,b) | Ou_pbf (a,b) ->
       [(s, a); (s, b)]
    | P_pbf (c,q) ->
       let newS = set_nth (c-1) (K.succ s m) in
       [(newS, automtau m newS q)]
    | B_pbf _ -> []

  (* Renvoit peut-être la couleur de l'état *)
  let get_coul s =
    try
      let _ =
        GS.iter
          (function
           | (_,P_pbf (_,x)) -> raise (Found_elem (poids x))
           | _ -> ()) s in
      failwith "get_coul"
    with
    | Found_elem i -> i

  (* Renvoit le joueur correspondant à l'état *)
  let get_player q =
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

  let ex_all p =
    List.fold_left (fun (a,b) x -> let y = p x in a || y, b && y ) (false,true)

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
              let ex,all = use_pred ex_all computed gammabarre xs in
              let b = (* Si j'ai le droit de jouer et qu'un de mes successeurs est gagnant pour moi OU que je n'ai pas le droit mais tous les sucesseurs sont gagnants pour moi *)
                ((get_player (snd v) = gammabarre) && ex) || all in
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
      if GS.cardinal ind = 1 && (let x = GS.min_elt ind in [x] != gsphi m x )
      then
        let elem = GS.min_elt ind in
        let player = get_player (snd elem) in
        let xs = gsphi m elem in
        GM.add
          elem
          (if exists_in_succ computed player xs then player else get_other player)
          computed
      else (* Le poids d'un état au hasard, valide car tous les états ont la même couleur dans le même CFC *)
        let gamma = if (get_coul ind) mod 2 = 0 then Eve else Adam in
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
    insert GM.empty (start, automtau m start phi)

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
    let win = get_win m (CFC.to_cfc gsphi m (start, automtau m start phi)) in
    write_game_into_file filename printer g (Some win)

  (* La fonction de model-checking *)
  let check phi m start =
    Eve = GM.find (start, automtau m start phi) (get_win m (CFC.to_cfc gsphi m (start, automtau m start phi)))
end
