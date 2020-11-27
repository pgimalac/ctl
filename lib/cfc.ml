open Poor_formule
open Kripke

(*
from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm#The_algorithm_in_pseudocode
 *)

module Make (K : Kripke.K) (T : Set.OrderedType) = struct
  (* Un type de Map avec comme clé des états du jeu *)
  module GM = Map.Make (T)

  (* Un type de Set avec comme clé des états du jeu *)
  module GS = Set.Make (T)

  type node = {
    state : T.t;
    transitions : T.t list;
    mutable cfc : int;
    mutable index : int option;
    mutable onStack : bool;
    mutable lowlink : int;
  }

  let new_node state_list state =
    {
      state;
      transitions = state_list;
      cfc = -1;
      index = None;
      onStack = false;
      lowlink = -1;
    }

  let partition_while predicate =
    let rec aux partition = function
      | h :: q ->
          if predicate h then (List.rev (h :: partition), q)
          else aux (h :: partition) q
      | [] -> (List.rev partition, [])
    in
    aux []

  (* the main part of the algorithm *)
  let rec strong_connect map v (cfc, index, stack) =
    (* set the nodes's values *)
    v.index <- Some index;
    v.lowlink <- index;
    v.onStack <- true;
    (* for each transition from v, if not already done call strong_connect on the target and then update the node *)
    let cfc, index, stack =
      List.fold_left
        (fun acc w_state ->
          let w = GM.find w_state map in
          match w.index with
          | None ->
              let acc = strong_connect map w acc in
              v.lowlink <- min v.lowlink w.lowlink;
              acc
          | Some ind ->
              if w.onStack then v.lowlink <- min v.lowlink ind;
              acc)
        (cfc, index + 1, v :: stack)
        v.transitions
    in
    (* when done with the neighbours, if v is the root of a cfc then for each w in the stack, pop it and add it to the current cfc *)
    match v.index with
    | Some ind ->
        if v.lowlink = ind then
          let to_add, stack = partition_while (( = ) v) stack in
          let to_add =
            List.map
              (fun w ->
                w.onStack <- false;
                w.state)
              to_add
          in
          (GS.of_list to_add :: cfc, index, stack)
        else (cfc, index, stack)
    | None -> failwith "Not possible"

  (* to avoid any warning *)

  (* Renvoit les CFC dans l'ordre topologique INVERSE *)
  let to_cfc (gsphi : K.kripke -> T.t -> T.t list) (m : K.kripke) (start : T.t)
      : (GS.t * S.t) list =
    (* used to fill a map that associate each state to its node *)
    let rec fill_map map state =
      if GM.mem state map then map
      else
        let state_list = gsphi m state in
        let map = GM.add state (new_node state_list state) map in
        List.fold_left fill_map map state_list
    in
    let map = fill_map GM.empty start in
    (* for each node, if not already done call strong_connect *)
    let cfc, _, _ =
      GM.fold
        (fun _ v a -> if v.index = None then strong_connect map v a else a)
        map ([], 0, [])
    in
    (* set the cfc's number on each node *)
    List.iteri
      (fun i s ->
        GS.iter
          (fun st ->
            let node = GM.find st map in
            node.cfc <- i)
          s)
      cfc;
    (* eventually create the set of transitions for each cfc *)
    let lst =
      List.map
        (fun cfc ->
          let set =
            GS.fold
              (fun st set ->
                let node = GM.find st map in
                List.fold_left
                  (fun set st ->
                    let node = GM.find st map in
                    S.add node.cfc set)
                  set node.transitions)
              cfc S.empty
          in
          (cfc, set))
        cfc
    in
    List.rev lst

  (* Permet d'écrire les CFC dans un fichier au format DOT *)
  let write_cfc_into_file file cfc =
    let st_out = open_out file in
    Printf.fprintf st_out "digraph {\n";
    List.iteri
      (fun i (_, transitions) ->
        if transitions != S.empty then
          let left = string_of_int i in
          let right =
            S.fold (fun e -> ( ^ ) (string_of_int e ^ " ")) transitions ""
          in
          Printf.fprintf st_out "    %s -> { %s}\n" left right)
      cfc;
    Printf.fprintf st_out "}\n";
    close_out st_out
end
