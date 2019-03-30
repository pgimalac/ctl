open Formule
open Automata
open Marqueur

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

type game_state =
  int * (T.SV.elt formule, (int * T.SV.elt formule) pbf) either

let print_state i f =
  let s = match f with
  | Left f -> string_of_formule (fun x -> x) f
  | Right (c, f) -> "(" ^ (string_of_int c) ^ ", " ^ (string_of_formule (fun x -> x) f) ^ ")"
  in string_of_int i ^ " : " ^ s

let deg s (m : T.kripke) =
  S.cardinal (snd (M.find s m))

exception Found_elem of int

let set_nth n s =
  try
    let _ = S.fold (fun x acc -> if acc = n then raise (Found_elem x) else acc+1) s 0 in
    failwith "elem not in set"
  with
  | Found_elem i -> i

let gsphi (m : Marqueur.T.kripke)  ((s,qt) : game_state) : game_state list =
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

(*
from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm#The_algorithm_in_pseudocode
*)

type node = {
  state: game_state;
  transitions: game_state list;
  mutable cfc: int;
  mutable index: int option;
  mutable onStack: bool;
  mutable lowlink: int;
}

let new_node m state =
  { state = state; transitions = gsphi m state; cfc = -1; index = None; onStack = false; lowlink = -1 }

let partition_while predicate =
  let rec aux partition l =
    match l with
    | h :: q ->
        if predicate h
        then List.rev (h :: partition), q
        else aux (h :: partition) q
    | [] -> List.rev partition, []
  in aux []

(* the main part of the algorithm *)
let rec strong_connect map v (cfc, index, stack) =
  (* set the nodes's values *)
  v.index <- Some index;
  v.lowlink <- index;
  v.onStack <- true;
  (* for each transition from v, if not already done call strong_connect on the target and then update the node *)
  let cfc, index, stack =
    List.fold_left (
      fun (cfc, index, stack) w_state ->
        let w = GM.find w_state map in
        match w.index with
        | None ->
            let cfc, index, stack = strong_connect map w (cfc, index, stack) in
            v.lowlink <- min v.lowlink w.lowlink;
            cfc, index, stack
        | Some ind ->
            if w.onStack
            then
              v.lowlink <- min v.lowlink ind;
            cfc, index, stack
    ) (cfc, index + 1, v :: stack) v.transitions in
  (* when done with the neighbours, if v is the root of a cfc then for each w in the stack, pop it and add it to the current cfc *)
  match v.index with
  | Some ind ->
      if v.lowlink = ind
      then
        let to_add, stack = partition_while ((=) v) stack in
        let to_add = List.map (fun w -> w.onStack <- false; w.state) to_add in
        let set = GS.of_list to_add in
        (set :: cfc), index, stack
      else cfc, index, stack
  | None -> failwith "Not possible" (* to avoid any warning *)

let to_cfc (m : Marqueur.T.kripke) (start : int) (phi : string formule) : (GS.t * S.t) list =
  (* used to fill a map that associate each state to its node *)
  let rec fill_map map state =
    if GM.mem state map
    then map
    else
      let map = GM.add state (new_node m state) map in
      let state_list = gsphi m state in
      List.fold_left fill_map map state_list
  in
  let map = fill_map GM.empty (start, Left phi) in
  (* for each node, if not already done call strong_connect *)
  let cfc, _, _ = GM.fold (fun _ v a ->
    if v.index = None
    then strong_connect map v a
    else a
  ) map ([], 0, []) in
  (* set the cfc's number on each node *)
  List.iteri (fun i s ->
    GS.iter (fun st -> let node = GM.find st map in node.cfc <- i) s
  ) cfc;
  (* eventually create the set of transitions for each cfc *)
  List.map (fun cfc ->
    let set = GS.fold (fun st set ->
      let node = GM.find st map in
      List.fold_left (fun set st ->
        let node = GM.find st map in
        S.add node.cfc set
      ) set node.transitions
    ) cfc S.empty in
    cfc, set
  ) cfc

let write_cfc_into_file file cfc =
  let st_out = open_out file in
  Printf.fprintf st_out "digraph {\n";
  List.iteri (fun i (_, transitions) ->
    if transitions != S.empty
    then
      let left = string_of_int i in
      let right = S.fold (fun e -> (^) (string_of_int e ^ " ")) transitions "" in
      Printf.fprintf st_out "    %s -> { %s}\n" left right
  ) cfc;
  Printf.fprintf st_out "}\n";
  close_out st_out
