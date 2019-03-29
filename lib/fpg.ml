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

(* type node = {
  state: game_state;
  mutable index: int option;
  mutable onStack: bool;
  mutable lowlink: int;
} *)

(* let to_cfc (m : T.kripke) (start : int) (phi : string formule) : (GM.t * SM.t) array =
  let arr = [||] in
  let rec strong_connect v (index, stack) =
    v.index <- Some index;
    v.lowlink <- index;
    v.onStack <- true;
    let index, stack =
      List.fold_left (
        fun (index, stack) w ->
          if w.index = None
          then
            let index, stack = strong_connect w in
            w.lowlink <- min v.lowlink w.lowlink;
            index, stack
          else if

      ) (index, stack) (gsphi m v.state) in
    ()

 *)
(*
  input: graph G = (V, E)
  output: set of strongly connected components (sets of vertices)

  index := 0
  S := empty stack
  for each v in V do
    if (v.index is undefined) then
      strongconnect(v)
    end if
  end for

  function strongconnect(v)
    v.index := index
    v.lowlink := index
    index := index + 1
    S.push(v)
    v.onStack := true

    for each (v, w) in E do
      if (w.index is undefined) then
        strongconnect(w)
        v.lowlink  := min(v.lowlink, w.lowlink)
      else if (w.onStack) then
        v.lowlink  := min(v.lowlink, w.index)
      end if
    end for

    if (v.lowlink = v.index) then
      start a new strongly connected component
      repeat
        w := S.pop()
        w.onStack := false
        add w to current strongly connected component
      while (w != v)
      output the current strongly connected component
    end if
*)
