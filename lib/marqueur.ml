open Formule

(* On représente les graphes comme une Map (Int,(Set Int))*)

module L = struct
  type t = int
  let compare = compare
end

module S = Set.Make(L)

module M = Map.Make(L)

(* Prend un graphe et renvoit une map int bool *)
let rec marquageS f m =
  match f with
  | P q -> M.map (S.mem q) m
  | B b -> M.map (fun _ -> b) m
  | SNot q -> M.map not (marquageS q m) (* Todo opti ? *)
  | SBinop (c,a,b) ->  (* Todo opti ? *)
     let op = getop c in
     let a',b' = marquageS a m, marquageS b m in
     M.mapi (fun k e -> op e (M.find k b')) a'

let rec marquage f m =
  match f with
  | State s -> marquageS s m
  | FNot q -> M.map not (marquage q m)
  | FBinop (c,a,b) -> (* Todo opti ? *)
     let op = getop c in
     let a',b' = marquage a m, marquage b m in
     M.mapi (fun k e -> op e (M.find k b')) a'
  | EX psi ->
     let m' = marquageS psi m in
     M.map (fun s -> if S.exists (fun e -> M.find e m') s then true else false) m
  | _ -> failwith "todo"
