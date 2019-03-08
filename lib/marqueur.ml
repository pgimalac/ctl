open Formule

(* On représente les graphes comme une Map (Int,(Set Int,Set Int))*)

module L = struct
  type t = int
  let compare = compare
end

module S = Set.Make(L)
module M = Map.Make(L)
  
module type VARIABLES = sig
  type t
  val compare : t -> t -> int
end

module Make (V : VARIABLES) = struct

  module SV = Set.Make(V)

  let opt_map f k =
    match k with
    | Some x -> Some (f x)
    | None -> None

  (* Prend un graphe et renvoit une map int bool *)
  let rec marquageS f m =
    match f with
    | P q -> M.map (fun (e,_) -> SV.mem q e) m
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
       M.map (fun (_,s) -> S.exists (fun e -> M.find e m') s) m
    | EU (psi1,psi2) ->
       let mpsi1 = marquageS psi1 m in
       let mpsi2 = marquageS psi2 m in
       let todo = ref [] in (* todo est L *)
       let res = (* Map (res,dejavu) *)
         M.mapi
           (fun i _ ->
             if M.find i mpsi2
             then
               begin
                 todo := i :: !todo;
                 (false,true)
               end
             else (false,false)) m in
       let rec tantque todo res =
         match todo with
         | [] -> M.map fst res
         | q::xs ->
            let res = M.update q (opt_map (fun (_,y) -> (true,y))) res in
            let (newlist,res) =
              M.fold (fun q' (_,s) (newlist,res) ->
                  (* C'est un prédécesseur que l'on n'a pas vu *)
                  if S.mem q s && (not (snd (M.find q' res)))
                  then
                    let res = M.update q' (opt_map (fun (x,_) -> (x,true))) res in
                    if M.find q' mpsi1
                    then (q'::newlist,res)
                    else (newlist,res)
                  else (newlist,res)
                )
                m (xs,res) in
            tantque newlist res
       in tantque !todo res
    | AU (psi1,psi2) ->
       let mpsi1 = marquageS psi1 m in
       let mpsi2 = marquageS psi2 m in
       let todo = ref [] in
       let res = (* Map (rex, nb) *)
         M.mapi
           (fun q (_,s) ->
             if M.find q mpsi2
             then
               todo := q :: !todo;
             (false,S.cardinal s)
           ) m in
       let rec tantque todo res =
         match todo with
         | [] -> M.map fst res
         | q::xs ->
            let res = M.update q (opt_map (fun (_,y) -> (true,y))) res in
            let (newlist,res) =
              M.fold (fun q' (_,s) (newlist,res) ->
                  (* C'est un prédécesseur que l'on n'a pas vu *)
                  if S.mem q s
                  then
                    let newqnb = ref (true,0) in
                    let res = M.update q' (opt_map (fun (x,y) -> newqnb := (x,y-1); !newqnb)) res in
                    if (snd !newqnb = 0) && (M.find q' mpsi1) && not (fst !newqnb)
                    then (q'::newlist,res)
                    else (newlist,res)
                  else (newlist,res)
                )
                m (xs,res) in
            tantque newlist res
       in tantque !todo res

  let check phi g x = M.find x (marquage phi g)

end
