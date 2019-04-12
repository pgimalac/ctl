open Formule
open Poor_formule
open Kripke

let rec insert_no_dup_in_sorted x xs =
  match xs with
  | [] -> [x]
  |  y :: ys ->
      let b = compare x y in
      if b = -1
      then x :: xs
      else
        if b = 0
        then xs
        else y :: (insert_no_dup_in_sorted x ys)

let opt_map f k =
  match k with
  | Some x -> Some (f x)
  | None -> None

module Make (K : Kripke.K) = struct

  let rec marquage (f : K.SV.elt poor_formule) (m : K.kripke) =
    match f with
    | B b -> M.map (fun _ -> b) m
    | L q ->
       begin
         match q with
         | P q -> M.map (fun (e,_) -> K.SV.mem q e) m
         | N q -> M.map (fun (e,_) -> not (K.SV.mem q e)) m
       end
    | Binop (c, a, b) ->  (* Todo opti ? *)
       let op = getop c in
       let a',b' = marquage a m, marquage b m in
       M.mapi (fun k e -> op e (M.find k b')) a'
    | TempUnop (u, phi) ->
       begin
         match u with
         | AX -> M.map not (marquage (TempUnop (EX, neg phi)) m)
         | EX ->
            let m' = marquage phi m in
            M.map (fun (_,s) -> S.exists (fun e -> M.find e m') s) m
       end
    | TempBinop (b, psi1, psi2) ->
       begin
         match b with
         | AW -> (* A phi W psi == not (E (not psi) U (not psi /\ neg psi))*)
            let npsi2 = neg psi2 in
            M.map not (marquage (TempBinop (EU, npsi2, Binop (And, npsi2, neg psi1))) m)
         | EW -> (* EW == Or (eg psi1, EU psi1 psi2) *)
            let a' = M.map not (marquage (af (neg psi1)) m) in
            let b' = marquage (TempBinop (EU, psi1,psi2)) m in
            M.mapi (fun k e -> e || (M.find k b')) a'
         | EU ->
            let mpsi1 = marquage psi1 m in
            let mpsi2 = marquage psi2 m in
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
                   M.fold (fun q' (_,s) ((newlist,res) as acc) ->
                       (* C'est un prédécesseur que l'on n'a pas vu *)
                       if S.mem q s && (not (snd (M.find q' res)))
                       then
                         let res = M.update q' (opt_map (fun (x,_) -> (x,true))) res in
                         if M.find q' mpsi1
                         then (insert_no_dup_in_sorted q' newlist,res)
                         else (newlist,res)
                       else acc
                     )
                     m (xs,res) in
                 tantque newlist res
            in tantque !todo res
         | AU ->
            let mpsi1 = marquage psi1 m in
            let mpsi2 = marquage psi2 m in
            let todo = ref [] in
            let res = (* Map (res, nb) *)
              M.mapi
                (fun q (_,s) ->
                  if M.find q mpsi2
                  then
                    todo := q :: !todo;
                  (false, S.cardinal s)
                ) m in
            let todo = List.rev !todo in
            let rec tantque todo res =
              match todo with
              | [] -> M.map fst res
              | q::xs ->
                 let res = M.update q (opt_map (fun (_,y) -> (true,y))) res in
                 let (newlist,res) =
                   M.fold (fun q' (_,s) ((newlist,res) as acc)->
                       (* C'est un prédécesseur que l'on n'a pas vu *)
                       if S.mem q s
                       then
                         let newqnb = ref (true,0) in
                         let res =
                           M.update q'
                             (opt_map (fun (x,y) -> newqnb := (x,y-1); !newqnb)) res in
                         if (snd !newqnb = 0) && (M.find q' mpsi1) && not (fst !newqnb)
                         then (insert_no_dup_in_sorted q' newlist,res)
                         else (newlist,res)
                       else acc
                     )
                     m (xs,res) in
                 tantque newlist res
            in tantque todo res
       end

  let check phi g x = M.find x (marquage phi g)
end
