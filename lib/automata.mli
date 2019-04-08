open Poor_formule

type 'a pbf =
    P_pbf of 'a
  | Et_pbf of ('a pbf * 'a pbf)
  | Ou_pbf of ('a pbf * 'a pbf)
  | B_pbf of bool
val et : 'a pbf -> 'a pbf -> 'a pbf
val ou : 'a pbf -> 'a pbf -> 'a pbf
val string_of_pbf : ('a -> string) -> 'a pbf -> string
module Make :
  functor (K : Kripke.K) ->
    sig
      val tau :
        int ->
        K.SV.elt poor_formule * K.SV.t ->
        (int * K.SV.elt poor_formule) pbf
    end
module AutomataS :
  sig
    val tau :
      int ->
      (Kripke.KripkeS.SV.elt poor_formule * Kripke.KripkeS.SV.t) ->
      (int * Kripke.KripkeS.SV.elt poor_formule) pbf
  end
val poids : 'a poor_formule -> int
