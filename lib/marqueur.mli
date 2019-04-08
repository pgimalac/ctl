open Poor_formule

val insert_no_dup_in_sorted : 'a -> 'a list -> 'a list
val opt_map : ('a -> 'b) -> 'a option -> 'b option
module Make :
functor (K : Kripke.K) ->
sig
  val marquage : K.SV.elt poor_formule -> K.kripke -> bool Kripke.M.t
  val check :
    K.SV.elt poor_formule -> K.kripke -> Kripke.M.key -> bool
end
