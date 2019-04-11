open Automata
open Poor_formule
open Either

module Make :
functor (K : Kripke.K) ->
sig

  type game_state =
    int *
      (K.SV.elt poor_formule,
       (int * K.SV.elt poor_formule) pbf)
        either

  val string_of_state :
    int ->
    (string poor_formule, (int * string poor_formule) pbf)
      either -> string

  val gsphi : K.kripke -> game_state -> game_state list

  val export_game_checked :
    K.SV.elt poor_formule ->
    K.kripke ->
    int ->
    ((K.SV.elt poor_formule,
      (int * K.SV.elt poor_formule) pbf)
       either ->
     (string poor_formule,
      (int * string poor_formule) pbf)
       either) ->
    string -> unit

  val check : K.SV.elt poor_formule -> K.kripke -> int -> bool
end
