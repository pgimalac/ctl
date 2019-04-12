open Automata
open Poor_formule

module Make :
functor (K : Kripke.K) ->
sig

  type game_state =
    int *
      (int * K.SV.elt poor_formule) pbf

  val gsphi : K.kripke -> game_state -> game_state list

  val export_game_checked :
    K.SV.elt Poor_formule.poor_formule ->
    K.kripke ->
    int ->
    ((int * K.SV.elt Poor_formule.poor_formule) Automata.pbf ->
     (int * ('a, string) Formule.formule) Automata.pbf) ->
    string -> bool

  val check : K.SV.elt poor_formule -> K.kripke -> int -> bool
end
