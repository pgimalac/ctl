open Automata
open Poor_formule

type ('a, 'b) either = Left of 'a | Right of 'b
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

  val write_cfc_into_file : string -> ('a * Kripke.S.t) list -> unit

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
