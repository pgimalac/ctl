open Automata

type ('a, 'b) either = Left of 'a | Right of 'b
module Make :
  functor (K : Kripke.K) ->
    sig

      type game_state =
        int *
          (K.SV.elt Formule.formule,
           (int * K.SV.elt Formule.formule) Automata.pbf)
            either

      val string_of_state :
        int ->
        (string Formule.formule, (int * string Formule.formule) Automata.pbf)
          either -> string


      val gsphi : K.kripke -> game_state -> game_state list

      val write_cfc_into_file : string -> ('a * Kripke.S.t) list -> unit


      val export_game_checked :
        K.SV.elt Formule.formule ->
        K.kripke ->
        int ->
        ((K.SV.elt Formule.formule,
          (int * K.SV.elt Formule.formule) Automata.pbf)
           either ->
         (string Formule.formule,
          (int * string Formule.formule) Automata.pbf)
           either) ->
        string -> unit

      val check : K.SV.elt Formule.formule -> K.kripke -> int -> bool
    end
