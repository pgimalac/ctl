{
    open Parser
}

let var = ['a'-'z']['a'-'z''0'-'9']*

(* PROPOSITIONNAL LOGIC CONNECTORS *)
(* 0 *)
let top  = "⊤" | '1'
let bot  = "⊥" | '0'
(* 1 *)
let not  = "¬" | '~' | '-'
(* 2 *)
let an   = '^' | "∧" | '&'
let or   = '|' | "∨" | '+' | "∥"
let impl = "⇒" | "→" | "->" | "=>"
let equ  = "⇔" | "≡" | "<->" | "<=>"
let xor  = "⊕" | "⊻"

(* STATE CONNECTORS *)
(* 1 *)
let future    = 'F'
let always    = 'G'
let tomorrow  = 'X'
(* 2 *)
let until     = 'U'
let weakUntil = 'W'

(* PATH CONNECTORS *)
let all   = 'A'
let exist = 'E'

rule token = parse
      [' ' '\t']
        { token lexbuf }
    | '('
        { LPAR }
    | ')'
        { RPAR }
    | var as v
        { VAR(v) }
    | top
        { BOOL(true) }
    | bot
        { BOOL(false) }
    | not
        { NOT }
    | an
        { AND }
    | or
        { OR }
    | impl
        { IMPL }
    | equ
        { EQU }
    | xor
        { XOR }
    | future
        { FUTURE }
    | always
        { ALWAYS }
    | tomorrow
        { TOMORROW }
    | until
        { UNTIL }
    | weakUntil
        { WEAKUNTIL }
    | all
        { ALL }
    | exist
        { EXIST }
    | eof
        { EOF }
    | _ as c
        { failwith (String.make 1 c ^ "Unexpected character") }
