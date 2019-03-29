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

(* For graphs *)
let eol = '\n'
let dot = ':'
let int = ['0'-'9']+

(* for comments *)

let start_com = "(*"
let end_com   = "*)"

rule token = parse
      [' ' '\t']
        { token lexbuf }
    | '('
        { LPAR }
    | ')'
        { RPAR }
    | int as i
        { INT(int_of_string i) }
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
    | dot
        { DOT }
    | eol
        { EOL }
    | start_com
        { comment lexbuf }
    | _ as c
        { failwith (String.make 1 c ^ "Unexpected character") }

and comment = parse
    | end_com
        { token lexbuf }
    | _
        { comment lexbuf }