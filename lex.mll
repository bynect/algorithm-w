{
open Parse
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let var = letter+

rule token = 
  parse
  | white { token lexbuf }
  | "--" [^'\n']* { token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { ARROW }
  | "=" { EQ }
  | "fun" { FUN }
  | "if" { IF }
  | "let" { LET }
  | "in" { IN }
  | "then" { THEN }
  | "else" { ELSE }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | _ { failwith "Lexing error" }
  | eof { EOF }