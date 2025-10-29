{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*


rule read =
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":=" { TAKES }
  | "+" { PLUS }
  | "-" { MINUS }
  | ";" { SEQ }
  | "while" { WHILE }
  | "do" { DO }
  | "*" { MUL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "skip" { SKIP }
  | id  { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
