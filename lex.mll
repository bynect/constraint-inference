{
open Parse

exception Lex_error
}

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "--" [^'\n']* { token lexbuf }
  | eof { EOF }
  | '(' { RPAREN }
  | ')' { LPAREN }
  | '\\' { BACKSLASH }
  | '.' { DOT }
  | '=' { EQUAL }
  | "let" { LET }
  | "in" { IN }
  | "True" { BOOL true }
  | "False" { BOOL false }
  | ['a'-'z' 'A'-'Z' '_']+
    {
        VAR (Lexing.lexeme lexbuf)
    }
  | ['0'-'9']+
    {
        INT (Lexing.lexeme lexbuf |> int_of_string)
    }
  | _ { raise Lex_error }
