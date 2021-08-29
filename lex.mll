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
  | "->" { ARROW }
  | ',' { COMMA }
  | '=' { EQUAL }
  | "rec" { REC }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
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
