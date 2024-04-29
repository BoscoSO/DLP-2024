
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']    { token lexbuf }
  | "lambda"      { LAMBDA }
  | "L"           { LAMBDA }
  | "type"        { TYPE }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "succ"        { SUCC }
  | "pred"        { PRED }
  | "iszero"      { ISZERO }
  | "let"         { LET }
  | "letrec"      { LETREC }
  | "fix"         { FIX }
  | "in"          { IN }
  | "concat"      { CONCAT }
  | "first"       { FIRST }
  | "rest"        { REST }
  | "isEmptyList" { ISEMPTYLIST }
  | "head"        { HEAD }
  | "hd"          { HEAD }
  | "tail"        { TAIL }
  | "tl"          { TAIL }
  | "Bool"        { BOOL }
  | "Nat"         { NAT }
  | "String"      { STRING }
  | "null"        { NULL }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACK }
  | ']'           { RBRACK }
  | '.'           { DOT }
  | ','           { COMMA }
  | '='           { EQ }
  | ':'           { COLON }
  | "->"          { ARROW }
  | ['0'-'9']+    { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                  { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['a'-'z' '_' '0'-'9']*
                  { IDT (Lexing.lexeme lexbuf) }
  | '"'[^ '"' '\n']*'"'
                  { let s = Lexing.lexeme lexbuf in STRINGV (String.sub s 1 (String.length s - 2))}
  | eof           { EOF }
  | _             { raise Lexical_error } 

