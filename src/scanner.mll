(* Ocamllex scanner for DCL *)

{ open Parser 
 let unescape s =
    	Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}


(* Using nice formatting from DICE (Fall 2015) to make parser code easier to read *)
let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let string = '"' ( (ascii | escape)* as s) '"'
let char = ''' ( ascii | digit ) '''
let double = (digit+) ['.'] digit+
let integer = digit+
let whitespace = [' ' '\t' '\r']
let return = '\n'


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)


(* General parentheses and syntax requirements *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }

(*Operators for DCL *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }


(* Tilde Operator in DCL *)
(*| '~'	   { TILDE } *)

(* Branch Controlling in DCL *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "void"   { VOID }

(* Data Types in DCL *)
| "int"    { INT }
| "double" { FLOAT }
| "void"   { VOID }
| "string"  { STRING }
| "bool"	{ BOOL }

| "true" { TRUE }
| "false" { FALSE }
| integer as lxm { INT_LITERAL(int_of_string lxm) }
| double as lxm { FLOAT_LITERAL(float_of_string lxm) } 
| char as lxm   {CHAR_LITERAL(String.get lxm 1)      }
| escape_char as lxm {CHAR_LITERAL(String.get (unescape lxm) 1)}
| string         {STRING_LITERAL(unescape s)} 
| id as lxm {ID(lxm)}
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
