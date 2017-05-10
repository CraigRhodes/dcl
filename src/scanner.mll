(* Ocamllex scanner for DCL *)

{ open Parser }

let exponent_rule = ('e' | 'E') ('+' | '-')? ['0'-'9']+
let float_rule =             '.'['0'-'9']+ exponent_rule? | 
                 ['0'-'9']+ ('.'['0'-'9']* exponent_rule? |
                 	                       exponent_rule)

let string_rule = ('\'' | '\"')
                  ([' '-'!'] |
                   ['#'-'&'] | 
                   ['('-']'] | 
                   [']'-'~'] | 
                      "\\\\" |
                       "\\r" |
                       "\\n" |
                       "\\t" |
                      "\\\'" |
                      "\\\""  )* 
                  ('\'' | '\"')


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)

(* Brackets and Punctuation *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }

(* Mathematical Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { EXPONT }
| '='      { ASSIGN }

(*Equality Operators *)
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }

(* Logical Operators *)
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "~"      { TILDE }

(* Conditional Operators *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }


(* Keywords for functions *)
| "return" { RETURN }
| "buteverytime" { BUTEVERYTIME }

(* Keywords for Data Types *)
| "int"    { INT }
| "double" { DOUBLE }
| "string" { STRING }
| "void"   { VOID }
| "bool"    { BOOL }
| "true"   { TRUE }
| "false"  { FALSE }
| "["      { LSQUARE }
| "]"      { RSQUARE }
| ","      { COMMA }
| "{|"     { LINDEX }
| "|}"     { RINDEX }
| "of"     { OF }
| "#"      { LENGTH }
| ['0'-'9']+ as lxm { INTLITERAL(int_of_string lxm) }
| float_rule as lxm { DBLLITERAL(float_of_string lxm) }
| string_rule as lxm { STRLITERAL(let rec int_range = function
                                       0 -> [ ]
                                    |  1 -> [ 0 ]
                                    | n -> int_range (n - 1) @ [ n - 1 ] in
                                  let rec glob = function 
                                    | '\\' :: 'n' :: rest -> '\n' :: (glob rest)
                                    | '\\' :: 'r' :: rest -> '\r' :: (glob rest)
                                    | '\\' :: 't' :: rest -> '\t' :: (glob rest)
                                    | '\\' :: '\\' :: rest -> '\\' :: (glob rest)
                                    | '\\' :: '"' :: rest -> '\"' :: (glob rest)
                                    | '\\' :: '\'' :: rest -> '\'' :: (glob rest)
                                    | x :: rest ->  x :: (glob rest) 
                                    | [] -> [] in
	                              let char_cleaned = glob (List.map (fun x -> lxm.[x]) (int_range (String.length lxm))) in
	                              let cleaned = String.concat "" (List.map (fun x -> String.make 1 x) char_cleaned) in
	                              let strlen = String.length cleaned in
                                if strlen == 2 then "" else String.sub cleaned 1 (strlen - 2)) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
