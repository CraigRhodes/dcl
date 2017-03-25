type token =
  | INT
  | FLOAT
  | CHAR
  | VOID
  | NULL
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | ID of (string)
  | CHAR_LITERAL of (char)
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
 open Ast 
# 46 "parser.ml"
let yytransl_const = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* CHAR *);
  260 (* VOID *);
  261 (* NULL *);
  262 (* SEMI *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* LBRACE *);
  266 (* RBRACE *);
  267 (* COMMA *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIVIDE *);
  272 (* ASSIGN *);
  273 (* NOT *);
  274 (* EQ *);
  275 (* NEQ *);
  276 (* LT *);
  277 (* LEQ *);
  278 (* GT *);
  279 (* GEQ *);
  280 (* TRUE *);
  281 (* FALSE *);
  282 (* AND *);
  283 (* OR *);
  284 (* RETURN *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* WHILE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  289 (* INT_LITERAL *);
  290 (* FLOAT_LITERAL *);
  291 (* STRING_LITERAL *);
  292 (* ID *);
  293 (* CHAR_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\007\000\007\000\003\000\008\000\008\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\012\000\012\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\013\000\013\000\013\000\013\000\
\013\000\013\000\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\000\000\002\000\003\000\000\000\002\000\
\002\000\002\000\003\000\003\000\005\000\007\000\009\000\005\000\
\000\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\004\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\011\000\001\000\003\000\
\004\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\012\000\000\000\000\000\009\000\013\000\
\000\000\000\000\000\000\050\000\000\000\015\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\000\046\000\047\000\
\000\000\048\000\016\000\000\000\027\000\000\000\000\000\040\000\
\041\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\000\020\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\030\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\022\000\
\000\000\000\000\023\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\010\000\015\000\022\000\026\000\
\016\000\043\000\044\000\075\000\045\000\078\000\079\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\234\254\064\255\000\000\015\255\006\255\036\255\035\255\
\000\000\043\255\015\255\000\000\022\255\015\255\000\000\000\000\
\024\255\044\255\061\255\000\000\174\255\000\000\000\000\174\255\
\174\255\169\255\076\255\078\255\084\255\000\000\000\000\000\000\
\251\254\000\000\000\000\224\255\000\000\166\000\077\255\000\000\
\000\000\000\000\250\255\174\255\174\255\174\255\174\255\174\255\
\000\000\174\255\174\255\174\255\174\255\174\255\174\255\174\255\
\174\255\174\255\174\255\174\255\174\255\000\000\000\000\000\000\
\182\000\214\000\086\255\198\000\214\000\085\255\087\255\214\000\
\074\255\074\255\000\000\000\000\001\001\001\001\180\255\180\255\
\180\255\180\255\245\000\230\000\136\255\174\255\136\255\000\000\
\174\255\066\255\012\000\000\000\214\000\136\255\174\255\000\000\
\093\255\136\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\094\255\000\000\000\000\096\255\
\000\000\000\000\000\000\000\000\000\000\090\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\206\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\109\255\000\000\108\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\255\000\000\000\000\051\255\000\000\112\255\007\255\
\034\000\056\000\000\000\000\000\037\255\150\000\078\000\100\000\
\122\000\144\000\172\255\001\255\000\000\000\000\000\000\000\000\
\000\000\123\255\000\000\000\000\058\255\000\000\121\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\095\000\000\000\028\000\000\000\000\000\101\000\
\000\000\171\255\227\255\031\000\000\000\000\000\000\000"

let yytablesize = 536
let yytable = "\046\000\
\007\000\055\000\048\000\049\000\051\000\001\000\039\000\098\000\
\039\000\100\000\056\000\039\000\042\000\011\000\042\000\005\000\
\104\000\042\000\006\000\026\000\107\000\026\000\073\000\074\000\
\076\000\077\000\080\000\039\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\014\000\017\000\032\000\018\000\032\000\019\000\021\000\032\000\
\028\000\025\000\029\000\020\000\030\000\031\000\032\000\032\000\
\032\000\023\000\053\000\027\000\033\000\053\000\032\000\032\000\
\099\000\054\000\012\000\101\000\054\000\012\000\013\000\034\000\
\035\000\074\000\036\000\037\000\038\000\039\000\040\000\041\000\
\042\000\028\000\052\000\029\000\053\000\030\000\071\000\060\000\
\061\000\032\000\054\000\094\000\096\000\033\000\015\000\102\000\
\015\000\097\000\015\000\015\000\106\000\006\000\015\000\007\000\
\034\000\035\000\015\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\025\000\051\000\024\000\015\000\015\000\052\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\021\000\
\025\000\021\000\047\000\021\000\021\000\105\000\000\000\021\000\
\000\000\000\000\000\000\021\000\028\000\000\000\029\000\000\000\
\030\000\000\000\000\000\000\000\032\000\000\000\021\000\021\000\
\033\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\000\000\000\000\000\000\034\000\035\000\000\000\036\000\037\000\
\038\000\039\000\040\000\041\000\042\000\028\000\050\000\029\000\
\000\000\038\000\028\000\038\000\029\000\032\000\038\000\000\000\
\000\000\033\000\032\000\000\000\000\000\000\000\033\000\058\000\
\059\000\060\000\061\000\000\000\000\000\038\000\038\000\000\000\
\000\000\038\000\039\000\040\000\041\000\042\000\038\000\039\000\
\040\000\041\000\042\000\049\000\000\000\049\000\000\000\000\000\
\049\000\049\000\049\000\049\000\049\000\000\000\000\000\049\000\
\049\000\049\000\049\000\049\000\049\000\057\000\000\000\049\000\
\049\000\000\000\000\000\058\000\059\000\060\000\061\000\000\000\
\000\000\062\000\063\000\064\000\065\000\066\000\067\000\000\000\
\000\000\068\000\069\000\000\000\000\000\000\000\000\000\072\000\
\000\000\005\000\000\000\000\000\006\000\058\000\059\000\060\000\
\061\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\103\000\000\000\068\000\069\000\000\000\000\000\058\000\
\059\000\060\000\061\000\000\000\000\000\062\000\063\000\064\000\
\065\000\066\000\067\000\000\000\000\000\068\000\069\000\028\000\
\000\000\028\000\000\000\000\000\028\000\028\000\028\000\000\000\
\000\000\000\000\000\000\028\000\028\000\028\000\028\000\028\000\
\028\000\000\000\000\000\028\000\028\000\029\000\000\000\029\000\
\000\000\000\000\029\000\029\000\029\000\000\000\000\000\000\000\
\000\000\029\000\029\000\029\000\029\000\029\000\029\000\000\000\
\000\000\029\000\029\000\034\000\000\000\034\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\034\000\034\000\034\000\034\000\034\000\000\000\000\000\034\000\
\034\000\035\000\000\000\035\000\000\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\035\000\035\000\
\035\000\035\000\035\000\000\000\000\000\035\000\035\000\036\000\
\000\000\036\000\000\000\000\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\036\000\036\000\036\000\036\000\
\036\000\000\000\000\000\036\000\036\000\037\000\000\000\037\000\
\000\000\000\000\037\000\033\000\000\000\033\000\000\000\000\000\
\033\000\037\000\037\000\037\000\037\000\037\000\037\000\033\000\
\033\000\037\000\037\000\000\000\000\000\070\000\000\000\033\000\
\033\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\093\000\000\000\068\000\
\069\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\095\000\000\000\068\000\
\069\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\000\000\000\000\068\000\
\069\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\000\000\000\000\068\000\
\069\000\058\000\059\000\060\000\061\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\067\000\000\000\000\000\068\000\
\058\000\059\000\060\000\061\000\000\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\000\000\064\000\065\000\066\000\067\000"

let yycheck = "\029\000\
\000\000\007\001\032\000\033\000\034\000\001\000\006\001\093\000\
\008\001\095\000\016\001\011\001\006\001\036\001\008\001\001\001\
\102\000\011\001\004\001\006\001\106\000\008\001\052\000\053\000\
\054\000\055\000\056\000\027\001\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\013\000\036\001\006\001\008\001\008\001\011\001\019\000\011\001\
\005\001\022\000\007\001\009\001\009\001\010\001\018\001\019\001\
\013\001\036\001\008\001\036\001\017\001\011\001\026\001\027\001\
\094\000\008\001\006\001\097\000\011\001\006\001\007\001\028\001\
\029\001\103\000\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\005\001\007\001\007\001\007\001\009\001\010\001\014\001\
\015\001\013\001\007\001\006\001\008\001\017\001\005\001\030\001\
\007\001\011\001\009\001\010\001\008\001\008\001\013\001\008\001\
\028\001\029\001\017\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\006\001\008\001\022\000\028\001\029\001\008\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\005\001\
\008\001\007\001\030\000\009\001\010\001\103\000\255\255\013\001\
\255\255\255\255\255\255\017\001\005\001\255\255\007\001\255\255\
\009\001\255\255\255\255\255\255\013\001\255\255\028\001\029\001\
\017\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\255\255\255\255\255\255\028\001\029\001\255\255\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\005\001\006\001\007\001\
\255\255\006\001\005\001\008\001\007\001\013\001\011\001\255\255\
\255\255\017\001\013\001\255\255\255\255\255\255\017\001\012\001\
\013\001\014\001\015\001\255\255\255\255\026\001\027\001\255\255\
\255\255\033\001\034\001\035\001\036\001\037\001\033\001\034\001\
\035\001\036\001\037\001\006\001\255\255\008\001\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\006\001\255\255\026\001\
\027\001\255\255\255\255\012\001\013\001\014\001\015\001\255\255\
\255\255\018\001\019\001\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\255\255\255\255\255\255\255\255\006\001\
\255\255\001\001\255\255\255\255\004\001\012\001\013\001\014\001\
\015\001\255\255\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\006\001\255\255\026\001\027\001\255\255\255\255\012\001\
\013\001\014\001\015\001\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\026\001\027\001\006\001\
\255\255\008\001\255\255\255\255\011\001\012\001\013\001\255\255\
\255\255\255\255\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\255\255\026\001\027\001\006\001\255\255\008\001\
\255\255\255\255\011\001\012\001\013\001\255\255\255\255\255\255\
\255\255\018\001\019\001\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\006\001\255\255\008\001\255\255\255\255\
\011\001\255\255\255\255\255\255\255\255\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\006\001\255\255\008\001\255\255\255\255\011\001\255\255\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\026\001\027\001\006\001\
\255\255\008\001\255\255\255\255\011\001\255\255\255\255\255\255\
\255\255\255\255\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\255\255\026\001\027\001\006\001\255\255\008\001\
\255\255\255\255\011\001\006\001\255\255\008\001\255\255\255\255\
\011\001\018\001\019\001\020\001\021\001\022\001\023\001\018\001\
\019\001\026\001\027\001\255\255\255\255\008\001\255\255\026\001\
\027\001\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\008\001\255\255\026\001\
\027\001\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\008\001\255\255\026\001\
\027\001\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\012\001\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\023\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001"

let yynames_const = "\
  INT\000\
  FLOAT\000\
  CHAR\000\
  VOID\000\
  NULL\000\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  STRING_LITERAL\000\
  ID\000\
  CHAR_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
            ( _1 )
# 355 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                 ( [], [] )
# 361 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 37 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 369 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 38 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 377 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 42 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 392 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( [] )
# 398 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                  ( List.rev _1 )
# 405 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)] )
# 413 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                             ( (_3,_4) :: _1 )
# 422 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
        ( Int )
# 428 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         ( Void )
# 434 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                     ( [] )
# 440 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 62 "parser.mly"
                     ( _2 :: _1 )
# 448 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 65 "parser.mly"
               ( (_1, _2) )
# 456 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                   ( [] )
# 462 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 69 "parser.mly"
                   ( _2 :: _1 )
# 470 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
              ( Expr _1 )
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                ( Return Noexpr )
# 483 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( Return _2 )
# 490 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 75 "parser.mly"
                            ( Block(List.rev _2) )
# 497 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 505 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                            ( If(_3, _5, _7) )
# 514 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 524 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                  ( While(_3, _5) )
# 532 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                  ( Noexpr )
# 538 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                  ( _1 )
# 545 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literals) in
    Obj.repr(
# 87 "parser.mly"
                    ( Literal(_1) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 584 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Or,    _3) )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                         ( Unop(Neg, _2) )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Unop(Not, _2) )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Assign(_1, _3) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 103 "parser.mly"
                                 ( Call(_1, _3) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( _2 )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 107 "parser.mly"
                (Int_Lit(_1)))
# 692 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 108 "parser.mly"
                  (Float_Lit(_1))
# 699 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                  (String_Lit(_1))
# 706 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 110 "parser.mly"
                (Char_Lit(_1))
# 713 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
                  ( Id(_1) )
# 720 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                    ( Null )
# 726 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                  ( [] )
# 732 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 116 "parser.mly"
                  ( List.rev _1 )
# 739 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                            ( [_1] )
# 746 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                            ( _3 :: _1 )
# 754 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
