type token =
  | INT
  | FLOAT
  | CHAR
  | VOID
  | NULL
  | STRING
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
# 47 "parser.ml"
let yytransl_const = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* CHAR *);
  260 (* VOID *);
  261 (* NULL *);
  262 (* STRING *);
  263 (* SEMI *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* LBRACE *);
  267 (* RBRACE *);
  268 (* COMMA *);
  269 (* PLUS *);
  270 (* MINUS *);
  271 (* TIMES *);
  272 (* DIVIDE *);
  273 (* ASSIGN *);
  274 (* NOT *);
  275 (* EQ *);
  276 (* NEQ *);
  277 (* LT *);
  278 (* LEQ *);
  279 (* GT *);
  280 (* GEQ *);
  281 (* TRUE *);
  282 (* FALSE *);
  283 (* AND *);
  284 (* OR *);
  285 (* RETURN *);
  286 (* IF *);
  287 (* ELSE *);
  288 (* FOR *);
  289 (* WHILE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  290 (* INT_LITERAL *);
  291 (* FLOAT_LITERAL *);
  292 (* STRING_LITERAL *);
  293 (* ID *);
  294 (* CHAR_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\007\000\007\000\003\000\008\000\
\008\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\000\000\002\000\003\000\000\000\
\002\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\054\000\000\000\010\000\011\000\012\000\001\000\
\003\000\004\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\013\000\000\000\000\000\009\000\
\014\000\000\000\000\000\000\000\000\000\016\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\029\000\030\000\
\000\000\031\000\017\000\000\000\000\000\000\000\045\000\046\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\021\000\020\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\023\000\000\000\
\000\000\024\000"

let yydgoto = "\002\000\
\003\000\004\000\009\000\010\000\011\000\016\000\023\000\027\000\
\017\000\043\000\044\000\074\000\077\000\078\000"

let yysindex = "\050\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\221\254\053\255\000\000\087\255\017\255\067\255\
\050\255\000\000\075\255\087\255\000\000\052\255\087\255\000\000\
\000\000\055\255\045\255\091\255\164\255\000\000\000\000\164\255\
\164\255\034\255\093\255\094\255\096\255\000\000\000\000\000\000\
\002\255\000\000\000\000\214\255\131\000\076\255\000\000\000\000\
\000\000\232\255\164\255\164\255\164\255\164\255\164\255\000\000\
\164\255\164\255\164\255\164\255\164\255\164\255\164\255\164\255\
\164\255\164\255\164\255\164\255\000\000\000\000\000\000\147\000\
\179\000\109\255\163\000\179\000\123\255\117\255\179\000\080\255\
\080\255\000\000\000\000\222\000\222\000\251\255\251\255\251\255\
\251\255\210\000\195\000\133\255\164\255\133\255\000\000\164\255\
\102\255\255\255\000\000\179\000\133\255\164\255\000\000\128\255\
\133\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\130\255\000\000\000\000\
\131\255\000\000\000\000\000\000\000\000\000\000\089\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\196\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\135\255\000\000\136\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\255\000\000\000\000\005\255\000\000\137\255\037\255\167\255\
\021\000\000\000\000\000\038\255\115\000\043\000\065\000\087\000\
\109\000\108\255\255\254\000\000\000\000\000\000\000\000\000\000\
\120\255\000\000\000\000\031\255\000\000\139\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\121\000\000\000\254\255\000\000\000\000\129\000\
\000\000\171\255\227\255\058\000\000\000\000\000"

let yytablesize = 502
let yytable = "\045\000\
\008\000\012\000\047\000\048\000\050\000\044\000\097\000\044\000\
\099\000\054\000\044\000\015\000\027\000\052\000\027\000\103\000\
\052\000\022\000\055\000\106\000\026\000\072\000\073\000\075\000\
\076\000\079\000\044\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\053\000\
\049\000\029\000\053\000\047\000\037\000\047\000\037\000\032\000\
\047\000\037\000\001\000\033\000\029\000\018\000\030\000\031\000\
\037\000\037\000\032\000\013\000\014\000\020\000\033\000\098\000\
\037\000\037\000\100\000\038\000\039\000\040\000\041\000\042\000\
\073\000\034\000\035\000\019\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\029\000\021\000\030\000\070\000\005\000\
\024\000\032\000\006\000\028\000\007\000\033\000\059\000\060\000\
\016\000\013\000\016\000\016\000\051\000\052\000\016\000\053\000\
\034\000\035\000\016\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\093\000\043\000\016\000\016\000\043\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\022\000\
\096\000\022\000\022\000\095\000\101\000\022\000\043\000\043\000\
\105\000\022\000\006\000\007\000\029\000\026\000\030\000\025\000\
\050\000\051\000\032\000\026\000\022\000\022\000\033\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\046\000\104\000\
\000\000\034\000\035\000\000\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\029\000\000\000\033\000\000\000\033\000\
\000\000\032\000\033\000\033\000\033\000\033\000\000\000\000\000\
\000\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
\000\000\033\000\033\000\000\000\000\000\038\000\039\000\040\000\
\041\000\042\000\032\000\000\000\032\000\000\000\000\000\032\000\
\032\000\032\000\032\000\032\000\000\000\000\000\032\000\032\000\
\032\000\032\000\032\000\032\000\056\000\000\000\032\000\032\000\
\000\000\000\000\057\000\058\000\059\000\060\000\000\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\071\000\000\000\
\067\000\068\000\000\000\000\000\057\000\058\000\059\000\060\000\
\000\000\000\000\061\000\062\000\063\000\064\000\065\000\066\000\
\000\000\005\000\067\000\068\000\006\000\102\000\007\000\057\000\
\058\000\059\000\060\000\057\000\058\000\059\000\060\000\000\000\
\000\000\061\000\062\000\063\000\064\000\065\000\066\000\000\000\
\000\000\067\000\068\000\034\000\000\000\034\000\000\000\000\000\
\034\000\034\000\034\000\000\000\000\000\000\000\000\000\034\000\
\034\000\034\000\034\000\034\000\034\000\000\000\000\000\034\000\
\034\000\039\000\000\000\039\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\000\000\000\000\039\000\039\000\040\000\
\000\000\040\000\000\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\040\000\040\000\040\000\040\000\
\040\000\000\000\000\000\040\000\040\000\041\000\000\000\041\000\
\000\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\041\000\041\000\041\000\041\000\041\000\000\000\
\000\000\041\000\041\000\042\000\000\000\042\000\000\000\000\000\
\042\000\038\000\000\000\038\000\000\000\000\000\038\000\042\000\
\042\000\042\000\042\000\042\000\042\000\038\000\038\000\042\000\
\042\000\000\000\000\000\069\000\000\000\038\000\038\000\057\000\
\058\000\059\000\060\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\092\000\000\000\067\000\068\000\057\000\
\058\000\059\000\060\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\094\000\000\000\067\000\068\000\057\000\
\058\000\059\000\060\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\067\000\068\000\057\000\
\058\000\059\000\060\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\067\000\068\000\057\000\
\058\000\059\000\060\000\000\000\000\000\061\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\067\000\057\000\058\000\
\059\000\060\000\000\000\000\000\061\000\062\000\063\000\064\000\
\065\000\066\000\057\000\058\000\059\000\060\000\000\000\000\000\
\000\000\000\000\063\000\064\000\065\000\066\000"

let yycheck = "\029\000\
\000\000\037\001\032\000\033\000\034\000\007\001\092\000\009\001\
\094\000\008\001\012\001\014\000\007\001\009\001\009\001\101\000\
\012\001\020\000\017\001\105\000\023\000\051\000\052\000\053\000\
\054\000\055\000\028\001\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\009\001\
\007\001\008\001\012\001\007\001\007\001\009\001\009\001\014\001\
\012\001\012\001\001\000\018\001\008\001\037\001\010\001\011\001\
\019\001\020\001\014\001\007\001\008\001\012\001\018\001\093\000\
\027\001\028\001\096\000\034\001\035\001\036\001\037\001\038\001\
\102\000\029\001\030\001\009\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\008\001\010\001\010\001\011\001\001\001\
\037\001\014\001\004\001\037\001\006\001\018\001\015\001\016\001\
\008\001\007\001\010\001\011\001\008\001\008\001\014\001\008\001\
\029\001\030\001\018\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\007\001\007\001\009\001\029\001\030\001\012\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\008\001\
\012\001\010\001\011\001\009\001\031\001\014\001\027\001\028\001\
\009\001\018\001\009\001\009\001\008\001\007\001\010\001\023\000\
\009\001\009\001\014\001\009\001\029\001\030\001\018\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\030\000\102\000\
\255\255\029\001\030\001\255\255\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\008\001\255\255\007\001\255\255\009\001\
\255\255\014\001\012\001\013\001\014\001\018\001\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\255\255\255\255\034\001\035\001\036\001\
\037\001\038\001\007\001\255\255\009\001\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\255\255\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\007\001\255\255\027\001\028\001\
\255\255\255\255\013\001\014\001\015\001\016\001\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\007\001\255\255\
\027\001\028\001\255\255\255\255\013\001\014\001\015\001\016\001\
\255\255\255\255\019\001\020\001\021\001\022\001\023\001\024\001\
\255\255\001\001\027\001\028\001\004\001\007\001\006\001\013\001\
\014\001\015\001\016\001\013\001\014\001\015\001\016\001\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\007\001\255\255\009\001\255\255\255\255\
\012\001\013\001\014\001\255\255\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\255\255\255\255\027\001\
\028\001\007\001\255\255\009\001\255\255\255\255\012\001\255\255\
\255\255\255\255\255\255\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\007\001\
\255\255\009\001\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\007\001\255\255\009\001\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\007\001\255\255\009\001\255\255\255\255\
\012\001\007\001\255\255\009\001\255\255\255\255\012\001\019\001\
\020\001\021\001\022\001\023\001\024\001\019\001\020\001\027\001\
\028\001\255\255\255\255\009\001\255\255\027\001\028\001\013\001\
\014\001\015\001\016\001\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\009\001\255\255\027\001\028\001\013\001\
\014\001\015\001\016\001\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\009\001\255\255\027\001\028\001\013\001\
\014\001\015\001\016\001\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\013\001\
\014\001\015\001\016\001\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\013\001\
\014\001\015\001\016\001\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\013\001\014\001\
\015\001\016\001\255\255\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\021\001\022\001\023\001\024\001"

let yynames_const = "\
  INT\000\
  FLOAT\000\
  CHAR\000\
  VOID\000\
  NULL\000\
  STRING\000\
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
# 350 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                 ( [], [] )
# 356 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 37 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 364 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 38 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 372 "parser.ml"
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
# 387 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( [] )
# 393 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                  ( List.rev _1 )
# 400 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)] )
# 408 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                             ( (_3,_4) :: _1 )
# 417 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
        ( Int )
# 423 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         ( Void )
# 429 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
           (String)
# 435 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                     ( [] )
# 441 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 63 "parser.mly"
                     ( _2 :: _1 )
# 449 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 66 "parser.mly"
               ( (_1, _2) )
# 457 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                   ( [] )
# 463 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                   ( _2 :: _1 )
# 471 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
              ( Expr _1 )
# 478 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                ( Return Noexpr )
# 484 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                     ( Return _2 )
# 491 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 76 "parser.mly"
                            ( Block(List.rev _2) )
# 498 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 506 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "parser.mly"
                                            ( If(_3, _5, _7) )
# 515 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                                  ( While(_3, _5) )
# 533 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                  ( Noexpr )
# 539 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                  ( _1 )
# 546 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
                (Int_Lit(_1))
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 89 "parser.mly"
                  (Float_Lit(_1))
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                   (String_Lit(_1))
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 91 "parser.mly"
                (Char_Lit(_1))
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                  ( Id(_1) )
# 581 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 589 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 597 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 605 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 613 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 621 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 629 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 637 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 645 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 653 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Binop(_1, And,   _3) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binop(_1, Or,    _3) )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                         ( Unop(Neg, _2) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Unop(Not, _2) )
# 691 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                     ( Assign(_1, _3) )
# 699 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 108 "parser.mly"
                                 ( Call(_1, _3) )
# 707 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                       ( _2 )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                  ( [] )
# 720 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 115 "parser.mly"
                  ( List.rev _1 )
# 727 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                            ( [_1] )
# 734 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                            ( _3 :: _1 )
# 742 "parser.ml"
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
