

%{
open Ast
%}

/* Ocamlyacc parser for DCL */
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE EXPONT ASSIGN NOT TILDE
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR DOUBLE STRING BUTEVERYTIME
%token RETURN IF ELSE FOR WHILE INT BOOL VOID LINDEX RINDEX
%token LSQUARE RSQUARE OF LENGTH
%token <int> INTLITERAL
%token <float> DBLLITERAL
%token <string> STRLITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right EXPONT
%right NOT NEG LENGTH
%nonassoc LINDEX

%start program
%type <Ast.program> program

%%


program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls globalstmt { ($2 :: fst $1), snd $1 }
 | decls bdecl { fst $2 :: fst $1, (snd $2 :: snd $1) }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 body = List.rev $7 } }

bdecl:
   typ ID ASSIGN expr BUTEVERYTIME LPAREN expr RPAREN LBRACE stmt_list RBRACE
     { (GlobalAssign($1, $2, $4), { typ = $1;
   fname = "__" ^ $2;
   formals = [($1, $2)];
   body = let full_stmt_list = (List.rev $10) @ [ Return (Id($2)) ] in
          [ If($7, Block(full_stmt_list), Return (Id($2))) ]  (*(Id($2))*)
        }) 
     }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

dtyp:
    INT { Int }
  | DOUBLE { Double }
  | STRING { String }

dim_list:
    LSQUARE RSQUARE  { 1 }
  | LSQUARE RSQUARE dim_list { 1 + $3 }

atyp:
    dtyp dim_list { Array($1, $2) }

typ:
    dtyp { Simple($1) }
  | VOID { Void }
  | atyp { $1 }

globalstmt_list:
    /* nothing */    { [] }
  | globalstmt_list globalstmt { $2 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }


stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | typ ID SEMI {Local($1, $2)}

globalstmt:
    typ ID SEMI { Global($1, $2) }
  | typ ID ASSIGN expr SEMI { GlobalAssign($1, $2, $4) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

index:
    LINDEX expr RINDEX { $2 }

val_list:
    expr                { [ $1 ] }
  | expr COMMA val_list { [ $1 ] @ $3 }

simple_arr_literal:
    LSQUARE val_list RSQUARE { $2 }

expr:
    INTLITERAL       { IntLiteral($1) }
  | DBLLITERAL       { DblLiteral($1) }
  | STRLITERAL       { StrLiteral($1) }
  | simple_arr_literal { ArrLiteral($1) }
  | TILDE ID       { TildeOp($2) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EXPONT expr { Binop($1, Exp,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | LENGTH expr      { Unop(Length, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LSQUARE expr OF expr RSQUARE { DefaultArrLiteral($2, $4) }
  | ID LSQUARE expr RSQUARE ASSIGN expr { ArrayAssign($1, [$3], $6) }
  | expr index { Index($1, [$2]) }
 /* | ID index ASSIGN expr  { Assign(Index(Id($1), $2), $4) } */
  | LPAREN expr RPAREN { $2 }
  | typ ID ASSIGN expr {LocalAssign($1, $2, $4)}

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
