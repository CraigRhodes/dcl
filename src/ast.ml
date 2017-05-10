(* Abstract Syntax Tree and functions for printing it *)


type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Exp 

type uop = Neg | Not | Length

type dtyp = Int | Double | String

type typ = Simple of dtyp | Void | Array of dtyp * int

type bind = typ * string

(*type arr_literals = 
    ArrLiteral of expr list
  | MultiArrLiteral of arr_literals list *)

type expr =
  (* arr_literals
  |*) 
    IntLiteral of int
  | DblLiteral of float 
  | StrLiteral of string
  | ArrLiteral of expr list 
  | DefaultArrLiteral of expr * expr 
  | Index of expr * expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | ArrayAssign of string * expr list * expr
  | Call of string * expr list
  | Noexpr
  | LocalAssign of typ * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Local of typ * string

type globalstmt = 
    Global of typ * string
  | GlobalAssign of typ * string * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }
  

type program = globalstmt list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Exp -> "^"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Length -> "#"

let convert_array l conversion joiner =
    let glob_item original data = original ^ (conversion data) ^ joiner in
    let full = (List.fold_left glob_item "" l) in
    "[" ^ String.sub full 0 ((String.length full) - 2) ^ "]"

let string_of_d_typ = function
    Int -> "int"
  | Double -> "double"
  | String -> "string"

let rec repeat c = function 
    0 -> ""
  | n -> c ^ (repeat c (n - 1))

let string_of_typ = function
   Void -> "void"
  |  Simple(d) -> string_of_d_typ d
  | Array(d, n) -> string_of_d_typ d ^ repeat "[]" n

let rec string_of_expr = function
    IntLiteral(l) -> string_of_int l
  | DblLiteral(l) -> string_of_float l
  | StrLiteral(l) -> "\"" ^ l ^ "\""
  | ArrLiteral(l) -> convert_array l string_of_expr ", "
(*  | MultiArrLiteral(l) -> convert_array l string_of_expr ",\n" *)
  | DefaultArrLiteral(e1, e2) -> "[" ^ string_of_expr e1 ^ " of " ^ string_of_expr e2 ^ "]"
  | Id(s) -> s
  | Index(e, l) -> string_of_expr e ^ 
                   (*convert_array l (fun e -> "[" ^ string_of_expr e ^ "]") ""*)
                   "{|" ^ string_of_expr (List.hd l) ^ "|}"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ArrayAssign(v, l, e) -> v ^ "[" ^ string_of_expr (List.hd l) ^ "]" ^ " = " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | LocalAssign(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e 


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Local(t, s) -> string_of_typ t ^ " " ^ s ^ ";\n"

let string_of_globalstmt = function 
    Global(t,s) -> string_of_typ t ^ " " ^ s ^ ";\n"
  | GlobalAssign(t,s,e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_globalstmt vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
