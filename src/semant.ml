(* Semantic checking for the DCL compiler *)

module A = Ast
open Ast
open Hashtbl
open Llvm

module StringMap = Map.Make(String)
let symbols:(string, A.typ) Hashtbl.t = Hashtbl.create 100 
let globalsymbols:(string, A.typ) Hashtbl.t = Hashtbl.create 100 

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
  n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  let report_local_duplicate exceptf s =
      if Hashtbl.mem symbols s then raise (Failure (exceptf s));
  in

  let report_global_duplicate exceptf s =
      if Hashtbl.mem globalsymbols s then raise (Failure (exceptf s));
  in

  let check_var_void exceptf t n = 
      if t == Void then raise (Failure (exceptf n)) 
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in 

   let type_of_identifier s =
      try Hashtbl.find symbols s
      with Not_found -> try Hashtbl.find globalsymbols s
                        with Not_found -> raise (Failure ("undeclared identifier " ^ s ))
    in

    (* Return the type of an expression or throw an exception *)

  (**** Checking Functions ****)

  if List.mem "print_int" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print_int may not be defined")) else ();

  if List.mem "print_bool" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print_bool may not be defined")) else ();

  if List.mem "print_double" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print_double may not be defined")) else ();

  if List.mem "print_string" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print_string may not be defined")) else ();

  if List.mem "exp_int" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function exp_int may not be defined")) else ();

  if List.mem "exp_dbl" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function exp_dbl may not be defined")) else ();

  if List.mem "add_str" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function add_str may not be defined")) else ();

  if List.mem "bopen" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function bopen may not be defined")) else ();

  if List.mem "bclose" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function bclose may not be defined")) else ();

  if List.mem "bread" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function bread may not be defined")) else ();

  if List.mem "bwrite" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function bwrite may not be defined")) else ();

  if List.mem "malloc" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function malloc may not be defined")) else ();

  if List.mem "free" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function free may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  
    StringMap.add "free" 
    { typ = Void; fname = "free"; formals = [(String, "tofree")]; body = [] }
    (StringMap. add "malloc"
     { typ = String; fname = "malloc"; formals = [(Int, "size")]; body = [] }
      (StringMap.add "bwrite"
        { typ = Int; fname = "bwrite"; formals = [(Int, "fd"); (String, "buf"); (Int, "count")]; body = []}
      (StringMap.add "bread"
         { typ = Int; fname = "bread"; formals = [(Int, "fd"); (String, "buf"); (Int, "count")];  body = [] }
      (
      StringMap.add "bclose"  (* key *)
       { typ = Int; fname = "bclose"; formals = [(Int, "fd")]; body = [] }
      ( 
      StringMap.add "bopen"  (* key *)
       { typ = Int; fname = "bopen"; formals = [(String, "name"); (Int, "flags"); (Int, "mode")];
         body = [] } (* value *)

      (StringMap.add "print_double"  (* key *)
       { typ = Void; fname = "print"; formals = [(Double, "x")];
         body = [] } (* value *)
       
       (StringMap.add "print_int" 
        { typ = Void; fname = "print"; formals = [(Int, "x")];
          body = [] }

        (StringMap.add "print_bool"
        { typ = Void; fname = "printb"; formals = [(Bool, "x")];
          body = [] }


        (StringMap.singleton "print_string"
        
         { typ = String; fname = "print_string"; formals = [(String, "x")];
           body = [] })

      ) )))))))
   in



  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with 
       Not_found -> (let _ = print_string s in raise (Failure ("unrecognized function " ^ s)))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate variable " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals
      );

    let symbol = List.iter (fun (t, n) -> Hashtbl.add symbols n t )
  func.formals
    in

    let rec expr = function
  IntLiteral _ -> Int 
      | DblLiteral _ -> Double
      | StrLiteral _ -> String
      | BoolLiteral _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
  (match op with
        Equal | Neq when t1 = t2 -> Bool

        |  Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
        | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
        | And  | Or when t1 = Bool && t2 = Bool -> Bool
        | Exp when t1 = Int && t2 = Int -> Double

        | Add | Sub | Mult | Div | Exp when t1 = Double && t2 = Double -> Double
        | Less | Leq | Greater | Geq 
        when t1 = Double && t2 = Double -> Int

        | Add when t1 = String && t2 = String -> String 
        | Less | Leq | Greater | Geq when t1 = String && t2 = String -> Int
        
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
   (match op with
     Neg when t = Int -> Int
   | Not when t = Bool -> Bool

   | Neg when t = Double -> Double

         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
           string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_expr ex))
      | LocalAssign (t, s, e) as ex -> check_var_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname) t s; report_local_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname) s;
      let lt = t and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_expr ex)); Hashtbl.add symbols s t; t
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.typ
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Bool expression in " ^ string_of_expr e))
     else () in

    
    let globalstmt = function 
       Global(t,s) as ex -> check_var_void (fun n -> "illegal void global " ^ n) t s; 
       report_global_duplicate (fun n -> "duplicate global " ^ n) s;
       Hashtbl.add globalsymbols s t;
     | GlobalAssign(t,s,e) as ex -> check_var_void (fun n -> "illegal void global " ^ n) t s; 
     report_global_duplicate (fun n -> "duplicate global " ^ n) s;
     let lt = t and rt = expr e in
        check_assign lt rt (Failure ("illegal global assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_globalstmt ex)); Hashtbl.add globalsymbols s t in 
        let globalvars = List.map globalstmt globals in 

    (* Verify a statement or throw an exception *)
    let rec stmt = function
  Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Local (t, s) as ex -> check_var_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname) t s; report_local_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname) s;
      ignore(Hashtbl.add symbols s t);
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions
