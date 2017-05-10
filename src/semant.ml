(* Semantic checking for the DCL compiler *)

module A = Ast
open Ast
open Hashtbl
open Llvm

module StringMap = Map.Make(String)
let formals:(string, A.typ) Hashtbl.t = Hashtbl.create 100
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
  let check_type lvaluet rvaluet err =
     (*let _ = print_endline (string_of_typ lvaluet) in
     let _ = print_endline (string_of_typ rvaluet) in
     let _ = print_endline (string_of_bool(string_of_typ lvaluet == string_of_typ rvaluet)) in
     let _ = print_int (String.length (string_of_typ lvaluet)) in
     let _ = print_int (String.length (string_of_typ rvaluet)) in
     let _ = print_int (String.compare (string_of_typ lvaluet) (string_of_typ rvaluet)) in*)

     (* See if = could be used :O *)
     if (String.compare (string_of_typ lvaluet) (string_of_typ rvaluet)) == 0 then lvaluet else raise err 
  in
   
  (**** Checking Global Variables ****)

   let type_of_identifier s =
      try Hashtbl.find symbols s
      with Not_found -> try Hashtbl.find formals s
                        with Not_found -> try Hashtbl.find globalsymbols s
                                          with Not_found -> raise (Failure ("undeclared identifier " ^ s ))
    in

    (* Return the type of an expression or throw an exception *)

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();
  
  if List.mem "print_line" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print_line may not be defined")) else ();
  
  if List.mem "read" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function read may not be defined")) else ();
  
  if List.mem "write" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function write may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  
    StringMap.add "read" 
    { typ = A.Simple(A.String); fname = "read"; formals = [(Simple(String), "file_name")]; body = [] }
    (StringMap.singleton "write" 
    { typ = A.Simple(A.Int); fname = "write"; formals = [(Simple(String), "file_name") ; (Simple(String), "string_to_write")]; body = [] }) in



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

    let symbol = List.iter (fun (t, n) -> Hashtbl.add formals n t )
  func.formals
    in

    let rec expr = function
	    IntLiteral _ -> Simple(Int)
      | DblLiteral _ -> Simple(Double)
      | StrLiteral _ -> Simple(String)
      | ArrLiteral(l) -> let first_type = expr (List.hd l) in
                         let _ = (match first_type with 
                                    Simple _ -> ()
                                  | _ -> raise (Failure ("'" ^ string_of_expr (List.hd l) ^ "' is not simple and is in array"))
                                 ) in
                         let _ = List.iter (fun x -> if string_of_typ(expr x) == string_of_typ first_type then ()
                                                     else raise (Failure ("'" ^ string_of_expr x ^ "' doesn't match array's type"))) l in
                         Array((match first_type with Simple(x) -> x), 1)
      | DefaultArrLiteral(e1, e2) -> if string_of_typ (expr e1) == string_of_typ(Simple(Int))
                                     then (match expr e2 with
                                               Simple(t) -> Array(t, 1)
                                             | _ -> raise (Failure ("'" ^ string_of_expr e2 ^ "' is not a simple type")))
                                     else raise (Failure ("'" ^ string_of_expr e1 ^ "' is not an integer"))
      | Index(a, i) -> if string_of_typ(expr (List.hd i)) != string_of_typ(Simple(Int))
                       then raise ( Failure("Array index ('" ^ string_of_expr (List.hd i) ^ "') is not an integer") )
                       else 
                         let type_of_entity = expr a in
                         (match type_of_entity with
                            Array(d, _) -> Simple(d)
                          | Simple(String) -> Simple(String)
                          | _ -> raise (Failure ("Entity being indexed ('" ^ string_of_expr a ^"') cannot be array")))
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	(match op with
        Equal | Neq when t1 = t2 -> Simple(Int)
        |  Add | Sub | Mult | Div when t1 = Simple(Int) && t2 = Simple(Int) -> Simple(Int)
	      | Less | Leq | Greater | Geq when t1 = Simple(Int) && t2 = Simple(Int) -> Simple(Int)
	      | And  | Or when t1 = Simple(Int) && t2 = Simple(Int) -> Simple(Int)
        | Exp when t1 = Simple(Int) && t2 = Simple(Int) -> Simple(Double)
        | Add | Sub | Mult | Div | Exp when t1 = Simple(Double) && t2 = Simple(Double) -> Simple(Double)
        | Less | Leq | Greater | Geq 
        when t1 = Simple(Double) && t2 = Simple(Double) -> Simple(Int)

        | Add when t1 = Simple(String) && t2 = Simple(String) -> Simple(String) 
        | Less | Leq | Greater | Geq when t1 = Simple(String) && t2 = Simple(String) -> Simple(Int)
        
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
   (match op with
	   Neg when t = Simple(Int) -> Simple(Int)
	 | Not when t = Simple(Int) -> Simple(Int)
   | Neg when t = Simple(Double) -> Simple(Double)
   | Length when t = Simple(String) -> Simple(Int)
   | Length when t = Array(Double, 1) -> Simple(Int)
   | Length when t = Array(String, 1) -> Simple(Int)
   | Length when t = Array(Int, 1) -> Simple(Int)
   | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_type lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_expr ex))
	  | ArrayAssign(v, i, e) as ex -> let type_of_left_side = 
                                      if string_of_typ(expr (List.hd i)) != string_of_typ(Simple(Int))
                                      then raise ( Failure("Array index ('" ^ string_of_expr (List.hd i) ^ "') is not an integer") )
                                      else 
                                        let type_of_entity = type_of_identifier v in
                                        (match type_of_entity with
                                           Array(d, _) -> Simple(d)
                                         | _ -> raise (Failure ("Entity being indexed ('" ^ v ^"') cannot be array"))) in
                                      let type_of_right_side = expr e in
                                      check_type type_of_left_side type_of_right_side 
                                      (Failure ("illegal assignment " ^ string_of_typ type_of_left_side ^
                                                " = " ^ string_of_typ type_of_right_side ^ " in " ^ 
                                                string_of_expr ex))
      | LocalAssign (t, s, e) as ex -> check_var_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname) t s; report_local_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname) s;
      let lt = t and rt = expr e in
        check_type lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_expr ex)); Hashtbl.add symbols s t; t
      | Call(fname, actuals) as call -> 
         if fname = "print" || fname = "print_line" 
         then (if List.length actuals == 1 
               then let arg_type = string_of_typ (expr (List.hd actuals)) in
                    if arg_type = string_of_typ (Simple(Int)) || 
                       arg_type = string_of_typ (Simple(Double)) ||
                       arg_type = string_of_typ (Simple(String)) 
                    then Void
                    else raise (Failure ("illegal actual argument found " ^ string_of_typ (expr (List.hd actuals)) ^
                                                      " in " ^ string_of_expr (List.hd actuals)))
               else raise (Failure ("expecting 1 argument in " ^ string_of_expr call)))
         else (let fd = function_decl fname in
               if List.length actuals != List.length fd.formals then
                 raise (Failure ("expecting " ^ string_of_int
                   (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
               else List.iter2 (fun (ft, _) e -> let et = expr e in
                       ignore (check_type ft et
                         (Failure ("illegal actual argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
                      fd.formals actuals;
                    fd.typ)
    in

    let check_int_expr e = if string_of_typ (expr e) != string_of_typ (Simple(Int))
     then raise (Failure ("expected int expression in " ^ string_of_expr e))
     else () in

    
    let globalstmt = function 
       Global(t,s) as ex -> check_var_void (fun n -> "illegal void global " ^ n) t s; 
       report_global_duplicate (fun n -> "duplicate global " ^ n) s;
       Hashtbl.add globalsymbols s t;
     | GlobalAssign(t,s,e) as ex -> check_var_void (fun n -> "illegal void global " ^ n) t s; 
     report_global_duplicate (fun n -> "duplicate global " ^ n) s;
     let lt = t and rt = expr e in
        check_type lt rt (Failure ("illegal global assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_globalstmt ex)); Hashtbl.add globalsymbols s t in 
        Hashtbl.clear globalsymbols; let globalvars = List.map globalstmt globals in 

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
           
      | If(p, b1, b2) -> check_int_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_int_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_int_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function functions
