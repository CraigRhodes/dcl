(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "DCL"
  and i32_t  = L.i32_type    context
  and i8_t   = L.i8_type     context
  and i1_t   = L.i1_type     context
  and f64_t  = L.double_type context
  and void_t = L.void_type   context in

  let rec int_range = function
      0 -> [ 0 ]
    | n -> int_range (n - 1) @ [ n ] in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.String -> L.pointer_type i8_t
    | A.Double -> f64_t
    | A.Void -> void_t in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = (match t with 
                    A.Int            -> L.const_int          (ltype_of_typ t) 0 
                  | A.Double         -> L.const_float        (ltype_of_typ t) 0.
                  | _ (* A.String *) -> L.const_pointer_null (L.element_type (ltype_of_typ t)))
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let strcmp_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let strcmp_func = L.declare_function "strcmp" strcmp_t the_module in

  let addstr_t = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let addstr_func = L.declare_function "add_str" addstr_t the_module in
  
  let expint_t = L.function_type f64_t [| i32_t ; i32_t |] in
  let expint_func = L.declare_function "exp_int" expint_t the_module in

  let expdbl_t = L.function_type f64_t [| f64_t ; f64_t |] in
  let expdbl_func = L.declare_function "exp_dbl" expdbl_t the_module in

  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let dbl_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder);
  StringMap.add n local m in

      let add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let build_string_from_code e' = let size = L.operand (L.size_of (L.type_of e')) 1 in
                                    let dest = L.build_array_malloc i8_t size "tmp" builder in
                                    List.iter (fun x -> 
                                      let more = (L.build_gep dest  [| L.const_int i32_t x |] "tmp2" builder) in
                                      let x = L.build_extractvalue e' x "tmp2" builder in
                                      ignore (L.build_store x more builder)
                                    ) (int_range ((match (L.int64_of_const size) with Some i -> Int64.to_int i) - 1)) ;
                                    L.build_in_bounds_gep dest [| L.const_int i32_t 0 |] "whatever" builder in
    let clean_up_string_stuff dest = L.build_free dest builder in
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLiteral i -> L.const_int i32_t i
      | A.BoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
      | A.DblLiteral d -> L.const_float f64_t d
      | A.StrLiteral s -> build_string_from_code (L.const_string context s)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
    let e1' = expr builder e1
    and e2' = expr builder e2 in
    (match op with
      A.Add       -> if      L.type_of e1' == ltype_of_typ A.Int    then L.build_add  e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fadd e1' e2' "tmp" builder
                     else                                                L.build_call addstr_func [| e1' ; e2' |] "tmp" builder
    | A.Sub       -> (if L.type_of e1' == ltype_of_typ A.Int then L.build_sub else L.build_fsub) e1' e2' "tmp" builder
    | A.Mult      -> (if L.type_of e1' == ltype_of_typ A.Int then L.build_mul else L.build_fmul) e1' e2' "tmp" builder
    | A.Div       -> (if L.type_of e1' == ltype_of_typ A.Int then L.build_sdiv else L.build_fdiv) e1' e2' "tmp" builder
    | A.Exp       -> if   L.type_of e1' == ltype_of_typ A.Int 
                     then L.build_call expint_func [| e1' ; e2' |] "tmp" builder
                     else L.build_call expdbl_func [| e1' ; e2' |] "tmp" builder
    | A.And       -> L.build_and e1' e2' "tmp" builder
    | A.Or        -> L.build_or e1' e2' "tmp" builder
    | A.Equal     -> if      L.type_of e1' == ltype_of_typ A.Int    then L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
                     else 
                       let result = L.build_call strcmp_func [| e1' ; e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Eq result (L.const_int i32_t 0) "tmp" builder
    | A.Neq       -> if      L.type_of e1' == ltype_of_typ A.Int    then L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
                     else 
                       let result = L.build_call strcmp_func [| e1' ; e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Ne result (L.const_int i32_t 0) "tmp" builder
    | A.Less      -> if      L.type_of e1' == ltype_of_typ A.Int    then    L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
                     else 
                       let result = L.build_call strcmp_func [| e1' ; e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Slt result (L.const_int i32_t 0) "tmp" builder
    | A.Leq       -> if      L.type_of e1' == ltype_of_typ A.Int    then L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
                     else 
                       let result = L.build_call strcmp_func [| e1' ; e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Sle result (L.const_int i32_t 0) "tmp" builder
    | A.Greater   -> if      L.type_of e1' == ltype_of_typ A.Int    then L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
                     else 
                       let result = L.build_call strcmp_func [| e1' ; e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Sgt result (L.const_int i32_t 0) "tmp" builder
    | A.Geq       -> if      L.type_of e1' == ltype_of_typ A.Int    then L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ A.Double then L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
                     else 
                       let result = L.build_call strcmp_func [| e1' ; e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Sge result (L.const_int i32_t 0) "tmp" builder
    )
      | A.Unop(op, e) ->
    let e' = expr builder e in
    (match op with
            A.Neg     -> if L.type_of e' == ltype_of_typ A.Int then L.build_neg else L.build_fneg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in
                     ignore (L.build_store e' (lookup s) builder); e'
      | A.Call ("print_int", [e])  -> L.build_call printf_func [| int_format_str ; expr builder e |] "print_int" builder
      | A.Call ("print_bool", [e]) -> L.build_call printf_func [| int_format_str ; expr builder e |] "print_bool" builder
      | A.Call ("print_double", [e]) -> L.build_call printf_func [| dbl_format_str ; expr builder e |] "print_double" builder

      | A.Call ("print_string", [e]) -> L.build_call printf_func [| str_format_str ; expr builder e |] "print_string" builder
      (* https://www.ibm.com/developerworks/library/os-createcompilerllvm1/ *)

      | A.Call ("printbig", [e]) -> L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (expr builder) (List.rev act)) in
   let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
  A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
    A.Void -> L.build_ret_void builder
  | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
   let merge_bb = L.append_block context "merge" the_function in

   let then_bb = L.append_block context "then" the_function in
   add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
     (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
     (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
