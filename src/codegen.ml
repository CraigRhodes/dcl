(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

This is for DCL.
*)

module L = Llvm
module A = Ast

open Printf
open List 
open Hashtbl
open Llvm 

module StringMap = Map.Make(String)

let local_vars:(string, llvalue) Hashtbl.t = Hashtbl.create 100 
let global_vars:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let expr_store_local:(string, llvalue) Hashtbl.t = Hashtbl.create 100
let expr_store_global:(string, llvalue) Hashtbl.t = Hashtbl.create 100

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "DCL" in 
  let globalbuilder = builder context 
  and i32_t  = L.i32_type    context
  and i8_t   = L.i8_type     context
  and f64_t  = L.double_type context
  and ptr_t = L.pointer_type (L.i8_type (context)) 
  and void_t = L.void_type   context in

  let rec int_range = function
      0 -> [ ]
    | 1 -> [ 0 ]
    | n -> int_range (n - 1) @ [ n - 1 ] in

  let rec ltype_of_typ = function
      A.Simple(A.Int) -> i32_t
    | A.Simple(A.String) -> L.struct_type context [| i32_t ; L.pointer_type i8_t |]
    | A.Simple(A.Double) -> f64_t
    | A.Array(d, _) ->  L.struct_type context [| i32_t ; L.pointer_type (ltype_of_typ (A.Simple(d))) |]
    | A.Void -> void_t in

  let default = function 
    A.Simple(A.Int)    -> L.const_int          i32_t 0 
  | A.Simple(A.Double) -> L.const_float        f64_t 0.
  | A.Simple(A.String) -> L.const_null (L.struct_type context [| i32_t ; L.pointer_type i8_t |])
  | A.Array(d, _)      -> L.const_null (L.struct_type context [| i32_t ; L.pointer_type (ltype_of_typ (A.Simple(d))) |]) in

  (* Declare each global variable; remember its value in a map *)
  let lookupglobal n = try Hashtbl.find global_vars n 
                       with Not_found -> raise (Failure ("undeclared id: " ^ n ))

    in


  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* String concatenation *)
  let strcmp_t = L.function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
  let strcmp_func = L.declare_function "strcmp" strcmp_t the_module in
  
  (* Exponent *)
  let expint_t = L.function_type f64_t [| i32_t ; i32_t |] in
  let expint_func = L.declare_function "__exp_int" expint_t the_module in

  let expdbl_t = L.function_type f64_t [| f64_t ; f64_t |] in
  let expdbl_func = L.declare_function "__exp_dbl" expdbl_t the_module in

  (* File I/O *)

  let read_t = L.function_type (ltype_of_typ (A.Simple(A.String))) [| ltype_of_typ (A.Simple(A.String)) |] in
  let read_func = L.declare_function "read" read_t the_module in

  let write_t = L.function_type i32_t [| ltype_of_typ (A.Simple(A.String)) ; ltype_of_typ (A.Simple(A.String)) |] in
  let write_func = L.declare_function "write" write_t the_module in

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
    Hashtbl.clear local_vars;
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d" "fmt" builder in
    let dbl_format_str = L.build_global_stringptr "%f" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s" "fmt" builder in
    let int_format_str_nl = L.build_global_stringptr "%d\n" "fmt" builder in
    let dbl_format_str_nl = L.build_global_stringptr "%f\n" "fmt" builder in
    let str_format_str_nl = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)


    (* Return the value for a variable or formal argument *)
    let lookup n = try Hashtbl.find local_vars n 
                   with Not_found -> try Hashtbl.find global_vars n 
                                     with Not_found -> raise (Failure ("undeclared id: " ^ n ))

    in

    let findValue s = if Hashtbl.mem expr_store_local s then Hashtbl.find expr_store_local s
                  else Hashtbl.find expr_store_global s

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
      | A.DblLiteral d -> L.const_float f64_t d
      | A.StrLiteral s -> let llvm_string = L.const_string context s in
                          let size = String.length s in
                          let new_array = L.build_array_malloc i8_t (L.const_int i32_t (size + 1)) "tmp" builder in
                          List.iter (fun x -> 
                            let more = (L.build_gep new_array  [| L.const_int i32_t x |] "tmp2" builder) in
                            let y = if x != size 
                                    then L.build_extractvalue llvm_string x "tmp2" builder
                                    else L.const_int i8_t 0 in
                            ignore (L.build_store y more builder)
                          ) (int_range (size + 1)) ;
                          let new_literal = L.build_malloc (ltype_of_typ (A.Simple(A.String))) "arr_literal" builder in
                          let first_store = L.build_struct_gep new_literal 0 "first" builder in
                          let second_store = L.build_struct_gep new_literal 1 "second" builder in
                          let store_it = L.build_store (L.const_int i32_t size) first_store builder in
                          let store_it_again = L.build_store new_array second_store builder in
                          let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
                          actual_literal
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.ArrLiteral(l) -> let size = L.const_int i32_t (List.length l) in
                           let all = List.map (fun e -> expr builder e) l in
                           let new_array = L.build_array_malloc (L.type_of (List.hd all)) size "tmp" builder in
                           List.iter (fun x ->
                              let more = (L.build_gep new_array [| L.const_int i32_t x |] "tmp2" builder) in
                              let intermediate = List.nth all x in
                              ignore (L.build_store intermediate more builder)
                           ) (int_range (List.length l)) ;
                           let type_of_new_literal = L.struct_type context [| i32_t ; L.pointer_type (L.type_of (List.hd all)) |] in
                           let new_literal = L.build_malloc type_of_new_literal "arr_literal" builder in
                           let first_store = L.build_struct_gep new_literal 0 "first" builder in
                           let second_store = L.build_struct_gep new_literal 1 "second" builder in
                           let store_it = L.build_store size first_store builder in
                           let store_it_again = L.build_store new_array second_store builder in
                           let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
                           actual_literal
      | A.DefaultArrLiteral(e1, e2) -> let size = expr builder e1 in
                                       let first = expr builder e2 in
                                       let d = if L.type_of first == ltype_of_typ (A.Simple(A.Int)) 
                                               then A.Int
                                               else if L.type_of first == ltype_of_typ (A.Simple(A.Double)) 
                                                    then A.Double
                                                    else A.String in
                                       let new_array = L.build_array_malloc (ltype_of_typ (A.Simple(d))) size "tmp" builder in
                                       let start = L.build_alloca i32_t "start" builder in 
                                       let store = L.build_store (L.const_int i32_t 1) start builder in
                                       let first_pointer_in_array = L.build_gep new_array [| (L.const_int i32_t 0) |] "gep_ptr_in_array" builder in
                                       let first_store_in_array = L.build_store first first_pointer_in_array builder in
                                       let position = L.block_parent (L.insertion_block builder) in
                                       let continue_basic_block = L.append_block context "continue" position in
                                       ignore (L.build_br continue_basic_block builder) ;
                                       let iteration_basic_block = L.append_block context "iterate" position in
                                       let end_builder = L.builder_at_end context iteration_basic_block in
                                       let cur_value = L.build_load start "cur_value" end_builder in
                                       let pointer_in_array = L.build_gep new_array [| cur_value |] "gep_ptr_in_array" end_builder in
                                       let store_in_array = L.build_store (expr end_builder e2) pointer_in_array end_builder in
                                       let update = L.build_add cur_value (L.const_int i32_t 1) "tmp" end_builder in
                                       let new_store = L.build_store update start end_builder in
                                       let loop = L.build_br continue_basic_block end_builder in
                                       L.block_terminator (L.insertion_block end_builder) ;
                                       let continue_builder = L.builder_at_end context continue_basic_block in
                                       let cur_value = L.build_load start "cur_value" continue_builder in
                                       let continue_value = L.build_icmp L.Icmp.Slt cur_value size "tmp" continue_builder in
                                       let merge_basic_block = L.append_block context "merge" position in
                                       ignore (L.build_cond_br continue_value iteration_basic_block merge_basic_block continue_builder) ;
                                       L.builder_at_end context merge_basic_block ;
                                       L.position_at_end merge_basic_block builder ;
                                       let new_literal = L.build_malloc (ltype_of_typ (A.Array(d, 1))) "arr_literal" builder in
                                       let first_store = L.build_struct_gep new_literal 0 "first" builder in
                                       let second_store = L.build_struct_gep new_literal 1 "second" builder in 
                                       let store_it = L.build_store size first_store builder in
                                       let store_it_again = L.build_store new_array second_store builder in
                                       let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
                                       actual_literal
      | A.Index(a, i) -> let a' = expr builder a in 
                         let i' = expr builder (List.hd i) in
                         let extract_array = L.build_extractvalue a' 1 "extract_ptr" builder in
                         let extract_value = L.build_gep extract_array [| i' |] "extract_value" builder in
                         if L.type_of extract_array == L.pointer_type i8_t
                         then let first_value = L.build_load extract_value "value" builder in
                              let new_string = L.build_array_malloc i8_t (L.const_int i32_t 2) "tmp" builder in
                              let more = L.build_gep new_string [| L.const_int i32_t 0 |] "tmp2" builder in
                              let store_it = L.build_store first_value more builder in
                              let more = L.build_gep new_string [| L.const_int i32_t 1 |] "tmp2" builder in
                              let store_it_again = L.build_store (L.const_int i8_t 0) more builder in
                              let new_literal = L.build_malloc (ltype_of_typ (A.Simple(A.String))) "arr_literal" builder in
                              let first_store = L.build_struct_gep new_literal 0 "first" builder in
                              let second_store = L.build_struct_gep new_literal 1 "second" builder in
                              let store_it = L.build_store (L.const_int i32_t 2) first_store builder in
                              let store_it_again = L.build_store new_string second_store builder in
                              let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
                              actual_literal
                         else L.build_load extract_value "value" builder
      | A.Binop (e1, op, e2) ->
    let e1' = expr builder e1
    and e2' = expr builder e2 in
    (match op with
      A.Add       -> if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then L.build_add  e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fadd e1' e2' "tmp" builder
                     else 
                          let first_size = L.build_extractvalue e1' 0 "extract_size" builder in
                          let second_size = L.build_extractvalue e2' 0 "extract_size" builder in   
                          let first_array = L.build_extractvalue e1' 1 "extract_size" builder in    
                          let second_array = L.build_extractvalue e2' 1 "extract_size" builder in                             
                          let total_size = L.build_add first_size second_size "tmp" builder in
                          let new_array = L.build_array_malloc i8_t (L.build_add total_size (L.const_int i32_t 1) "tmp" builder) "tmp" builder in
                          let start = L.build_alloca i32_t "start" builder in 
                          let store = L.build_store (L.const_int i32_t 0) start builder in
                          let position = L.block_parent (L.insertion_block builder) in
                          let continue_basic_block = L.append_block context "continue" position in
                          ignore (L.build_br continue_basic_block builder) ;
                          let iteration_basic_block = L.append_block context "iterate" position in
                          let end_builder = L.builder_at_end context iteration_basic_block in
                          let cur_value = L.build_load start "cur_value" end_builder in
                          let pointer_in_array = L.build_gep new_array [| cur_value |] "gep_ptr_in_array" end_builder in
                          let position_for_value = L.build_gep first_array [| cur_value |] "gep_ptr_in_first_array" end_builder in
                          let value_to_store = L.build_load position_for_value "tmp" end_builder in
                          let store_in_array = L.build_store value_to_store pointer_in_array end_builder in
                          let update = L.build_add cur_value (L.const_int i32_t 1) "tmp" end_builder in
                          let new_store = L.build_store update start end_builder in
                          let loop = L.build_br continue_basic_block end_builder in
                          L.block_terminator (L.insertion_block end_builder) ;
                          let continue_builder = L.builder_at_end context continue_basic_block in
                          let cur_value = L.build_load start "cur_value" continue_builder in
                          let continue_value = L.build_icmp L.Icmp.Slt cur_value first_size "tmp" continue_builder in
                          let merge_basic_block = L.append_block context "merge" position in
                          ignore (L.build_cond_br continue_value iteration_basic_block merge_basic_block continue_builder) ;
                          L.builder_at_end context merge_basic_block ;
                          L.position_at_end merge_basic_block builder ;
                          let store = L.build_store (L.const_int i32_t 0) start builder in
                          let position = L.block_parent (L.insertion_block builder) in
                          let continue_basic_block = L.append_block context "continuetwo" position in
                          ignore (L.build_br continue_basic_block builder) ;
                          let iteration_basic_block = L.append_block context "iteratetwo" position in
                          let end_builder = L.builder_at_end context iteration_basic_block in
                          let cur_value = L.build_load start "cur_value" end_builder in
                          let pointer_in_array = L.build_gep new_array [| (L.build_add cur_value first_size "tmp" end_builder) |] "gep_ptr_in_array" end_builder in
                          let position_for_value = L.build_gep second_array [| cur_value |] "gep_ptr_in_first_array" end_builder in
                          let value_to_store = L.build_load position_for_value "tmp" end_builder in
                          let store_in_array = L.build_store value_to_store pointer_in_array end_builder in
                          let update = L.build_add cur_value (L.const_int i32_t 1) "tmp" end_builder in
                          let new_store = L.build_store update start end_builder in
                          let loop = L.build_br continue_basic_block end_builder in
                          L.block_terminator (L.insertion_block end_builder) ;
                          let continue_builder = L.builder_at_end context continue_basic_block in
                          let cur_value = L.build_load start "cur_value" continue_builder in
                          let continue_value = L.build_icmp L.Icmp.Slt cur_value second_size "tmp" continue_builder in
                          let merge_basic_block = L.append_block context "mergetwo" position in
                          ignore (L.build_cond_br continue_value iteration_basic_block merge_basic_block continue_builder) ;
                          L.builder_at_end context merge_basic_block ;
                          L.position_at_end merge_basic_block builder ;
                          let pointer_in_array = L.build_gep new_array [| (L.build_add first_size second_size "tmp" builder) |] "gep_ptr_in_array" builder in
                          let store_in_array = L.build_store (L.const_int i8_t 0) pointer_in_array builder in
                          let new_literal = L.build_malloc (ltype_of_typ (A.Simple(A.String))) "str_literal" builder in
                          let first_store = L.build_struct_gep new_literal 0 "first" builder in
                          let second_store = L.build_struct_gep new_literal 1 "second" builder in 
                          let store_it = L.build_store total_size first_store builder in
                          let store_it_again = L.build_store new_array second_store builder in
                          let actual_literal = L.build_load new_literal "actual_str_literal" builder in
                          actual_literal
    | A.Sub       -> (if L.type_of e1' == ltype_of_typ (A.Simple(A.Int)) then L.build_sub else L.build_fsub) e1' e2' "tmp" builder
    | A.Mult      -> (if L.type_of e1' == ltype_of_typ (A.Simple(A.Int)) then L.build_mul else L.build_fmul) e1' e2' "tmp" builder
    | A.Div       -> (if L.type_of e1' == ltype_of_typ (A.Simple(A.Int)) then L.build_sdiv else L.build_fdiv) e1' e2' "tmp" builder
    | A.Exp       -> if   L.type_of e1' == ltype_of_typ (A.Simple(A.Int)) 
                     then L.build_call expint_func [| e1' ; e2' |] "tmp" builder
                     else L.build_call expdbl_func [| e1' ; e2' |] "tmp" builder
    | A.And       -> L.build_and e1' e2' "tmp" builder
    | A.Or        -> L.build_or e1' e2' "tmp" builder
    | A.Equal     -> let result = (
                     if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
                     else 
                       let str_ptr_e1' = L.build_extractvalue e1' 1 "extract_char_array" builder in
                       let str_ptr_e2' = L.build_extractvalue e2' 1 "extract_char_array" builder in
                       let result = L.build_call strcmp_func [| str_ptr_e1' ; str_ptr_e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Eq result (L.const_int i32_t 0) "tmp" builder) in
                     L.build_mul (L.build_intcast result i32_t "convert" builder) (L.const_int i32_t (-1)) "tmp" builder
    | A.Neq       -> let result = (
                     if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
                     else 
                       let str_ptr_e1' = L.build_extractvalue e1' 1 "extract_char_array" builder in
                       let str_ptr_e2' = L.build_extractvalue e2' 1 "extract_char_array" builder in
                       let result = L.build_call strcmp_func [| str_ptr_e1' ; str_ptr_e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Ne result (L.const_int i32_t 0) "tmp" builder) in
                     L.build_mul (L.build_intcast result i32_t "convert" builder) (L.const_int i32_t (-1)) "tmp" builder
    | A.Less      -> let result = (
                     if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then    L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
                     else 
                       let str_ptr_e1' = L.build_extractvalue e1' 1 "extract_char_array" builder in
                       let str_ptr_e2' = L.build_extractvalue e2' 1 "extract_char_array" builder in
                       let result = L.build_call strcmp_func [| str_ptr_e1' ; str_ptr_e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Slt result (L.const_int i32_t 0) "tmp" builder) in
                     L.build_mul (L.build_intcast result i32_t "convert" builder) (L.const_int i32_t (-1)) "tmp" builder
    | A.Leq       -> let result = (
                     if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
                     else 
                       let str_ptr_e1' = L.build_extractvalue e1' 1 "extract_char_array" builder in
                       let str_ptr_e2' = L.build_extractvalue e2' 1 "extract_char_array" builder in
                       let result = L.build_call strcmp_func [| str_ptr_e1' ; str_ptr_e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Sle result (L.const_int i32_t 0) "tmp" builder) in
                     L.build_mul (L.build_intcast result i32_t "convert" builder) (L.const_int i32_t (-1)) "tmp" builder
    | A.Greater   -> let result = (
                     if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
                     else 
                       let str_ptr_e1' = L.build_extractvalue e1' 1 "extract_char_array" builder in
                       let str_ptr_e2' = L.build_extractvalue e2' 1 "extract_char_array" builder in
                       let result = L.build_call strcmp_func [| str_ptr_e1' ; str_ptr_e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Sgt result (L.const_int i32_t 0) "tmp" builder) in
                     L.build_mul (L.build_intcast result i32_t "convert" builder) (L.const_int i32_t (-1)) "tmp" builder
    | A.Geq       -> let result = (
                     if      L.type_of e1' == ltype_of_typ (A.Simple(A.Int))    then L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
                     else if L.type_of e1' == ltype_of_typ (A.Simple(A.Double)) then L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
                     else 
                       let str_ptr_e1' = L.build_extractvalue e1' 1 "extract_char_array" builder in
                       let str_ptr_e2' = L.build_extractvalue e2' 1 "extract_char_array" builder in
                       let result = L.build_call strcmp_func [| str_ptr_e1' ; str_ptr_e2' |] "tmp" builder in
                       L.build_icmp L.Icmp.Sge result (L.const_int i32_t 0) "tmp" builder) in
                     L.build_mul (L.build_intcast result i32_t "convert" builder) (L.const_int i32_t (-1)) "tmp" builder
    )
      | A.TildeOp(id) -> let x = "~" ^ id in 
                      (*if Hashtbl.mem expr_store_global x 
                      then*)
                        L.build_load (lookup (x)) x builder 
                     (* else 
                        ignore (Hashtbl.add expr_store_global x () ); L.build_load (lookup (x)) id builder *)

      | A.Unop(op, e) ->
    let e' = expr builder e in
    (match op with
            A.Neg     -> if L.type_of e' == ltype_of_typ (A.Simple(A.Int)) then L.build_neg e' "tmp" builder else L.build_fneg e' "tmp" builder
          | A.Not     -> L.build_not e' "tmp" builder
          | A.Length  -> L.build_extractvalue e' 0 "extract_size" builder)
      | A.Assign (s, e) -> let e' = expr builder e in
                     ignore (L.build_store e' (lookup s) builder); ignore (Hashtbl.add expr_store_local s e'); e'
      | A.LocalAssign (t, s, e) -> let local_var = L.build_alloca (ltype_of_typ t) s builder in
       Hashtbl.add local_vars s local_var;
      let e' = expr builder e in ignore (L.build_store e' local_var builder); ignore (Hashtbl.add expr_store_local s e'); e'
      (* File I/O calls *)
      | A.Call ("read", [e]) -> let temp = expr builder e in
                                L.build_call read_func [| temp |] "read" builder  
      | A.Call ("write", [e1 ; e2]) -> let temp1 = expr builder e1 in
                                       let temp2 = expr builder e2 in
                                       L.build_call write_func [| temp1 ; temp2 |] "write" builder  
      (* https://www.ibm.com/developerworks/library/os-createcompilerllvm1/ *)
      | A.ArrayAssign(v, i, e) -> let e' = expr builder e in 
                                  let i' = expr builder (List.hd i) in
                                  let v' = L.build_load (lookup v) v builder in 
                                  let extract_array = L.build_extractvalue v' 1 "extract_ptr" builder in
                                  let extract_value = L.build_gep extract_array [| i' |] "extract_value" builder in
                                  ignore (L.build_store e' extract_value builder); e'
      | A.Call("print", [e]) -> let e' = expr builder e in 
                                if L.type_of e' == ltype_of_typ (A.Simple(A.Int))
                                then L.build_call printf_func [| int_format_str ; e' |] "print" builder
                                else if L.type_of e' == ltype_of_typ (A.Simple(A.Double))
                                     then L.build_call printf_func [| dbl_format_str ; e' |] "print" builder
                                     else L.build_call printf_func [| str_format_str ; L.build_extractvalue e' 1 "extract_char_array" builder |] "print" builder
      | A.Call("print_line", [e]) -> let e' = expr builder e in 
                                     if L.type_of e' == ltype_of_typ (A.Simple(A.Int))
                                     then L.build_call printf_func [| int_format_str_nl ; e' |] "print" builder
                                     else if L.type_of e' == ltype_of_typ (A.Simple(A.Double))
                                          then L.build_call printf_func [| dbl_format_str_nl ; e' |] "print" builder
                                          else L.build_call printf_func [| str_format_str_nl ; L.build_extractvalue e' 1 "extract_char_array" builder |] "print" builder
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

      let globalstmt = function
    A.Global(t,s) -> let global_var = L.build_alloca (ltype_of_typ t) s builder in
       Hashtbl.add global_vars s global_var;
  | A.GlobalAssign(t,s,e) -> let global_var = L.build_alloca (ltype_of_typ t) s builder in
       Hashtbl.add global_vars s global_var;
       let e' = expr builder e in 
    ignore (L.build_store e' (lookup s) builder); ignore (Hashtbl.add expr_store_global s e') in 

      let globalvars = List.map globalstmt globals in 

    let add_formal (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore(L.build_store p local builder);
  Hashtbl.add local_vars n local in 

  let formals = ignore(List.iter2 add_formal fdecl.A.formals 
  (Array.to_list (L.params the_function))) in 
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
  A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); ignore (if String.sub fdecl.A.fname 0 2 = "__" then () (* Don't generate calls to callback within a callback *) 
                    else (
                      let callbackStrMap = StringMap.filter (let x k v = (String.sub k 0 2) = "__" in x ) function_decls in 
                      let callbackList = StringMap.bindings callbackStrMap in
                      for j = 0 to ((List.length callbackList) - 1) do
                        let (key, (fdef, fdec)) = List.nth callbackList j in 
                        let newStr = String.create ((String.length fdec.A.fname) - 2) in 
                        let varName = ignore(for i = 0 to ((String.length newStr) - 1) do String.set newStr i (String.get fdec.A.fname (i + 2)) done); newStr in
                        let actuals = [(findValue varName)] in
                        let result = "" in
                        ignore ( L.build_call fdef (Array.of_list actuals) result builder ) 
                        (* Hashtbl.iter (fun a b -> print_endline (L.string_of_llvalue (Hashtbl.find local_vars a)) ; print_endline a) local_vars *)
                      done
                    ) 
              ) ; builder
      | A.Return e -> ignore (match fdecl.A.typ with
    A.Void -> L.build_ret_void builder
  | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         let bool_val = L.build_trunc bool_val (L.i1_type context) "convert" builder in
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
    let bool_val = L.build_trunc bool_val (L.i1_type context) "convert" pred_builder in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb
      | A.Local (t, s) -> 
      ignore (let local_var = L.build_alloca (ltype_of_typ t) s builder in
       Hashtbl.add local_vars s local_var); builder
      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) ;
  in

  List.iter build_function_body functions;
  the_module
