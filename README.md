# DCL

CircleCI: 
[![CircleCI](https://circleci.com/gh/PLT-DCL/dcl.svg?style=svg)](https://circleci.com/gh/PLT-DCL/dcl)

This is the home for DCL - a new callback-based programming language.

## Generate compiler

To generate the compiler executable:  
  `cd docker`  
  `make compile`  

## Run tests  

To run tests:  
  `cd docker`  
  `make test`  

## Docker

To compile:  
  `cd docker`  
  `make # This puts you inside the docker container`   
  `make # This makes the executable`  
  `./microc.native < <filename>`  
OR  
Run the following command inside the docker environment:  
  `compile <filename>`  

To run:  
  `lli <compiled_file>`

To compile and run inside docker container:  
  `run <filename>`  


## Steps to link C functions to LLVM-generated code

'e.g.' refers to the MicroC example

  `First Step`
  
  add .c file containing the functions to src directory

  e.g.: printbig.c

  `Second Step`
  
  in codegen.ml, declare those functions as built-in

  e.g.:
(* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  `Third Step`
  
  in semant.ml, add those functions to "function declarations for a named function"

  e.g.:
let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] }))
   in

  `Fourth Step`:
  
  add compilation for those functions to Makefile

  e.g.:
printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

  `Fifth Step`:
  
  link in .o files and generate executables in test driver (testall.sh)

  e.g.:
generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    Run "$MICROC" "<" $1 ">" "${basename}.ll" &&
    Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" &&
    Run "./${basename}.exe" > "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff


  `Summary`:
  
  for printbig function in the MicroC example:
  The stock C compiler compiles printbig.o.  testall.sh runs the microc
executable on each testcase (.mc file) to produce a .ll file, invokes
"llc" (the LLVM compiler) to produce a .s (assembly) file, then
invokes "cc" (the stock C compiler) to assemble the .s file, link in
printbig.o, and generate an executable.  See testall.sh for details.
