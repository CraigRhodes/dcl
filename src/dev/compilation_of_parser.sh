# This will build a `try` binary that will take source code and pretty print the program using AST. (Obviously, run this in the `src` folder.)
# It can let you test whether a source program has a syntax error or whether your parser has all the features you think it's supposed to have.

ocamlc -c ast.ml &&
ocamllex scanner.mll && 
ocamlyacc -v parser.mly && 
ocamlc -c parser.mli && 
ocamlc -c scanner.ml && 
ocamlc -c parser.ml && 
ocamlc -c try.ml && 
ocamlc -o try ast.cmo scanner.cmo parser.cmo try.cmo
