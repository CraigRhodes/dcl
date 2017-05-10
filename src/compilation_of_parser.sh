ocamlc -c ast.ml &&
ocamllex scanner.mll && 
ocamlyacc -v parser.mly && 
ocamlc -c parser.mli && 
ocamlc -c scanner.ml && 
ocamlc -c parser.ml && 
ocamlc -c try.ml && 
ocamlc -o try ast.cmo scanner.cmo parser.cmo try.cmo
