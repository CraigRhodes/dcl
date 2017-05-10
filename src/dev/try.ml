#The OCaml code that drives the binary to test parsing.

let lexbuf = Lexing.from_channel stdin in
let ast = Parser.program Scanner.token lexbuf in
print_string (Ast.string_of_program ast)
