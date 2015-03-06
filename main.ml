let main () =
  let output =
    (*let input      = "3 * 19 - 27 + 11 / 4"*)
    let input      = "1 + 2 + 3" in
    let lex_stream = Lexer.lex (Stream.of_string input) in
    let ast        = Parser.parse lex_stream in
    Ast.pretty_print ast
  in
  print_string (output ^ "\n")
;;

main ()
