let main () =
  let output =
    let input      = "3 * 19 - 27 % 9 + 11 / 4" in
    let lex_stream = Lexer.lex (Stream.of_string input) in
    let ast        = Parser.parse_expr lex_stream in
    Ast.pretty_print ast
  in
  print_string (output ^ "\n")
;;

main ()
