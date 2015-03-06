let main () =
  let output =
    let input      = "(3 * oc4m1) * 19 - (17 + 11) % (3 - 1) + 11 / 4 >= 11 - 3 > 7" in
    let lex_stream = Lexer.lex (Stream.of_string input) in
    let ast        = Parser.parse_expr lex_stream in
    Ast.pretty_print ast
  in
  print_string (output ^ "\n")
;;

main ()
