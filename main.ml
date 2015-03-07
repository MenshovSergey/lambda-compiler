let main () =
  let output =
    let tokens = Lexer.lex (Stream.of_channel stdin) in
    let ast    = Parser.parse_expr tokens in
    Ast.toLambda ast
  in
  print_endline output
;;

main ()
