let main () =
  let output =
    let tokens = Lexer.lex (Stream.of_channel stdin) in
    let ast    = Parser.parse_expr tokens in
    (*Ast.pretty_print ast;*)
    Syntax.pretty_print (Syntax.getType ast);
  in
  print_endline output
;;

main ()
