(* dummy lexer *)
(*let lex = Token.BinOp ('*', Token.Number 12, Token.BinOp ('+', Token.Number 1, Token.Variable "x"))*)

let rec lex = parser
  (* skip whitespaces *)
  | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream
  (* parse number *)
  | [< ' ('0' .. '9' as c); stream >] ->
    let buffer = Buffer.create 1
    in
    Buffer.add_char buffer c;
    lex_number buffer stream
  | [< ''*'; stream >] ->
    [< 'Token.Mul; lex stream >]
  | [< ''/'; stream >] ->
    [< 'Token.Div; lex stream >]
  | [< ''%'; stream >] ->
    [< 'Token.Mod; lex stream >]
  | [< ''+'; stream >] ->
    [< 'Token.Plus; lex stream >]
  | [< ''-'; stream >] ->
    [< 'Token.Minus; lex stream >]
  | [< ''<'; stream >] ->
    [< 'Token.Lt; lex stream >]
  | [< ''<'; ''='; stream >] ->
    [< 'Token.Le; lex stream >]
  | [< ''>'; stream >] ->
    [< 'Token.Gt; lex stream >]
  | [< ''>'; ''='; stream >] ->
    [< 'Token.Ge; lex stream >]
  | [< ''='; ''='; stream >] ->
    [< 'Token.Eq; lex stream >]
  | [< ''!'; ''='; stream >] ->
    [< 'Token.NEq; lex stream >]
  | [< 'c; stream >] ->
    [< 'Token.Keyword c; lex stream >]
  | [< >] -> [< >]

and lex_number buffer = parser
  | [< ' ('0' .. '9' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_number buffer stream
  | [< stream=lex >] ->
    [< 'Token.Number (int_of_string (Buffer.contents buffer)); stream >]
