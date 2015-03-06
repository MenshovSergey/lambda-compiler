(* dummy lexer *)

let rec lex = parser
  (* skip whitespaces *)
  | [< ' (' ' | '\n' | '\r' | '\t'); stream = lex >] -> stream

  (* number *)
  | [< ' ('0' .. '9' as c); stream >] ->
    let buffer = Buffer.create 1 in
    Buffer.add_char buffer c;
    lex_number buffer stream

  (* ident *)
  | [< ' ('a' .. 'z' | 'A' .. 'Z' as c); stream >] ->
    let buffer = Buffer.create 1 in
    Buffer.add_char buffer c;
    lex_ident buffer stream

  (* ariphmetic binary ops *)
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

  (* comparison ops *)
  | [< ''<';
       rest = parser
         | [< ''='; stream = lex >] -> [< 'Token.Le; stream >]
         | [< stream = lex >] -> [< 'Token.Lt; stream >] >] -> rest
  | [< ''>';
       rest = parser
         | [< ''='; stream = lex >] -> [< 'Token.Ge; stream >]
         | [< stream = lex >] -> [< 'Token.Gt; stream >] >] -> rest

  (* equality / inequality ops *)
  | [< ''='; ''='; stream >] ->
    [< 'Token.Eq; lex stream >]
  | [< ''!'; ''='; stream >] ->
    [< 'Token.NEq; lex stream >]

  (* other chars *)
  | [< 'c; stream = lex >] ->
    [< 'Token.Keyword c; stream >]

  (* empty *)
  | [< >] -> [< >]

(* number lexer with buffer *)
and lex_number buffer = parser
  | [< ' ('0' .. '9' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_number buffer stream
  | [< stream = lex >] ->
    [< 'Token.Number (int_of_string (Buffer.contents buffer)); stream >]

(* ident lexer with buffer *)
and lex_ident buffer = parser
  | [< ' ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c); stream >] ->
    Buffer.add_char buffer c;
    lex_ident buffer stream
  | [< stream = lex >] ->
    [< 'Token.Ident (Buffer.contents buffer); stream >]
