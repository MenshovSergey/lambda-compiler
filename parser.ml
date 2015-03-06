(* dummy parser *)

let rec parse_expr = parser
  | [< lhs = parse_summand; stream >] ->
    (parser
      | [< 'Token.Plus; rhs = parse_expr >] -> Ast.Plus(lhs, rhs)
      | [< 'Token.Minus; rhs = parse_expr >] -> Ast.Minus(lhs, rhs)
      | [< >] -> lhs) stream

and parse_summand = parser
  | [< lhs = parse_multiplier; stream >] ->
    (parser
      | [< 'Token.Mul; rhs = parse_summand >] -> Ast.Mul(lhs, rhs)
      | [< 'Token.Div; rhs = parse_summand >] -> Ast.Div(lhs, rhs)
      | [< 'Token.Mod; rhs = parse_summand >] -> Ast.Mod(lhs, rhs)
      | [< >] -> lhs) stream

and parse_multiplier = parser
  | [< 'Token.Number x >] -> Ast.Number x
  | [< 'Token.Keyword '('; e = parse_expr; 'Token.Keyword ')' >] -> e
