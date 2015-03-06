(* dummy parser *)
(*let rec parse = parser
  | [< lhs = parse_summand; 'Token.Plus; rhs = parse_summand >] -> Ast.Plus (lhs, rhs)
  | [< >] -> Ast.Number 0


and parse_summand = parser
  | [< lhs = parse_multiplier; 'Token.Mul; rhs = parse_multiplier >] -> Ast.Plus (lhs, rhs)
  | [< >] -> Ast.Number 1

and parse_multiplier = parser
  | [< 'Token.Number x >] -> Ast.Number x*)

let rec parse = parser
  | [< 'Token.Number x; other = parse_a (Ast.Number x) >] -> other

and parse_a lhs = parser
  | [< 'Token.Plus; rhs >] -> Ast.Plus (lhs, (parse rhs))
  | [< 'Token.Number x >] -> Ast.Number x
