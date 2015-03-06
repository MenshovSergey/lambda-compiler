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
  | [< u = parse; 'Token.Plus; v = parse2 >] -> Ast.Plus (u, v)
  | [< u = parse2 >] -> u

and parse2 = parser
  | [< u = parse2; 'Token.Plus; v = parse3 >] -> Ast.Mul (u, v)
  | [< u = parse3 >] -> u

and parse3 = parser
  | [< 'Token.Number x >] -> Ast.Number x
