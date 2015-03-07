(* dummy parser *)

(* expression *)
let rec parse_expr = parser
  | [< lhs = parse_1;
       rest = parser
         | [< 'Token.Eq; rhs = parse_expr >] -> Ast.Eq(lhs, rhs)
         | [< 'Token.NEq; rhs = parse_expr >] -> Ast.NEq(lhs, rhs)
         | [< >] -> lhs >] -> rest

(* equality args *)
and parse_1 = parser
  | [< lhs = parse_2;
       rest = parser
         | [< 'Token.Lt; rhs = parse_1 >] -> Ast.Lt(lhs, rhs)
         | [< 'Token.Le; rhs = parse_1 >] -> Ast.Le(lhs, rhs)
         | [< 'Token.Gt; rhs = parse_1 >] -> Ast.Gt(lhs, rhs)
         | [< 'Token.Ge; rhs = parse_1 >] -> Ast.Ge(lhs, rhs)
         | [< >] -> lhs >] -> rest

(* comparison args *)
and parse_2 = parser
  | [< lhs = parse_summand;
       rest = parser
         | [< 'Token.Plus; rhs = parse_2 >] -> Ast.Plus(lhs, rhs)
         | [< 'Token.Minus; rhs = parse_2 >] -> Ast.Minus(lhs, rhs)
         | [< >] -> lhs >] -> rest

(* addition args *)
and parse_summand = parser
  | [< lhs = parse_multiplier;
       rest = parser
         | [< 'Token.Mul; rhs = parse_summand >] -> Ast.Mul(lhs, rhs)
         | [< 'Token.Div; rhs = parse_summand >] -> Ast.Div(lhs, rhs)
         | [< 'Token.Mod; rhs = parse_summand >] -> Ast.Mod(lhs, rhs)
         | [< >] -> lhs >] -> rest

(* multiplication args *)
and parse_multiplier = parser
  | [< lhs = parse_power;
       rest = parser
         | [< 'Token.Pow; rhs = parse_multiplier >] -> Ast.Pow(lhs, rhs)
         | [< >] -> lhs >] -> rest

(* power args *)
and parse_power = parser
  | [< 'Token.Number x >] -> Ast.Number x
  | [< 'Token.Ident v >] -> Ast.Variable v
  | [< 'Token.Keyword '('; e = parse_expr; 'Token.Keyword ')' >] -> e
