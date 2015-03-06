type expr =
  | Variable of string
  | Number of int
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Lt of expr * expr
  | Le of expr * expr
  | Gt of expr * expr
  | Ge of expr * expr
  | Eq of expr * expr
  | NEq of expr * expr

let rec print_ast = function
  | Ast.Variable v -> v
  | Ast.Number x -> string_of_int x
  | Ast.Mul (x, y) -> "(* " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Div (x, y) -> "(/ " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Mod (x, y) -> "(% " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Plus (x, y) -> "(+ " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Minus (x, y) -> "(- " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Lt (x, y) -> "(< " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Le (x, y) -> "(<= " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Gt (x, y) -> "(> " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Ge (x, y) -> "(>= " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.Eq (x, y) -> "(== " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
  | Ast.NEq (x, y) -> "(!= " ^ (print_ast x) ^ " " ^ (print_ast y) ^ ")"
