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

let rec pretty_print = function
  | Variable v -> v
  | Number x -> string_of_int x
  | Mul (x, y) -> "(* " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Div (x, y) -> "(/ " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Mod (x, y) -> "(% " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Plus (x, y) -> "(+ " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Minus (x, y) -> "(- " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Lt (x, y) -> "(< " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Le (x, y) -> "(<= " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Gt (x, y) -> "(> " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Ge (x, y) -> "(>= " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | Eq (x, y) -> "(== " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
  | NEq (x, y) -> "(!= " ^ (pretty_print x) ^ " " ^ (pretty_print y) ^ ")"
