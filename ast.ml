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
