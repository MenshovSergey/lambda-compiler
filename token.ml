(* simple token type *)
type token =
  | Variable of string
  | Number of int
  | Mul
  | Div
  | Mod
  | Plus
  | Minus
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | NEq
  | Keyword of char
