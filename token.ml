(* simple token type *)
type token =
  | Ident of string
  | Number of int
  | Pow
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
