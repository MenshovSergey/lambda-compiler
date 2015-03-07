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
  | Assign of expr * expr






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
  | Assign (x, y) -> ((pretty_print x) ^ ":=" ^ pretty_print y)

let rec numeral x = 
  match x  with 
  | 0 -> "z"
  | _ -> "s(" ^ (numeral (x - 1) ) ^ ")"

let rec toLambda = function
  | Variable v -> v
  | Number x -> "(lsz." ^ (numeral x) ^ ")"
  | Mul (left, right) -> "((lmnsz.m (n s) z) " ^ toLambda left ^ " " ^ toLambda right ^ ")"
  | Plus (left, right) -> "((lmnsz.m s (n s z)) " ^ toLambda left ^ " " ^ toLambda right ^ ")"
  | Minus (left, right) -> "((lab.rec b pred a) " ^ toLambda left ^ " " ^ toLambda right ^ ")"
  | Div (left, right) -> "((Y(lfnm.iif (le n m) (lsz.z) plus ((f (minus n m) m) 1))) " ^ toLambda left ^ toLambda right ^ ")"
  | Mod (left, right) -> "((Y(lfnm.iif (gt n m) (f (minus n m) m)) n)" ^ toLambda left ^ toLambda right ^ ")"
  | Eq (left, right) -> "((lab.and (isZero (minus a b)) (isZero (minus b a) ) tru fls )" ^ toLambda left ^ toLambda right ^ ")"
  | NEq (left, right) -> "((lab.and (isZero (minus a b)) (isZero (minus b a) ) fls tru )" ^ toLambda left ^ toLambda right ^ ")"
  | Lt (left, right) -> "((lab.NEq (minus b a) (lsz.z)" ^ toLambda left ^ toLambda right ^ ")"
  | Gt (left, right) -> "((lab.NEq (minus a b) (lsz.z)" ^ toLambda left ^ toLambda right ^ ")"
  | Le (left, right) -> "((lab.or (Lt a b) (Eq a b))" ^ toLambda left ^ toLambda right ^ ")"
  | Ge (left, right) -> "((lab.or (Gt a b) (Eq a b))" ^ toLambda left ^ toLambda right ^ ")"
  | _ -> "ToDo"