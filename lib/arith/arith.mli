(** Chapter 03 Untyped Arithmetic Expressions *)

type term =
  | Const of const
  | Ite of { cond : term; tbranch : term; fbranch : term } (* if-then-else *)
  | Succ of term
  | Pred of term
  | IsZero of term

and const = B of bool | Z

val consts : term -> const list

val size : term -> int

val depth : term -> int
