(** Chapter 03 Untyped Arithmetic Expressions *)

type term =
  | TmTrue
  | TmFalse
  | TmIf of
      { cond : term
      ; tbranch : term
      ; fbranch : term
      }
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

val size : term -> int

val depth : term -> int

val is_numeric_val : term -> bool
