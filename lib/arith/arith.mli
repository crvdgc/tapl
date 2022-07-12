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

val is_val : term -> bool

exception NoRuleApplies

val eval1 : term -> term

module type Eval = sig
  exception NoRuleApplies

  val eval : term -> term
end

module SmallStep : Eval
