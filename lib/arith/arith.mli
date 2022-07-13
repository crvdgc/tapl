(** Chapter 3 & 4 Untyped Arithmetic Expressions *)

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

val equal_term : term -> term -> bool

val show_term : term -> string

val size : term -> int

val depth : term -> int

val is_numeric_val : term -> bool

val is_val : term -> bool

exception NoRuleApplies

module type Eval = sig
  val eval : term -> term
end

val eval1 : term -> term

module SmallStep : Eval

val eval_many : term -> term
(** Exercise 4.2.2 *)

module BigStep : Eval
