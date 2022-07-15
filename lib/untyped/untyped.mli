type raw_term =
  | RTVar of string
  | RTAbs of string * raw_term
  | RTApp of raw_term * raw_term

type term =
  | TmVar of
      { v : int
      ; ctx_l : int
      }
  | TmAbs of term
  | TmApp of term * term

type context = string list

val is_name_bound : context -> string -> bool

val pick_fresh_name : context -> string -> context * string
