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

let is_name_bound = Fun.flip List.mem

let rec pick_fresh_name (ctx : context) (x : string) : context * string =
  if is_name_bound ctx x then pick_fresh_name ctx (x ^ "'") else (x :: ctx, x)
