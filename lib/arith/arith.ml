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

let rec size = function
  | TmTrue | TmFalse | TmZero ->
      1
  | TmIf { cond; tbranch; fbranch } ->
      1 + size cond + size tbranch + size fbranch
  | TmSucc t | TmPred t | TmIsZero t ->
      1 + size t


let rec depth = function
  | TmTrue | TmFalse | TmZero ->
      1
  | TmIf { cond; tbranch; fbranch } ->
      1 + depth cond |> max (depth tbranch) |> max (depth fbranch)
  | TmSucc t | TmPred t | TmIsZero t ->
      1 + depth t


let rec is_numeric_val = function
  | TmZero ->
      true
  | TmIsZero t ->
      is_numeric_val t
  | _ ->
      false
