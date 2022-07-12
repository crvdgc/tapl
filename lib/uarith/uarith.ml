type term =
  | Const of const
  | Ite of { cond : term; tbranch : term; fbranch : term } (* if-then-else *)
  | Succ of term
  | Pred of term
  | IsZero of term

and const = B of bool | Z

let rec consts = function
  | Const c -> [ c ]
  | Ite { cond; tbranch; fbranch } ->
      consts cond @ consts tbranch @ consts fbranch
  | Succ t -> consts t
  | Pred t -> consts t
  | IsZero t -> consts t

let rec size = function
  | Const _ -> 1
  | Ite { cond; tbranch; fbranch } ->
      1 + size cond + size tbranch + size fbranch
  | Succ t -> 1 + size t
  | Pred t -> 1 + size t
  | IsZero t -> 1 + size t

let rec depth = function
  | Const _ -> 1
  | Ite { cond; tbranch; fbranch } ->
      1 + depth cond |> max (depth tbranch) |> max (depth fbranch)
  | Succ t -> 1 + depth t
  | Pred t -> 1 + depth t
  | IsZero t -> 1 + depth t
