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


let is_val = function
  | TmTrue | TmFalse ->
      true
  | t when is_numeric_val t ->
      true
  | _ ->
      false


module type Eval = sig
  exception NoRuleApplies

  val eval : term -> term
end

exception NoRuleApplies

let rec eval1 = function
  | TmIf t when t.cond = TmTrue ->
      t.tbranch
  | TmIf t when t.cond = TmFalse ->
      t.fbranch
  | TmIf t ->
      TmIf { t with cond = eval1 t.cond }
  | TmSucc t ->
      TmSucc (eval1 t)
  | TmPred TmZero ->
      TmZero
  | TmPred (TmSucc nv) when is_numeric_val nv ->
      nv
  | TmPred t ->
      TmPred (eval1 t)
  | TmIsZero TmZero ->
      TmTrue
  | TmIsZero (TmSucc nv) when is_numeric_val nv ->
      TmFalse
  | TmIsZero t ->
      TmIsZero (eval1 t)
  | _ ->
      raise NoRuleApplies


module SmallStep : Eval = struct
  exception NoRuleApplies = NoRuleApplies

  let eval = eval1
end

let rec eval_many = function
  | TmIf t ->
    ( match eval_many t.cond with
    | TmTrue ->
        eval_many t.tbranch
    | TmFalse ->
        eval_many t.fbranch
    | _ ->
        raise NoRuleApplies )
  | TmSucc t ->
      TmSucc (eval_many t)
  | TmPred t ->
    ( match eval_many t with
    | TmZero ->
        TmZero
    | TmSucc nv when is_numeric_val nv ->
        nv
    | _ ->
        raise NoRuleApplies )
  | TmIsZero t ->
    ( match eval_many t with
    | TmZero ->
        TmTrue
    | TmSucc nv when is_numeric_val nv ->
        TmFalse
    | _ ->
        raise NoRuleApplies )
  | _ ->
      raise NoRuleApplies


module BigStep : Eval = struct
  exception NoRuleApplies = NoRuleApplies

  let eval = eval_many
end
