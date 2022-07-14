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
[@@deriving eq]

let rec show_term = function
  | TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf t ->
      Format.sprintf
        "(if %s then %s else %s)"
        (show_term t.cond)
        (show_term t.tbranch)
        (show_term t.fbranch)
  | TmZero ->
      "0"
  | TmSucc t ->
      Format.sprintf "(succ %s)" (show_term t)
  | TmPred t ->
      Format.sprintf "(pred %s)" (show_term t)
  | TmIsZero t ->
      Format.sprintf "(iszero %s)" (show_term t)


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
  | TmSucc t ->
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
  let rec eval t =
    try
      let t' = eval1 t in
      eval t'
    with
    | NoRuleApplies ->
        t
end

let rec eval_many' = function
  | TmIf t ->
    ( match eval_many t.cond with
    | TmTrue ->
        eval_many t.tbranch
    | TmFalse ->
        eval_many t.fbranch
    | _ ->
        raise NoRuleApplies )
  | TmSucc t ->
    ( match eval_many t with
    | nv when is_numeric_val nv ->
        TmSucc nv
    | _ ->
        raise NoRuleApplies )
  | TmPred t ->
    ( match eval_many t with
    | TmZero ->
        TmZero
    | TmSucc nv when is_numeric_val nv ->
        nv
    (* The book's implementation prevents evalutaion inside pred, which is
     * actually different from the small step semantics. *)
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


and eval_many t = try eval_many' t with NoRuleApplies -> t

module BigStep : Eval = struct
  let eval = eval_many
end
