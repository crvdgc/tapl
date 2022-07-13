open Arith
open QCheck2

let term_gen =
  let pure_terms = [ TmTrue; TmFalse; TmZero ] in
  let if_term cond tbranch fbranch = TmIf { cond; tbranch; fbranch } in
  let unary_terms =
    [ (fun x -> TmSucc x); (fun x -> TmPred x); (fun x -> TmIsZero x) ]
  in
  let f self = function
    | 0 ->
        Gen.oneof (List.map Gen.pure pure_terms)
    | n ->
        Gen.oneof
          ( Gen.map3 if_term (self (n / 3)) (self (n / 3)) (self (n / 3))
          :: List.map (fun x -> Gen.map x (self (n - 1))) unary_terms )
  in
  Gen.sized @@ Gen.fix f

(* The book's inference rules for big step actually differ from small step when
      the term is 'wrong'.

      For example, `(pred (if true then true else true))` evaluates to `(pred
      true)` in small step semantics, while remains the same in big step
      semantics, since the latter one checks whether the result is a numeric
      value.

      For this reason, we only check good terms.

   let prop_congruity_big_step_small_step =
     QCheck2.(
       Test.make ~print:Arith.show_term term_gen (fun term ->
           Arith.BigStep.eval term = Arith.SmallStep.eval term ))

   let () = QCheck2.Test.check_exn prop_congruity_big_step_small_step
*)

let good_term_gen =
  let pure_bool_terms = [ TmTrue; TmFalse; ] in
  let pure_int_term = TmZero in
  let unary_bool_
