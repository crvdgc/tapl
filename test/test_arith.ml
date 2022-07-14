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

type ret_type =
  | B
  | Z

let good_term_gen =
  let is_zero_term x = TmIsZero x in
  let if_term cond tbranch fbranch = TmIf { cond; tbranch; fbranch } in
  let unary_terms_i = [ (fun x -> TmSucc x); (fun x -> TmPred x) ] in
  let f self (n, ret) : term Gen.t =
    Gen.(
      match n with
      | 0 ->
        ( match ret with
        | B ->
            oneof @@ List.map pure [ TmTrue; TmFalse ]
        | Z ->
            pure TmZero )
      | n ->
        ( match ret with
        | B ->
            oneof
            @@ [ map is_zero_term (self (n - 1, Z))
               ; map3
                   if_term
                   (self (n / 3, B))
                   (self (n / 3, B))
                   (self (n / 3, B))
               ]
        | Z ->
            oneof
            @@ map3
                 if_term
                 (self (n / 3, B))
                 (self (n / 3, Z))
                 (self (n / 3, Z))
               :: List.map (Fun.flip map (self (n - 1, Z))) unary_terms_i ))
  in
  Gen.(sized (fun n -> fix f (n, B)))


let show_good_term () = print_string @@ show_term @@ Gen.generate1 good_term_gen

let () =
  Test.check_exn
  @@ Test.make ~name:"hello" ~print:show_term good_term_gen (fun term ->
         BigStep.eval term = SmallStep.eval term )
