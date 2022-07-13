open Arith
open OUnit2
open QCheck2

let term_gen =
  Gen.(
    sized
    @@ fix (fun self n ->
           match n with
           | 0 ->
               oneof (List.map pure [ TmTrue; TmFalse; TmZero ])
           | n ->
               let t' = self (n - 1) in
               let constr x = map x t' in
               let if_term =
                 map3
                   (fun cond tbranch fbranch -> TmIf { cond; tbranch; fbranch })
                   t'
                   (self (n - 1))
                   (self (n - 1))
               in
               oneof
                 ( if_term
                 :: List.map
                      constr
                      [ (fun x -> TmSucc x)
                      ; (fun x -> TmPred x)
                      ; (fun x -> TmIsZero x)
                      ] ) ))


let tests =
  "test suite for sum" >::: [ ("empty" >:: fun _ -> assert_equal 0 (1 - 1)) ]


let () =
  run_test_tt_main tests ;
  print_string (Arith.to_string (Gen.generate1 term_gen))
