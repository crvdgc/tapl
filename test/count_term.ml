open Arith

let t : term = Const Z

let () = Arith.size t |> print_int |> print_newline
