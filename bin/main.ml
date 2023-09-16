open Calc_ans

let () =
  let expr =
    BinOp (Div, BinOp (Plus, Val (Int 1), Val (Float 2.5)), Val (Int 3))
  in
  let ans = match eval expr with Ok i -> i | Error e -> failwith e in
  let token_input = "a + (1 + 2) / 4 ^ 5 % 10" in
  let tokens_res = tokenize token_input in
  match tokens_res with
  | Error e -> print_endline (error_loc_with_pointer e token_input)
  | Ok tokens ->
      print_endline
        (expr_to_string expr ^ " = " ^ number_to_string ans ^ "\n" ^ token_input
       ^ " = " ^ token_loc_list_repr tokens)
