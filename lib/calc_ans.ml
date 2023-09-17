type bin_op = Plus | Sub | Mul | Div | Mod | Pow

let binop_to_string (op : bin_op) : string =
  match op with
  | Plus -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "^"

type number = Int of int | Float of float
type expr = Val of number | Op of bin_op

let number_to_string (num : number) : string =
  match num with Int i -> string_of_int i | Float f -> string_of_float f

let int_divide_if_divisible (i1 : number) (i2 : number) : number =
  match (i1, i2) with
  | Int i1, Int i2 ->
      if i1 mod i2 = 0 then Int (i1 / i2)
      else Float (float_of_int i1 /. float_of_int i2)
  | Int i1, Float f2 -> Float (float_of_int i1 /. f2)
  | Float f1, Int i2 -> Float (f1 /. float_of_int i2)
  | Float f1, Float f2 -> Float (f1 /. f2)

let truncate_number (num : number) : number =
  match num with
  | Int i -> Int i
  | Float f -> if f -. floor f = 0.0 then Int (int_of_float f) else Float f

let divide_smart (i1 : number) (i2 : number) : (number, string) result =
  match i2 with
  | Int 0 -> Error "Cannot divide by zero"
  | Float 0.0 -> Error "Cannot divide by zero"
  | _ -> Ok (truncate_number (int_divide_if_divisible i1 i2))

type token = Number of number | Op of bin_op | LParen | RParen | Ans
type token_loc = token * int
type error_loc = Str of string | WithLoc of string * int

let token_to_string (token : token) : string =
  match token with
  | Number n -> number_to_string n
  | Op o -> binop_to_string o
  | LParen -> "("
  | RParen -> ")"
  | Ans -> "Ans"

let point_to_error_text (error_loc : error_loc) (offset : int) : string =
  match error_loc with
  | Str e -> Printf.sprintf "Error: %s" e
  | WithLoc (e, loc) ->
      let pointer = String.make (loc + offset) ' ' ^ "^" in
      pointer ^ "\n" ^ e

let error_loc_with_pointer (error_loc : error_loc) (input : string) : string =
  match error_loc with
  | Str e -> Printf.sprintf "Error: %s" e
  | WithLoc _ ->
      let pointer = point_to_error_text error_loc 0 in
      input ^ "\n" ^ pointer

let token_loc_repr (token_loc : token_loc) : string =
  match token_loc with
  | Number n, _ -> "<Num " ^ number_to_string n ^ ">"
  | Op o, _ -> "<Op " ^ binop_to_string o ^ ">"
  | Ans, _ -> "<Ans>"
  | LParen, _ -> "<Lparen>"
  | RParen, _ -> "<Rparen>"

let token_loc_list_repr (token_loc_list : token_loc list) : string =
  token_loc_list |> List.map token_loc_repr |> String.concat " "

let find_last_unbalanced_paren (lst : token_loc list) : error_loc option =
  let rec find_last_unbalanced_paren_aux (lst : token_loc list) (depth : int)
      (paren_stack : token_loc list) : error_loc option =
    match lst with
    | [] -> (
        match depth with
        | 0 -> None
        | _ -> (
            match paren_stack with
            | [] ->
                failwith
                  (Printf.sprintf "Depth %d but paren stack is empty" depth)
            | (_, loc) :: _ ->
                Some
                  (WithLoc
                     ( "Open parenthesis without matching closed parenthesis",
                       loc ))))
    | (LParen, loc) :: cons_stack ->
        find_last_unbalanced_paren_aux cons_stack (depth + 1)
          ((LParen, loc) :: paren_stack)
    | (RParen, loc) :: cons_stack -> (
        match paren_stack with
        | [] ->
            Some
              (WithLoc
                 ("Closed parenthesis without matching open parenthesis", loc))
        | (LParen, _) :: parent_cons_stack ->
            find_last_unbalanced_paren_aux cons_stack (depth - 1)
              parent_cons_stack
        | _ -> failwith "Paren stack should only contain LParens")
    | _ :: rest -> find_last_unbalanced_paren_aux rest depth paren_stack
  in
  find_last_unbalanced_paren_aux lst 0 []

let rec tokenize_aux (input : string) (cursor : int) (acc : token_loc list)
    (depth : int) : (token_loc list, error_loc) result =
  if cursor >= String.length input then
    let tokens_rev = acc |> List.rev in
    match depth with
    | 0 -> Ok tokens_rev
    | _ -> (
        match find_last_unbalanced_paren tokens_rev with
        | None -> Ok tokens_rev
        | Some error_loc -> Error error_loc)
  else
    let c = String.get input cursor in
    match c with
    | '\n' | '\r' -> Error (Str "Newline not allowed in input")
    | ' ' | '\t' -> tokenize_aux input (cursor + 1) acc depth
    | '+' -> tokenize_aux input (cursor + 1) ((Op Plus, cursor) :: acc) depth
    | '-' -> tokenize_aux input (cursor + 1) ((Op Sub, cursor) :: acc) depth
    | '*' -> tokenize_aux input (cursor + 1) ((Op Mul, cursor) :: acc) depth
    | '/' -> tokenize_aux input (cursor + 1) ((Op Div, cursor) :: acc) depth
    | '%' -> tokenize_aux input (cursor + 1) ((Op Mod, cursor) :: acc) depth
    | '^' -> tokenize_aux input (cursor + 1) ((Op Pow, cursor) :: acc) depth
    | '(' ->
        tokenize_aux input (cursor + 1) ((LParen, cursor) :: acc) (depth + 1)
    | ')' ->
        tokenize_aux input (cursor + 1) ((RParen, cursor) :: acc) (depth - 1)
    | _ -> (
        let rec read_number (input : string) (cursor : int) (acc : string) :
            string =
          if cursor >= String.length input then acc
          else
            let c = String.get input cursor in
            match c with
            | '0' .. '9' ->
                read_number input (cursor + 1) (acc ^ String.make 1 c)
            | '.' -> read_number input (cursor + 1) (acc ^ String.make 1 c)
            | _ -> acc
        in
        let rec read_text (input : string) (cursor : int) (acc : string) :
            string =
          if cursor >= String.length input then acc
          else
            let c = String.get input cursor in
            match c with
            | 'a' .. 'z' | 'A' .. 'Z' ->
                read_text input (cursor + 1) (acc ^ String.make 1 c)
            | _ -> acc
        in
        let num_str = read_number input cursor "" in
        let text_str = read_text input cursor "" |> String.lowercase_ascii in
        (* if nothing could be read here, then we couldn't read an int or float, so its an unknown character *)
        match (num_str, text_str) with
        | _, "ans" ->
            tokenize_aux input
              (cursor + String.length text_str)
              ((Ans, cursor) :: acc) depth
        | num_str, _ when String.length num_str > 0 ->
            let num =
              if String.contains num_str '.' then
                Float (float_of_string num_str)
              else Int (int_of_string num_str)
            in
            tokenize_aux input
              (cursor + String.length num_str)
              ((Number num, cursor) :: acc)
              depth
        | _, _ ->
            if String.length text_str > 0 then
              Error
                (WithLoc ("Unknown token in input: '" ^ text_str ^ "'", cursor))
            else
              Error
                (WithLoc ("Unknown character '" ^ String.make 1 c ^ "'", cursor))
        )

let tokenize (input : string) = tokenize_aux input 0 [] 0

let operator_precedence (tk : token) : int =
  match tk with
  | Op o -> ( match o with Plus | Sub -> 1 | Mul | Div | Mod -> 2 | Pow -> 3)
  | _ -> failwith "Fatal: operator_precedence called on non-operator"

let token_is_left_associative (token : token) : bool =
  match token with
  | Op o -> ( match o with Pow -> false | _ -> true)
  | _ -> false

let hd_or_none (lst : 'a list) : 'a option =
  match lst with [] -> None | hd :: _ -> Some hd

(*
Infix -> Postfix using the Shunting-yard algorithm
shunting-yard algorithm https://en.wikipedia.org/wiki/Shunting_yard_algorithm

while there are tokens to be read:
    read a token
    if the token is:
    - a number:
        put it into the output queue
    - an operator o1:
        while (
            there is an operator o2 at the top of the operator stack which is not a left parenthesis,
            and (o2 has greater precedence than o1 or (o1 and o2 have the same precedence and o1 is left-associative))
        ):
            pop o2 from the operator stack into the output queue
        push o1 onto the operator stack
    - a left parenthesis (i.e. "("):
        push it onto the operator stack
    - a right parenthesis (i.e. ")"):
        while the operator at the top of the operator stack is not a left parenthesis:
            {assert the operator stack is not empty}
            /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
            pop the operator from the operator stack into the output queue
        {assert there is a left parenthesis at the top of the operator stack}
        pop the left parenthesis from the operator stack and discard it
        if there is a function token at the top of the operator stack, then:
            pop the function from the operator stack into the output queue

  After the while loop, pop the remaining items from the operator stack into the output queue. */
      while there are tokens on the operator stack:
      /* If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses. */
      {assert the operator on top of the stack is not a (left) parenthesis}
      pop the operator from the operator stack onto the output queue *)

type postfix_expr_token = Val of number | Op of bin_op | Ans
type postfix_expr_token_loc = postfix_expr_token * int

let postfix_expr_token_to_string (token : postfix_expr_token) : string =
  match token with
  | Val n -> number_to_string n
  | Op o -> binop_to_string o
  | Ans -> "Ans"

let postfix_expr_token_list_to_string (tokens : postfix_expr_token list) :
    string =
  tokens |> List.map postfix_expr_token_to_string |> String.concat " "

let tokens_to_postfix (tokens : token_loc list) : postfix_expr_token_loc list =
  let rec parse_expr_aux (tokens : token_loc list)
      (output : postfix_expr_token_loc list) (operator_stack : token_loc list) :
      postfix_expr_token_loc list =
    (* After the while loop, pop the remaining items from the operator stack into the output queue. */
       while there are tokens on the operator stack:
       /* If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses. */
       {assert the operator on top of the stack is not a (left) parenthesis}
       pop the operator from the operator stack onto the output queue *)
    match tokens with
    | [] ->
        let rec pop_operators (operator_stack : token_loc list)
            (output : postfix_expr_token_loc list) : postfix_expr_token_loc list
            =
          match operator_stack with
          | [] -> output |> List.rev
          | (Op o, l) :: rest -> pop_operators rest ((Op o, l) :: output)
          | (Ans, l) :: rest -> pop_operators rest ((Ans, l) :: output)
          | (Number _, _) :: _ ->
              failwith "Fatal: number should not be on operator stack"
          | (LParen, _) :: _ ->
              failwith "Fatal: left parenthesis should not be on operator stack"
          | (RParen, _) :: _ ->
              failwith
                "Fatal: right parenthesis should not be on operator stack"
        in
        pop_operators operator_stack output
    | (Number n, l) :: rest ->
        parse_expr_aux rest ((Val n, l) :: output) operator_stack
    | (LParen, l) :: rest ->
        parse_expr_aux rest output ((LParen, l) :: operator_stack)
    | (Ans, l) :: rest ->
        parse_expr_aux rest ((Ans, l) :: output) operator_stack
    | (RParen, _) :: rest ->
        (*
        while the operator at the top of the operator stack is not a left parenthesis:
            {assert the operator stack is not empty}
            /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
            pop the operator from the operator stack into the output queue
        {assert there is a left parenthesis at the top of the operator stack}
        pop the left parenthesis from the operator stack and discard it*)
        let rec pop_until_left_paren (operator_stack : token_loc list)
            (output : postfix_expr_token_loc list) :
            token_loc list * postfix_expr_token_loc list =
          match operator_stack with
          | [] -> failwith "Fatal: mismatched parentheses"
          | (LParen, _) :: rest -> (rest, output)
          | (Op o, l) :: rest -> pop_until_left_paren rest ((Op o, l) :: output)
          | (Ans, l) :: rest -> pop_until_left_paren rest ((Ans, l) :: output)
          | (Number _, _) :: _ ->
              failwith "Fatal: number should not be on operator stack"
          | (RParen, _) :: _ ->
              failwith
                "Fatal: right parenthesis should not be on operator stack"
        in
        let new_operator_stack, new_output =
          pop_until_left_paren operator_stack output
        in
        parse_expr_aux rest new_output new_operator_stack
        (*- an operator o1:
            while (
                there is an operator o2 at the top of the operator stack which is not a left parenthesis,
                and (o2 has greater precedence than o1 or (o1 and o2 have the same precedence and o1 is left-associative))
            ):
                pop o2 from the operator stack into the output queue
            push o1 onto the operator stack *)
    | (Op new_o, op_loc) :: rest -> (
        match operator_stack with
        | [] ->
            (* stack was empty, so just push operator onto stack *)
            parse_expr_aux rest output ((Op new_o, op_loc) :: operator_stack)
        | _ ->
            let top_of_stack_has_greater_precedence (new_token : token_loc)
                (operator_stack : token_loc list) : bool =
              match operator_stack with
              | [] -> false
              | (Op o, _) :: _ ->
                  let top_is_not_left_paren = fst new_token <> LParen in
                  let top_precedence = operator_precedence (Op o) in
                  let new_precedence = operator_precedence (fst new_token) in
                  let new_token_is_left_associative =
                    token_is_left_associative (fst new_token)
                  in
                  top_is_not_left_paren
                  && (top_precedence > new_precedence
                     || top_precedence = new_precedence
                        && new_token_is_left_associative)
              | _ -> false
            in
            let rec pop_until_top_has_greater_precedence (new_token : token_loc)
                (operator_stack : token_loc list)
                (output : postfix_expr_token_loc list) :
                token_loc list * postfix_expr_token_loc list =
              (* let  () = Printf.printf "pop_until_top_has_greater_precedence: new_token: %s, operator_stack: <<%s>>, output: %s\n" (token_loc_repr (new_token, 0)) (operator_stack |> List.map token_to_string |> String.concat "' '") (output |> postfix_expr_token_list_to_string) *)
              (* in *)
              match operator_stack with
              | [] -> (operator_stack, output)
              | (Op o, l) :: rest ->
                  if
                    top_of_stack_has_greater_precedence new_token operator_stack
                  then
                    pop_until_top_has_greater_precedence new_token rest
                      ((Op o, l) :: output)
                  else (operator_stack, output)
              | (LParen, _) :: _ ->
                  (* Top is a left paren, so we're done -- just return the stack and output *)
                  (operator_stack, output)
              | _ ->
                  failwith
                    "Fatal: top_of_stack_has_greater_precedence called on \
                     non-operator"
            in
            (* loops here to pop operators off stack *)
            let new_operator_stack, new_output =
              pop_until_top_has_greater_precedence (Op new_o, op_loc)
                operator_stack output
            in
            (* now that we've done that, push o1 onto operator stack *)
            parse_expr_aux rest new_output
              ((Op new_o, op_loc) :: new_operator_stack))
  in
  parse_expr_aux tokens [] []
