type bin_op = Plus | Sub | Mul | Div | Mod | Pow
type func = Abs | Floor | Ceil | Round | Sqrt

(* items which take no arguments as input *)
type null_op = Epoch | Pi

let null_op_to_string (op : null_op) : string =
  match op with Epoch -> "epoch" | Pi -> "pi"

let parse_null_op (input : string) : null_op option =
  match input with "epoch" -> Some Epoch | "pi" -> Some Pi | _ -> None

let is_null_op (input : string) : bool =
  match parse_null_op input with Some _ -> true | None -> false

let binop_to_string (op : bin_op) : string =
  match op with
  | Plus -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "^"

let parse_binop_alias s =
  match s with
  | "a" -> Some Plus
  | "p" -> Some Plus
  | "m" -> Some Sub
  | "t" -> Some Mul
  | "d" -> Some Div
  | _ -> None

let is_binop_alias s =
  match parse_binop_alias s with Some _ -> true | None -> false

let func_to_string (func : func) : string =
  match func with
  | Abs -> "abs"
  | Floor -> "floor"
  | Ceil -> "ceil"
  | Round -> "round"
  | Sqrt -> "sqrt"

let parse_func (input : string) : func option =
  match input with
  | "abs" -> Some Abs
  | "floor" -> Some Floor
  | "ceil" -> Some Ceil
  | "round" -> Some Round
  | "sqrt" -> Some Sqrt
  | _ -> None

let is_func (input : string) : bool =
  match parse_func input with Some _ -> true | None -> false

type number = Int of int | Float of float

let string_to_number_unsafe (input : string) : number =
  if String.contains input '.' then Float (float_of_string input)
  else Int (int_of_string input)

let number_to_string (num : number) : string =
  match num with Int i -> string_of_int i | Float f -> Printf.sprintf "%f" f

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

type token =
  | Number of number
  | Op of bin_op
  | LParen
  | RParen
  | Ans
  | Func of func
  | NullOp of null_op

type token_loc = token * int
type error_loc = Str of string | WithLoc of string * int

let token_to_string (token : token) : string =
  match token with
  | Number n -> number_to_string n
  | Op o -> binop_to_string o
  | LParen -> "("
  | RParen -> ")"
  | Ans -> "Ans"
  | Func f -> func_to_string f
  | NullOp o -> null_op_to_string o

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
  | Func f, _ -> "<Func " ^ func_to_string f ^ ">"
  | NullOp o, _ -> "<NullOp " ^ null_op_to_string o ^ ">"

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
  let rec read_number (input : string) (cursor : int) (acc : string) : string =
    if cursor >= String.length input then acc
    else
      let c = String.get input cursor in
      match c with
      | '0' .. '9' -> read_number input (cursor + 1) (acc ^ String.make 1 c)
      | '.' -> read_number input (cursor + 1) (acc ^ String.make 1 c)
      | '-' ->
          if String.length acc = 0 then
            read_number input (cursor + 1) (acc ^ String.make 1 c)
          else acc
      | _ -> acc
  in
  let rec read_text (input : string) (cursor : int) (acc : string) : string =
    if cursor >= String.length input then acc
    else
      let c = String.get input cursor in
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' ->
          read_text input (cursor + 1) (acc ^ String.make 1 c)
      | _ -> acc
  in
  let peek_num_exists (input : string) (cursor : int) : bool =
    if cursor >= String.length input then false
    else read_number input cursor "" |> String.length > 0
  in
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
    | '-' when not (peek_num_exists input (cursor + 1)) ->
        tokenize_aux input (cursor + 1) ((Op Sub, cursor) :: acc) depth
    | '*' -> tokenize_aux input (cursor + 1) ((Op Mul, cursor) :: acc) depth
    | '/' -> tokenize_aux input (cursor + 1) ((Op Div, cursor) :: acc) depth
    | '%' -> tokenize_aux input (cursor + 1) ((Op Mod, cursor) :: acc) depth
    | '^' -> tokenize_aux input (cursor + 1) ((Op Pow, cursor) :: acc) depth
    | '(' ->
        tokenize_aux input (cursor + 1) ((LParen, cursor) :: acc) (depth + 1)
    | ')' ->
        tokenize_aux input (cursor + 1) ((RParen, cursor) :: acc) (depth - 1)
    | _ -> (
        let num_str = read_number input cursor "" in
        let text_str = read_text input cursor "" |> String.lowercase_ascii in
        (* if nothing could be read here, then we couldn't read an int or float, so its an unknown character *)
        match (num_str, text_str) with
        | _, "ans" ->
            tokenize_aux input
              (cursor + String.length text_str)
              ((Ans, cursor) :: acc) depth
        | _, func_str when is_func func_str ->
            tokenize_aux input
              (cursor + String.length text_str)
              ((Func (parse_func func_str |> Option.get), cursor) :: acc)
              depth
        | _, null_op_str when is_null_op null_op_str ->
            tokenize_aux input
              (cursor + String.length text_str)
              ((NullOp (parse_null_op null_op_str |> Option.get), cursor) :: acc)
              depth
        | _, op_str when is_binop_alias op_str ->
            tokenize_aux input
              (cursor + String.length text_str)
              ((Op (parse_binop_alias op_str |> Option.get), cursor) :: acc)
              depth
        | num_str, _ when String.length num_str > 0 ->
            tokenize_aux input
              (cursor + String.length num_str)
              ((Number (string_to_number_unsafe num_str), cursor) :: acc)
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

(*
Infix -> Postfix using the Shunting-yard algorithm
shunting-yard algorithm https://en.wikipedia.org/wiki/Shunting_yard_algorithm

while there are tokens to be read:
    read a token
    if the token is:
    - a number:
        put it into the output queue
    - a function:
        push it onto the operator stack
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

type postfix_expr_token =
  | Val of number
  | Op of bin_op
  | Ans
  | Func of func
  | NullOp of null_op

type postfix_expr_token_loc = postfix_expr_token * int

let postfix_expr_token_to_string (token : postfix_expr_token) : string =
  match token with
  | Val n -> number_to_string n
  | Op o -> binop_to_string o
  | Ans -> "Ans"
  | Func f -> func_to_string f
  | NullOp o -> null_op_to_string o

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
          | (Func f, l) :: rest -> pop_operators rest ((Func f, l) :: output)
          | (NullOp _, _) :: _ ->
              failwith "Fatal: null op should not be on operator stack"
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
    | (NullOp o, l) :: rest ->
        parse_expr_aux rest ((NullOp o, l) :: output) operator_stack
    | (LParen, l) :: rest ->
        parse_expr_aux rest output ((LParen, l) :: operator_stack)
    | (Ans, l) :: rest ->
        parse_expr_aux rest ((Ans, l) :: output) operator_stack
    | (Func f, l) :: rest ->
        parse_expr_aux rest output ((Func f, l) :: operator_stack)
    | (RParen, _) :: rest ->
        (*
        while the operator at the top of the operator stack is not a left parenthesis:
            {assert the operator stack is not empty}
            /* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
            pop the operator from the operator stack into the output queue
        {assert there is a left parenthesis at the top of the operator stack}
        pop the left parenthesis from the operator stack and discard it
        if there is a function token at the top of the operator stack, then:
            pop the function from the operator stack into the output queue *)
        let rec pop_until_left_paren (operator_stack : token_loc list)
            (output : postfix_expr_token_loc list) :
            token_loc list * postfix_expr_token_loc list =
          match operator_stack with
          | [] -> failwith "Fatal: mismatched parentheses"
          | (LParen, _) :: rest -> (rest, output)
          | (Op o, l) :: rest -> pop_until_left_paren rest ((Op o, l) :: output)
          | (Ans, l) :: rest -> pop_until_left_paren rest ((Ans, l) :: output)
          | (Func f, l) :: rest ->
              pop_until_left_paren rest ((Func f, l) :: output)
          | (NullOp _, _) :: _ ->
              failwith "Fatal: null op should not be on operator stack"
          | (Number _, _) :: _ ->
              failwith "Fatal: number should not be on operator stack"
          | (RParen, _) :: _ ->
              failwith
                "Fatal: right parenthesis should not be on operator stack"
        in
        let rec pop_functions (operator_stack : token_loc list)
            (output : postfix_expr_token_loc list) :
            token_loc list * postfix_expr_token_loc list =
          match operator_stack with
          | [] -> (operator_stack, output)
          | (Func f, l) :: rest -> pop_functions rest ((Func f, l) :: output)
          | _ -> (operator_stack, output)
        in
        let new_operator_stack, new_output =
          pop_until_left_paren operator_stack output
        in
        let new_operator_stack, new_output =
          pop_functions new_operator_stack new_output
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
              | (Func _, _) :: _ ->
                  (* Top is a function, which has higher precedence than any operator, so we're done -- just return the stack and output *)
                  (operator_stack, output)
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

let eval_binary_operation (op : bin_op) (i1 : number) (i2 : number) :
    (number, string) result =
  match (op, i1, i2) with
  | Plus, Int i1, Int i2 -> Ok (Int (i1 + i2))
  | Plus, Int i1, Float f2 -> Ok (Float (float_of_int i1 +. f2))
  | Plus, Float f1, Int i2 -> Ok (Float (f1 +. float_of_int i2))
  | Plus, Float f1, Float f2 -> Ok (Float (f1 +. f2))
  | Sub, Int i1, Int i2 -> Ok (Int (i1 - i2))
  | Sub, Int i1, Float f2 -> Ok (Float (float_of_int i1 -. f2))
  | Sub, Float f1, Int i2 -> Ok (Float (f1 -. float_of_int i2))
  | Sub, Float f1, Float f2 -> Ok (Float (f1 -. f2))
  | Mul, Int i1, Int i2 -> Ok (Int (i1 * i2))
  | Mul, Int i1, Float f2 -> Ok (Float (float_of_int i1 *. f2))
  | Mul, Float f1, Int i2 -> Ok (Float (f1 *. float_of_int i2))
  | Mul, Float f1, Float f2 -> Ok (Float (f1 *. f2))
  | Div, i1, i2 -> divide_smart i1 i2
  | Mod, Int i1, Int i2 -> Ok (Int (i1 mod i2))
  | Mod, Int i1, Float f2 -> Ok (Float (mod_float (float_of_int i1) f2))
  | Mod, Float f1, Int i2 -> Ok (Float (mod_float f1 (float_of_int i2)))
  | Mod, Float f1, Float f2 -> Ok (Float (mod_float f1 f2))
  | Pow, Int i1, Int i2 -> Ok (Float (float_of_int i1 ** float_of_int i2))
  | Pow, Int i1, Float f2 -> Ok (Float (float_of_int i1 ** f2))
  | Pow, Float f1, Int i2 -> Ok (Float (f1 ** float_of_int i2))
  | Pow, Float f1, Float f2 -> Ok (Float (f1 ** f2))

let round_float (f : float) : float =
  let frac_part = f -. floor f in
  if f < 0.0 then if frac_part > 0.5 then ceil f else floor f
  else if frac_part >= 0.5 then ceil f
  else floor f

let eval_func (f : func) (i1 : number) : (number, string) result =
  match (f, i1) with
  | Abs, Int i1 -> Ok (Int (abs i1))
  | Abs, Float f1 -> Ok (Float (abs_float f1))
  | Floor, Int i1 -> Ok (Int i1)
  | Floor, Float f1 -> Ok (Int (floor f1 |> int_of_float))
  | Ceil, Int i1 -> Ok (Int i1)
  | Ceil, Float f1 -> Ok (Int (ceil f1 |> int_of_float))
  | Round, Int i1 -> Ok (Int i1)
  | Round, Float f1 -> Ok (Int (round_float f1 |> int_of_float))
  | Sqrt, n1 -> (
      let is_negative =
        match n1 with Int i1 -> i1 < 0 | Float f1 -> f1 < 0.0
      in
      if is_negative then Error "Cannot take square root of negative number"
      else
        match n1 with
        | Int i1 -> Ok (Float (sqrt (float_of_int i1)))
        | Float f1 -> Ok (Float (sqrt f1)))

let eval_null_op (op : null_op) : number =
  match op with
  | Epoch -> Int (int_of_float (Unix.time ()))
  | Pi -> Float Float.pi

let eval_null_expressions (ast : postfix_expr_token_loc list) :
    postfix_expr_token_loc list =
  ast
  |> List.map (fun (token, loc) ->
         match token with
         | NullOp o -> (Val (eval_null_op o), loc)
         | other -> (other, loc))

let eval_postfix_expression (ast : postfix_expr_token_loc list)
    (prev_ans : number option) (debug_func : string -> unit) :
    (number, error_loc) result =
  let rec eval_postfix_aux (ast : postfix_expr_token_loc list)
      (stack : (number * int) list) : (number, error_loc) result =
    match ast with
    | [] -> (
        match (stack, prev_ans) with
        | [], None -> Error (Str "Empty expression")
        | [], Some ans -> Ok ans
        | (v, _) :: [], _ -> Ok v
        | _ ->
            Error
              (Str
                 (Printf.sprintf
                    "Malformed expression, multiple values without operator or \
                     function. Stack contents: %s"
                    (stack |> List.rev
                    |> List.map (fun (n, _) -> number_to_string n)
                    |> String.concat " "))))
    | (Ans, l) :: rest -> (
        match prev_ans with
        | None -> Error (WithLoc ("No previous answer", l))
        | Some ans -> eval_postfix_aux rest ((ans, l) :: stack))
    | (Val n, l) :: rest -> eval_postfix_aux rest ((n, l) :: stack)
    | (Op o, l) :: rest -> (
        let args, new_stack =
          match (stack, prev_ans) with
          | [], _ -> (Error (WithLoc ("Need two operands for operator", l)), [])
          | (_, _) :: [], None ->
              (Error (WithLoc ("Need two operands for operator", l)), [])
          | (i2, _) :: [], Some i1 -> (Ok (i1, i2), [])
          | (i2, _) :: (i1, _) :: rest_stack, _ -> (Ok (i1, i2), rest_stack)
        in
        match args with
        | Error e -> Error e
        | Ok (i1, i2) -> (
            let () =
              debug_func
                (Printf.sprintf "Evaluating %s %s %s" (number_to_string i1)
                   (binop_to_string o) (number_to_string i2))
            in
            match eval_binary_operation o i1 i2 with
            | Ok result -> eval_postfix_aux rest ((result, l) :: new_stack)
            | Error e -> Error (WithLoc (e, l))))
    | (Func f, l) :: rest -> (
        let num, new_stack =
          match (stack, prev_ans) with
          | [], None ->
              (Error (WithLoc ("Need one operand for function", l)), [])
          | [], Some i1 -> (Ok i1, [])
          | (i1, _) :: rest_stack, _ -> (Ok i1, rest_stack)
        in
        match num with
        | Error e -> Error e
        | Ok i1 -> (
            let () =
              debug_func
                (Printf.sprintf "Evaluating %s %s" (func_to_string f)
                   (number_to_string i1))
            in
            match eval_func f i1 with
            | Ok result -> eval_postfix_aux rest ((result, l) :: new_stack)
            | Error e -> Error (WithLoc (e, l))))
    | (NullOp _, _) :: _ ->
        failwith "Fatal: null op should not in eval stage in postfix expression"
  in
  eval_postfix_aux (eval_null_expressions ast) []

let tokens_for_completion () =
  [ "abs"; "floor"; "ceil"; "round"; "sqrt"; "epoch"; "pi" ]
