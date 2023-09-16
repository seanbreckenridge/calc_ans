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
type expr = Val of number | BinOp of bin_op * expr * expr

let number_to_string (num : number) : string =
  match num with Int i -> string_of_int i | Float f -> string_of_float f

(* Define a function to print expressions *)
let rec expr_to_string (expr : expr) : string =
  match expr with
  | Val (Int i) -> string_of_int i
  | Val (Float f) -> string_of_float f
  | BinOp (o, e1, e2) ->
      expr_to_string e1 ^ " " ^ binop_to_string o ^ " " ^ expr_to_string e2

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

let rec eval (expr : expr) : (number, string) result =
  match expr with
  | Val i -> Ok i
  | BinOp (o, e1, e2) -> (
      match (e1, e2) with
      | Val (Int i1), Val (Int i2) -> (
          match o with
          | Plus -> Ok (Int (i1 + i2))
          | Sub -> Ok (Int (i1 - i2))
          | Mul -> Ok (Int (i1 * i2))
          | Div -> divide_smart (Int i1) (Int i2)
          | Pow -> Ok (Int (int_of_float (float_of_int i1 ** float_of_int i2)))
          | Mod -> Ok (Int (i1 mod i2)))
      | Val (Float f1), Val (Float f2) -> (
          match o with
          | Plus -> Ok (Float (f1 +. f2))
          | Sub -> Ok (Float (f1 -. f2))
          | Mul -> Ok (Float (f1 *. f2))
          | Div -> divide_smart (Float f1) (Float f2)
          | Pow -> Ok (Float (f1 ** f2))
          | Mod -> Ok (Float (mod_float f1 f2)))
      (* Otherwise, convert the ints to floats *)
      | Val (Int i1), Val (Float f2) ->
          (* Convert i1 to float *)
          eval (BinOp (o, Val (Float (float_of_int i1)), Val (Float f2)))
      | Val (Float f1), Val (Int i2) ->
          (* Convert i2 to float *)
          eval (BinOp (o, Val (Float f1), Val (Float (float_of_int i2))))
      (* recurse till we get to a value, patterns above will handle the rest *)
      | i1, i2 -> (
          match eval i1 with
          | Error e -> Error e
          | Ok i1 -> (
              match eval i2 with
              | Error e -> Error e
              | Ok i2 -> eval (BinOp (o, Val i1, Val i2)))))

type token = Number of number | Op of bin_op | LParen | RParen | Ans
type token_loc = token * int
type token_list = token_loc list
type error_loc = Str of string | WithLoc of string * int

let error_loc_with_pointer (error_loc : error_loc) (input : string) : string =
  match error_loc with
  | Str e -> e
  | WithLoc (e, loc) ->
      let pointer = String.make loc ' ' ^ "^" in
      input ^ "\n" ^ pointer ^ "\n" ^ e

let token_loc_repr (token_loc : token_loc) : string =
  match token_loc with
  | Number n, _ -> "<Num " ^ number_to_string n ^ ">"
  | Op o, _ -> "<BinOp " ^ binop_to_string o ^ ">"
  | Ans, _ -> "<Ans>"
  | LParen, _ -> "<Lparen>"
  | RParen, _ -> "<Rparen>"

let token_loc_list_repr (token_loc_list : token_loc list) : string =
  token_loc_list |> List.map token_loc_repr |> String.concat " "

let rec tokenize_aux (input : string) (cursor : int) (acc : token_list)
    (depth : int) : (token_loc list, error_loc) result =
  if cursor >= String.length input then
    match depth with
    | 0 -> Ok (acc |> List.rev)
    | _ -> Error (Str "Unbalanced parentheses")
  else
    let c = String.get input cursor in
    match c with
    | ' ' -> tokenize_aux input (cursor + 1) acc depth
    | '+' -> tokenize_aux input (cursor + 1) ((Op Plus, cursor) :: acc) depth
    | '-' -> tokenize_aux input (cursor + 1) ((Op Sub, cursor) :: acc) depth
    | '*' -> tokenize_aux input (cursor + 1) ((Op Mul, cursor) :: acc) depth
    | '/' -> tokenize_aux input (cursor + 1) ((Op Div, cursor) :: acc) depth
    | '%' -> tokenize_aux input (cursor + 1) ((Op Mod, cursor) :: acc) depth
    | '^' -> tokenize_aux input (cursor + 1) ((Op Pow, cursor) :: acc) depth
    | 'a' -> tokenize_aux input (cursor + 1) ((Ans, cursor) :: acc) depth
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
        let num_str = read_number input cursor "" in
        (* if nothing could be read here, then we couldn't read an int or float, so its an unknown character *)
        match num_str |> String.length with
        | 0 ->
            Error
              (WithLoc ("Unknown character '" ^ String.make 1 c ^ "'", cursor))
        | _ ->
            let num =
              if String.contains num_str '.' then
                Float (float_of_string num_str)
              else Int (int_of_string num_str)
            in
            tokenize_aux input
              (cursor + String.length num_str)
              ((Number num, cursor) :: acc)
              depth)

let tokenize (input : string) = tokenize_aux input 0 [] 0
