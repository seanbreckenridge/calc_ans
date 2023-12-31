open Calc_ans

type options = { debug : bool; args : string option }

let parse_args () : options =
  let debug = ref false in
  let args = ref None in
  let completion = ref false in
  let speclist =
    [
      ("-C", Arg.Set completion, "print tokens for completion");
      ("-d", Arg.Set debug, "Enable debug mode");
      ("-e", Arg.String (fun s -> args := Some s), "Input expression");
    ]
  in
  let usage_msg = "Usage: calc_ans [-d] [-C] [-e <expr>]" in
  let () = Arg.parse speclist print_endline usage_msg in
  let () =
    match !completion with
    | true ->
        let () =
          tokens_for_completion () |> String.concat "\n" |> print_endline
        in
        exit 0
    | false -> ()
  in
  { debug = !debug; args = !args }

(* Supports: *)
(**)
(*     +, -, *, /, // (floor divide) ^ (exponentiation), % (modulo), or their aliases *)
(*     ceil, floor, abs, round, sqrt *)
(*     epoch (current time in seconds since the Unix epoch), pi, rand *)
(*     ans (last answer) *)
(**)

let print_help () =
  let help_str =
    "##################################################\n\
     A CLI REPL for evaluating mathematical expressions\n\
     ##################################################\n\n\
     Examples:\n\
     > 5 + 10.5\n\
     15.5\n\
     # assumes the last answer as the left-hand side\n\
     > / 5\n\
     3.1\n\
     > ceil  # rounds up\n\
     4\n\
     > round (1 + sqrt(-4 ^ 4) * 5)\n\
     81\n\
     > (88 - ans)\n\
     7\n\n\
     If you don't provide an expression, it will assume the left hand side is \
     the last answer.\n\n\
     Anything after a '#' is a comment.\n\n\
     ##################################################\n\n\
     Supported operators:\n\n\
     +, -, *, /, // (floor divide) ^ (exponentiation), % (modulo)\n\
     ceil, floor, abs, round, sqrt\n\
     epoch (current time in seconds since the Unix epoch), pi, rand\n\
     ans (last answer)\n\n\
     Aliases:\n\n\
     + | a, p\n\
     - | m\n\
     * | t\n\
     / | d\n\n\
     E.g:\n\
     > 3 p 10 # (3 + 10)\n\
     13"
  in
  print_endline help_str

let read_line_opt () : string option =
  try Some (read_line ()) with End_of_file -> None

let debug_print (debug : bool) (s : string) =
  if debug then prerr_endline ("[DEBUG] " ^ s) else ()

let parse_ast (tokens : token_loc list) (opts : options) :
    postfix_expr_token_loc list =
  debug_print opts.debug ("Tokens: " ^ token_loc_list_repr tokens);
  let tokens_loc_postfix = tokens |> tokens_to_postfix in
  let tokens_postfix = tokens_loc_postfix |> List.map fst in
  debug_print opts.debug
    ("Postfix: " ^ postfix_expr_token_list_to_string tokens_postfix);
  tokens_loc_postfix

let start_repl (opts : options) : unit =
  let debug_func = debug_print opts.debug in
  let rec loop (opts : options) (prev_result : number option) =
    let prompt_str = "> " in
    let () = print_string prompt_str in
    let input = read_line_opt () in
    let () =
      debug_print opts.debug ("Input: " ^ Option.value input ~default:"")
    in
    let () =
      debug_print opts.debug
        ("Prev result: "
        ^
        match prev_result with
        | None -> "None"
        | Some num -> number_to_string num)
    in
    match input with
    | None -> Printf.printf "\n"
    | Some input -> (
        match input |> String.trim with
        | "q" | "quit" | "exit" -> exit 0
        | "h" | "help" ->
            let () = print_help () in
            loop opts prev_result
        | _ -> (
            let tokens_res = tokenize input in
            match tokens_res with
            | Error e ->
                print_endline (point_to_error_text e (String.length prompt_str));
                loop opts prev_result
            | Ok tokens -> (
                let ast = parse_ast tokens opts in
                let result =
                  eval_postfix_expression ast prev_result debug_func
                in
                match result with
                | Ok num ->
                    Printf.printf "%s\n" (number_to_string num);
                    loop opts (Some num)
                | Error e ->
                    print_endline
                      (point_to_error_text e (String.length prompt_str));
                    loop opts prev_result)))
  in
  loop opts None

let () =
  let opts = parse_args () in
  match opts.args with
  | None ->
      let _ = start_repl opts in
      ()
  | Some token_input -> (
      let tokens_res = tokenize token_input in
      match tokens_res with
      | Error e -> print_endline (error_loc_with_pointer e token_input)
      | Ok tokens -> (
          let ast = parse_ast tokens opts in
          let result =
            eval_postfix_expression ast None (debug_print opts.debug)
          in
          match result with
          | Ok num -> Printf.printf "%s\n" (number_to_string num)
          | Error e ->
              print_endline (error_loc_with_pointer e token_input);
              ()))
