open Calc_ans

type options = { debug : bool; args : string option }

let parse_args () : options =
  let debug = ref false in
  let args = ref None in
  let speclist =
    [
      ("-d", Arg.Set debug, "Enable debug mode");
      ("-e", Arg.String (fun s -> args := Some s), "Input expression");
    ]
  in
  let usage_msg = "Usage: calc_ans [-d] [-e <expr>]" in
  let () = Arg.parse speclist print_endline usage_msg in
  { debug = !debug; args = !args }

let read_line_opt () : string option =
  try Some (read_line ()) with End_of_file -> None

let debug_print (debug : bool) (s : string) =
  if debug then prerr_endline ("[DEBUG] " ^ s) else ()

let parse_ast (tokens : token_loc list) (opts : options) :
    postfix_expr_token list =
  debug_print opts.debug (token_loc_list_repr tokens);
  let tokens_loc_postfix = tokens |> tokens_to_postfix in
  let tokens_postfix = tokens_loc_postfix |> List.map fst in
  debug_print opts.debug (postfix_expr_token_list_to_string tokens_postfix);
  tokens_postfix

let repl (opts : options) =
  let rec loop (opts : options) =
    let prompt_str = "> " in
    let () = print_string prompt_str in
    let input = read_line_opt () in
    match input with
    | None -> Printf.printf "\n"
    | Some input -> (
        let tokens_res = tokenize input in
        match tokens_res with
        | Error e ->
            print_endline (point_to_error_text e (String.length prompt_str));
            loop opts
        | Ok tokens ->
            let _ = parse_ast tokens opts in
            loop opts)
  in
  loop opts

let () =
  let args = parse_args () in
  match args.args with
  | None -> repl args
  | Some token_input -> (
      let tokens_res = tokenize token_input in
      match tokens_res with
      | Error e -> print_endline (error_loc_with_pointer e token_input)
      | Ok tokens ->
          let _ = parse_ast tokens args in
          ())
