open Calc_ans

let args () : string option =
  if Array.length Sys.argv > 1 then
    Some (Sys.argv |> Array.to_list |> List.tl |> String.concat " ")
  else None

let read_line_opt () : string option =
  try Some (read_line ()) with End_of_file -> None

let repl () : unit =
  let rec loop () =
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
            loop ()
        | Ok tokens ->
            print_endline (token_loc_list_repr tokens);
            loop ())
  in
  loop ()

let () =
  match args () with
  | None -> repl ()
  | Some token_input -> (
      let tokens_res = tokenize token_input in
      match tokens_res with
      | Error e -> print_endline (error_loc_with_pointer e token_input)
      | Ok tokens ->
          print_endline (token_input ^ " = " ^ token_loc_list_repr tokens))
