let load_file_into_chars filename =
  let rec aux acc f =
    match In_channel.input_char f with
    | None -> List.rev acc
    | Some c -> aux (c :: acc) f
  in
  let f = In_channel.open_bin filename in
  aux [] f
;;

let () =
  print_endline "bf";

  if Array.length Sys.argv < 2 then begin
    print_endline "args: <filename>";
    exit 1;
  end;

  try
    let contents = load_file_into_chars (Array.get Sys.argv 1) in
    Printf.printf "Chars: %d\n" (List.length contents);
  with Sys_error msg -> begin
    Printf.printf "Failed to load file: %s\n" msg;
    exit 1;
  end
;;
