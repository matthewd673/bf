open Instr

let parse_chars chars =
  let rec aux acc = function
    | [] -> List.rev acc
    | '+' :: t -> aux (Inc :: acc) t
    | '-' :: t -> aux (Dec :: acc) t
    | '<' :: t -> aux (Left :: acc) t
    | '>' :: t -> aux (Right :: acc) t
    | '[' :: t -> aux (JOpen :: acc) t
    | ']' :: t -> aux (JClose :: acc) t
    | ',' :: t -> aux (Input :: acc) t
    | '.' :: t -> aux (Output :: acc) t
    | _ :: t -> aux acc t
  in
  aux [] chars
;;
