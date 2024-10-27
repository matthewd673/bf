open Instr

let nop_run = (' ', 0)

let instr_of_run = function
  | (_, 0) -> Nop
  | ('+', amt) -> Inc amt
  | ('-', amt) -> Dec amt
  | ('<', amt) -> Left amt
  | ('>', amt) -> Right amt
  | (',', 1) -> Input
  | ('.', 1) -> Output
  | _ -> raise (Failure "Invalid run")
;;

let ret_acc l =
  List.filter (fun x -> x <> Nop) l
  |> List.rev
;;

let parse_chars chars =
  let rec aux acc run loop = function
    | [] -> ret_acc ((instr_of_run run) :: acc), []
    | ('+' | '-' | '<' | '>' as h) :: t -> begin
      let (c, ct) = run in
      if h = c
        then aux acc (c, ct + 1) loop t
        else aux ((instr_of_run run) :: acc) (h, 1) loop t
    end
    | '[' :: t ->
      let (body, after) = aux [] nop_run true t in
      aux (Loop body :: (instr_of_run run) :: acc) nop_run loop after
    | ']' :: t ->
      if loop
        then ret_acc ((instr_of_run run) :: acc), t
        else raise (Failure "Unmatched ']'")
    | (',' | '.' as h) :: t -> aux ((instr_of_run run) :: acc) (h, 1) loop t
    | _ :: t -> aux acc run loop t (* Ignore all other characters *)
  in
  (* Start with a NOP to avoid using an option type *)
  let (instrs, _) = aux [] nop_run false chars in
  instrs
;;
