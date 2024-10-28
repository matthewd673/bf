open Instr

type t = {
  mem : int Array.t;
  mutable h : int;
}

let rec clamp = function
  | x when x > 255 -> clamp (x - 255)
  | x when x < 0 -> clamp (x + 256)
  | x -> x
;;

let make size =
  { mem = Array.make size 0; h = 0; }
;;

let read_mem vm =
  try
    Array.get vm.mem vm.h
  with Invalid_argument _ ->
    raise (Failure (Printf.sprintf "%d is out of bounds" vm.h))
;;

let write_mem vm value =
  try
    (* Array.set vm.mem vm.h (clamp value) *)
    Array.set vm.mem vm.h value
  with Invalid_argument _ ->
    raise (Failure (Printf.sprintf "%d is out of bounds" vm.h))
;;

let move_h vm amt =
  vm.h <- vm.h + amt
;;

let run vm =
  let rec aux vm = function
  | Inc amt :: t ->
      let value = read_mem vm in
      write_mem vm (value + amt);
      aux vm t
  | Dec amt :: t ->
      let value = read_mem vm in
      write_mem vm (value - amt);
      aux vm t
  | Left amt :: t ->
      move_h vm (-amt);
      aux vm t
  | Right amt :: t ->
      move_h vm amt;
      aux vm t
  | Input :: t ->
      Printf.printf "\n>> %!";
      begin
        match In_channel.input_line In_channel.stdin with
        | Some str when String.length str > 0 ->
            write_mem vm (Char.code (String.get str 0));
        | _ -> (); (* No input has no effect *)
      end;
      aux vm t
  | Output :: t ->
      Printf.printf "%c%!" (Char.chr (clamp (read_mem vm)));
      aux vm t
  | (Loop body :: t) as l ->
      if read_mem vm <> 0
        then begin aux vm body; aux vm l end
        else aux vm t
  | Nop :: t -> aux vm t (* Should have been filtered by Parser *)
  | [] -> ()
  in
  aux vm
;;
