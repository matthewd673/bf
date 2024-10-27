open Instr

type t = {
  mem : int Array.t;
  mutable h : int;
}

let clamp x y =
  let p = x mod y in
  if p >= 0 then x else p + y - 1
;;

let make size =
  { mem = Array.make size 0; h = 0; }
;;

let read_mem vm =
  Array.get vm.mem vm.h
;;

let write_mem vm value =
  Array.set vm.mem vm.h (clamp value 256)
;;

let move_h vm amt =
  vm.h <- vm.h + amt
;;

let run vm =
  let rec aux vm = function
  | Inc amt :: t ->
      let value = read_mem vm in
      write_mem vm (value + amt);
      Printf.printf "%d : [%d]\n" vm.h (read_mem vm);
      aux vm t
  | Dec amt :: t ->
      let value = read_mem vm in
      write_mem vm (value - amt);
      Printf.printf "%d : [%d]\n" vm.h (read_mem vm);
      aux vm t
  | Left amt :: t ->
      move_h vm (-amt);
      Printf.printf "<- %d\n" vm.h;
      aux vm t
  | Right amt :: t ->
      move_h vm amt;
      Printf.printf "-> %d\n" vm.h;
      aux vm t
  | Input :: t ->
      Printf.printf "input: %!";
      begin
        match In_channel.input_line In_channel.stdin with
        | Some str when String.length str > 0 ->
            write_mem vm (Char.code (String.get str 0));
        | _ -> (); (* TODO: invalid input *)
      end;
      aux vm t
  | Output :: t ->
      Printf.printf "output: %c\n" (Char.chr (read_mem vm));
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
