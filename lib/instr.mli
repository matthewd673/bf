type t =
  | Inc
  | Dec
  | Left
  | Right
  | JOpen
  | JClose
  | Input
  | Output
;;

val to_string : t -> string
