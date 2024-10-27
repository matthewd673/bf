type t =
  | Inc of int
  | Dec of int
  | Left of int
  | Right of int
  | Loop of t list
  | Input
  | Output
  | Nop
;;

val to_string : t -> string
