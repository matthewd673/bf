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

let to_string = function
  | Inc -> "inc"
  | Dec -> "dec"
  | Left -> "left"
  | Right -> "right"
  | JOpen -> "jopen"
  | JClose -> "jclose"
  | Input -> "input"
  | Output -> "output"
;;
