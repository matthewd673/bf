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

let rec to_string = function
  | Inc amt -> Printf.sprintf "inc %d" amt
  | Dec amt -> Printf.sprintf "dec %d" amt
  | Left amt -> Printf.sprintf "left %d" amt
  | Right amt -> Printf.sprintf "right %d" amt
  | Loop body ->
      List.map to_string body
      |> String.concat "; "
      |> Printf.sprintf "loop [%s]"
  | Input -> "input"
  | Output -> "output"
  | Nop -> "nop"
;;
