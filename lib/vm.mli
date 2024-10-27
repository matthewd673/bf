type t = {
  mem : int Array.t;
  mutable h : int;
}

val make : int -> t

val run : t -> Instr.t list -> unit
