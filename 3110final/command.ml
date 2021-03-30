type coords = (int * int) list

type command =
  | Move of coords
  | Quit

exception Empty

exception Malformed

let parse str = failwith "unimplemented"
