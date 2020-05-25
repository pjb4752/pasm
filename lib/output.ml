open Printf

type t = string list

let empty = []

let buffer_int_literal output value =
  (sprintf "%08x" value) :: output

let buffer_string_literal output value =
  value :: output

let buffer_instruction output opcode args =
  let args = match args with
  | [] -> (0, 0)
  | x :: [] -> (x, 0)
  | x :: y :: [] -> (x, y)
  | _ -> assert false in
  sprintf "%02x%08x%08x" opcode (fst args) (snd args) :: output

let to_string output =
  String.concat "\n" @@ List.rev output

let flush output channel =
  List.iter (fun l -> output_string channel @@ sprintf "%s\n" l) @@ List.rev output
