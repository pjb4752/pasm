open Printf

let filename = ref None

let input_file_ext = ".pa"
let output_file_ext = ".pb"

let assembled_filename filename =
  let extension = Filename.extension filename in
  let root_filename filename =
    Filename.basename filename |> Filename.remove_extension
  in
  match extension with
  | ".pa" -> Ok (root_filename filename ^ output_file_ext)
  | _ -> Error ("input file does not appear to be a valid pasm file")

let read_input_file filename =
  let input = open_in filename in
  let rec read_input_lines' lines =
    try
      read_input_lines' @@ input_line input :: lines
    with End_of_file ->
      let () = close_in input in
      List.rev lines
  in
  read_input_lines' []

let write_output_file filename output =
  let out_channel = open_out filename in
  let () = Pasm.Output.flush output out_channel in
  close_out out_channel

let run filename =
  Thwack.Extensions.Result.(
    let (let*) = (>>=) in
    let* output_filename = assembled_filename filename in
    let input = read_input_file filename in
    let () = printf "assembling input file... " in
    let* output = Pasm.Assembler.assemble input in
    let () = write_output_file output_filename output in
    return (output)
  )

let main () =
  let usage = "Usage: pasm [options] filename" in
  let options = [] in
  let () = Arg.parse options (fun f -> filename := Some f) usage in
  match !filename with
  | None -> eprintf "error: no input file given\n"
  | Some filename -> begin
      match run filename  with
      | Ok _ -> printf "done\n"
      | Error e -> eprintf "\n\ncompilation failed: %s\n" e
  end

let () = main ()
