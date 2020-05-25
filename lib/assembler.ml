open Printf
open Thwack.Result
open Thwack.Result.Syntax

module Instruction = struct
  module Argument = struct
    type t =
      | Register
      | Literal
  end

  type arguments_t = Argument.t list list

  type t = {
    opcode: int;
    arguments: arguments_t
  }

  let make opcode arguments=
    { opcode; arguments }
end

module InstrSpec = Map.Make(String)

let instr_specs = InstrSpec.empty |>
  (InstrSpec.add "halt" @@ Instruction.make 0x00 [[]; []]) |>
  (InstrSpec.add "load" @@ Instruction.make 0x01 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "stor" @@ Instruction.make 0x02 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "allc" @@ Instruction.make 0x03 [[Register];          []])                  |>
  (InstrSpec.add "iadd" @@ Instruction.make 0x04 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "isub" @@ Instruction.make 0x05 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "imul" @@ Instruction.make 0x06 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "idiv" @@ Instruction.make 0x07 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "jump" @@ Instruction.make 0x08 [[Register; Literal]; []])                  |>
  (InstrSpec.add "test" @@ Instruction.make 0x09 [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "brch" @@ Instruction.make 0x0A [[Register];          [Register; Literal]]) |>
  (InstrSpec.add "call" @@ Instruction.make 0x0B [[Register; Literal]; [Register; Literal]]) |>
  (InstrSpec.add "retn" @@ Instruction.make 0x0C [[];                  []])                  |>
  (InstrSpec.add "puts" @@ Instruction.make 0x0E [[Register];          []])

let argument_lengths_match expected actual =
  let expected_length = List.length @@ List.filter (fun l -> List.length l > 0) expected in
  let actual_length = List.length actual in
  if expected_length = actual_length then Ok ()
  else Error (sprintf "expected %d arguments, got %d" expected_length actual_length)

let process_register_argument index expected name =
  if List.exists (function Instruction.Argument.Register -> true | _ -> false) expected then Ok name
  else Error (sprintf "argument %i was register, which is not a valid argument type" index )

let process_variable_argument symbol_table index expected name =
  if List.exists (function Instruction.Argument.Literal -> true | _ -> false) expected then
    Symbol_table.lookup_variable symbol_table name (fun msg -> msg)
  else Error (sprintf "argument %i was a variable, which is not a valid argument type" index)

let process_label_argument symbol_table index expected name =
  if List.exists (function Instruction.Argument.Literal -> true | _ -> false) expected then
    Symbol_table.lookup_label symbol_table name (fun msg -> msg)
  else Error (sprintf "argument %i was a label, which is not a valid argument type" index)

let process_literal_argument index expected value =
  if List.exists (function Instruction.Argument.Literal -> true | _ -> false) expected then
    match value with
    | Parser.Statement.Literal.Bool v -> Ok (if v then 1 else 0)
    | Parser.Statement.Literal.Int v -> Ok v
    | Parser.Statement.Literal.String _ -> begin
        Error (sprintf "argument %i was a string, which is not a valid argument type" index)
    end
  else Error (sprintf "argument %i was literal, which is not a valid argument type" index )

let process_argument symbol_table index expected actual =
  let expected = List.nth expected index in
  Parser.Statement.Argument.(
    match actual with
    | Register name -> process_register_argument index expected name
    | Variable name -> process_variable_argument symbol_table index expected name
    | Label name -> process_label_argument symbol_table index expected name
    | Literal name -> process_literal_argument index expected name
  )

let process_arguments symbol_table expected actual =
  let* () = argument_lengths_match expected actual in
  let indexed = List.mapi (fun i arg -> (i, arg)) actual in
  List.fold_right (fun (i, actual) processed ->
    let* processed = processed in
    let* actual = process_argument symbol_table i expected actual in
    return (actual :: processed)
  ) indexed (Ok [])

let assemble_instruction symbol_table output name arguments =
  match InstrSpec.find_opt name instr_specs with
  | Some instr_spec -> begin
    let* arguments = process_arguments symbol_table instr_spec.arguments arguments in
    return (Output.buffer_instruction output instr_spec.opcode arguments)
  end
  | None -> Error (sprintf "%s is not a valid instruction name" name)

let assemble_definition output = function
  | Parser.Statement.Literal.Bool b -> Ok (Output.buffer_int_literal output (if b then 1 else 0))
  | Parser.Statement.Literal.Int i -> Ok (Output.buffer_int_literal output i)
  | Parser.Statement.Literal.String s -> Ok (Output.buffer_string_literal output s)

let assemble_statement symbol_table output = function
  | Parser.Statement.Instruction (name, args) -> assemble_instruction symbol_table output name args
  | Parser.Statement.Definition (_, value) -> assemble_definition output value
  | Parser.Statement.Empty -> Ok output

let assemble_statements symbol_table parsed =
  List.fold_left (fun output stmt ->
    let* output = output in
    assemble_statement symbol_table output stmt
  ) (Ok Output.empty) parsed

let parse_lines lines =
  List.fold_left (fun accumulator raw ->
    let* (symbol_table, i, parsed) = accumulator in
    let* (symbol_table, i, next) = Parser.parse symbol_table i raw in
    return (symbol_table, i, next :: parsed)
  ) (Ok (Symbol_table.empty, 1, [])) lines

let assemble lines =
  let* (symbol_table, _, parsed) = parse_lines lines in
  assemble_statements symbol_table @@ List.rev parsed
