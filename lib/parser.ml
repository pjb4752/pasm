open Printf
open Thwack.Extensions
open Thwack.Result
open Thwack.Result.Syntax

module Statement = struct
  module Literal = struct
    type t =
      | Bool of bool
      | Int of int
      | String of string
  end

  module Argument = struct
    type t =
      | Register of int
      | Variable of string
      | Label of string
      | Literal of Literal.t
  end

  type t =
    | Empty
    | Definition of string * Literal.t
    | Instruction of string * Argument.t list
end

module Error = struct
  let make_register_error line =
    sprintf "on line %d: registers must be integer values" line

  let make_literal_error line =
    sprintf "on line %d: literals must be boolean, numeric, or strings" line

  let make_definition_error line =
    sprintf "on line %d: variable definitions must match '@[NAME] [VALUE]'" line

  let make_label_error line =
    sprintf "on line %d: labels must be alphanumeric characters only" line

  let make_instruction_error line =
    sprintf "on line %d: instructions must match [INSTR] <ARG1> ... <, ARGN>" line

  let make_argument_error line =
    sprintf "on line %d: instruction arguments must match %%reg, @var, :lab or literal" line

  let attach_metadata line msg =
    sprintf "on line %d: %s" line msg
end

let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let lower_case = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l';
  'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let upper_case = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
  'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']
let characters = digits @ lower_case @ upper_case @ ['_']

let is_alpha c =
  List.exists ((=) c) characters

let tokenize expression =
  List.filter_not String.is_blank @@ String.split_on_char ' ' expression

let is_numeric str =
  let result = ref true in
  let is_digit c = List.exists ((=) c) digits in
  let () = String.iter (fun c -> if not (is_digit c) then result := false) str
  in !result

let is_symbol str =
  String.length str > 0 && List.for_all is_alpha @@ String.to_chars str

let is_quoted str =
  let length = String.length str in
  length > 2 && String.get str 0 = '"' && String.get str (length - 1) = '"'

let parse_literal line = function
  | "true" -> Ok (Statement.Literal.Bool true)
  | "false" -> Ok (Statement.Literal.Bool false)
  | lit when is_numeric lit -> Ok (Statement.Literal.Int (int_of_string lit))
  | lit when is_quoted lit -> Ok (Statement.Literal.String (String.sub lit 1 @@ String.length lit - 2))
  | _ -> Error (Error.make_literal_error line)

let parse_definition symbol_table line expression =
  match tokenize expression with
  | name :: value :: [] -> begin
    let* literal = parse_literal line value in
    let error_fn = Error.attach_metadata line in
    let* symbol_table = Symbol_table.define_variable symbol_table name line error_fn in
    return (symbol_table, line + 1, Statement.Definition (name, literal))
  end
  | _ -> Error (Error.make_definition_error line)

let parse_label symbol_table line expression =
  match tokenize expression with
  | name :: [] when is_symbol name -> begin
    let error_fn = Error.attach_metadata line in
    let* symbol_table = Symbol_table.define_label symbol_table name line error_fn in
    Ok (symbol_table, line, Statement.Empty)
  end
  | _ -> Error (Error.make_label_error line)

let parse_argument line arg =
  match String.get arg 0 with
  | '%' -> begin
    let register = String.sub_from arg 1 in
    if is_numeric register then Ok (Statement.Argument.Register (int_of_string register))
    else Error (Error.make_register_error line)
  end
  | '@' -> Ok (Statement.Argument.Variable (String.sub_from arg 1))
  | ':' -> Ok (Statement.Argument.Label (String.sub_from arg 1))
  | _ -> begin
    match parse_literal line arg with
    | Ok result -> Ok (Statement.Argument.Literal result)
    | Error _ -> Error (Error.make_argument_error line)
  end

let parse_instruction symbol_table line expression =
  match tokenize expression with
  | name :: args when is_symbol name -> begin
    let* parsed_args = List.fold_left (fun parsed arg ->
      let* parsed = parsed in
      let* next = parse_argument line arg in
      return (next :: parsed)
    ) (Ok []) args in
    return (symbol_table, line + 1, Statement.Instruction (name, List.rev parsed_args))
  end
  | _ -> Error (Error.make_instruction_error line)

let parse_nonempty symbol_table line expression =
  match String.get expression 0 with
  | '@' -> parse_definition symbol_table line @@ String.sub_from expression 1
  | ':' -> parse_label symbol_table line @@ String.sub_from expression 1
  | _ -> parse_instruction symbol_table line expression

let parse symbol_table line expression =
  let expression = String.trim expression in
  if expression = "" then Ok (symbol_table, line, Statement.Empty)
  else parse_nonempty symbol_table line expression
