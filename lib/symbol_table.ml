open Printf

module Symbol_table = Map.Make(String)

module Symbol = struct
  type t =
    | Label of int
    | Variable of int
end

type t = Symbol.t Symbol_table.t

type error_fn_t = string -> string

let empty = Symbol_table.empty

let define symbol_table name thunk error_fn =
  if Symbol_table.mem name symbol_table then Error (error_fn @@ sprintf "%s is already defined" name)
  else Ok (Symbol_table.add name (thunk ()) symbol_table)

let define_label symbol_table name address error_fn =
  define symbol_table name (fun () -> Symbol.Label address) error_fn

let define_variable symbol_table name address error_fn =
  define symbol_table name (fun () -> Symbol.Variable address) error_fn

let lookup_label symbol_table name error_fn =
  match Symbol_table.find_opt name symbol_table with
  | Some (Symbol.Label address) -> Ok address
  | _ -> Error (error_fn @@ sprintf "label %s is not defined" name)

let lookup_variable symbol_table name error_fn =
  match Symbol_table.find_opt name symbol_table with
  | Some (Symbol.Variable address) -> Ok address
  | _ -> Error (error_fn @@ sprintf "variable %s is not defined" name)
