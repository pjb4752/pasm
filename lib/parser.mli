module Statement: sig
  module Literal: sig
    type t =
      | Bool of bool
      | Int of int
      | String of string
  end

  module Argument: sig
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

val parse: Symbol_table.t -> int -> string -> ((Symbol_table.t * int * Statement.t), string) result
