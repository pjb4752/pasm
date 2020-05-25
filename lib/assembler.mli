module Instruction: sig
  module Argument: sig
    type t =
      | Register
      | Literal
  end

  type t
end

val assemble: string list -> (Output.t, string) result
