type t

val empty: t

val buffer_int_literal: t -> int -> t

val buffer_string_literal: t -> string -> t

val buffer_instruction: t -> int -> int list -> t

val to_string: t -> string

val flush: t -> out_channel -> unit
