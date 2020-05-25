type t

type error_fn_t = string -> string

val empty: t

val define_label: t -> string -> int -> error_fn_t -> (t, string) result

val define_variable: t -> string -> int -> error_fn_t -> (t, string) result

val lookup_label: t -> string -> error_fn_t -> (int, string) result

val lookup_variable: t -> string -> error_fn_t -> (int, string) result
