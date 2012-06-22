type error
val print_error : Format.formatter -> error -> unit
exception Error of Ast.pos * error

val interp: Ast.file -> unit
val interp_problems: Ast.file -> Tiling.problem list

