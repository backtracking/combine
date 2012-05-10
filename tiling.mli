
type piece

val create_piece : ?q:int -> ?e:bool -> ?n:string -> bool array array -> piece

type problem

val create_problem : ?n:string -> bool array array -> piece list -> problem

val emc: problem -> bool array array


