
type grid = char * bool array array

val raw_parser: in_channel -> grid list

val read_problem: in_channel -> Tiling.problem
