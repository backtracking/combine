
{
  open Format
  open Lexing

  type grid = char * bool array array

  let bool_array_of_string m s =
    let a = Array.create m false in
    for i = 0 to min (m - 1) (String.length s - 1) do
      if s.[i] <> ' ' then a.(i) <- true
    done;
    a

}

let space = [' ' '\t']
let comment = '#' [^ '\n']* '\n'

rule read = parse
  | comment
      { read lexbuf }
  | '\n'
      { read lexbuf }
  | (_ as c) ('*'+ as s) space* '\n'
      { let m = String.length s in
	let lines = read_lines m lexbuf in
	let p = c, Array.of_list lines in
	p :: read lexbuf }
  | _ as c
      { eprintf "parse error: invalid character `%c'@." c; exit 1 }
  | eof
      { [] }

and read_lines m = parse
  | space* '\n' | eof
      { [] }
  | comment
      { read_lines m lexbuf }
  | '*' ([^ '\n']* as s) '\n'
      { let line = bool_array_of_string m s in
	line :: read_lines m lexbuf }
  | _ as c
      { eprintf "parse error: invalid character `%c'@." c; exit 1 }

{

  let read_channel c =
    let lb = from_channel c in
    read lb

}
