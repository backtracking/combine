
(* Given a Pythagorean triple (a,b,c), with a < b < c,
   tile a c*c square using the hooks (L shapes) from the two
   squares a*a and b*b *)

open Format

(*
   primitive Pythagorean triples are generated using
     a = 2mn
     b = (m^2 - n^2)
     c = (m^2 + n^2)
*)

let n = int_of_string Sys.argv.(1)
let m = int_of_string Sys.argv.(2)

let rec gcd n m = if m = 0 then n else gcd m (n mod m)
let gcd n m = if n > m then gcd n m else gcd m n

let () = assert (n < m)
let () = assert (n mod 2 <> m mod 2)
let () = assert (gcd n m = 1)

let a = 2*m*n
let b = (m*m - n*n)
let c = (m*m + n*n)
let a = min a b and b = max a b
let () = printf "%d %d %d@." a b c

let file = sprintf "pyth-%d-%d-%d.cmb" a b c
let cout = open_out file
let fmt = formatter_of_out_channel cout
let () = fprintf fmt "debug on@\n@\n"
let () = fprintf fmt "timing on@\n@\n"

let hooks n =
  for k = 1 to n do
    fprintf fmt "pattern p_%d_%d = {@\n" n k;
    fprintf fmt "%s@\n" (String.make k '*');
    for i = 1 to k - 1 do fprintf fmt "*@\n" done;
    fprintf fmt "}@\n@\n"
  done

let () = hooks a; hooks b

let () =
  fprintf fmt "@[tiles all_tiles =@\n[ ";
  let tiles n =
    for k = 1 to n do
      fprintf fmt "p_%d_%d ~one ~sym" n k;
      if k < n then fprintf fmt ",@ "
    done in
  tiles a; fprintf fmt ",@ ";
  tiles b; fprintf fmt "]@]@\n@\n"

let () =
  fprintf fmt "problem pbm%d = {@\n" c;
  for k = 1 to c do fprintf fmt "%s@\n" (String.make c '*') done;
  fprintf fmt "} all_tiles@\n@\n";
  fprintf fmt "solve dlx pbm%d svg \"solutions/pyth-%d-%d-%d.svg\"@\n" c a b c;
  fprintf fmt "# count dlx pbm%d@\n" c

let () =
  fprintf fmt "@."; close_out cout


