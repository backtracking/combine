
open Combine

let emc =
  [| [| true; false; true; true;  |];
     [| false; true; true; false; |];
     [| true; true; false; true; |];
     [| true; false; false; true; |];
     [| false; true; false; false |] |]

let sat = Emc.Sat.create ~primary:4 emc

let () = Format.printf "%a@." Emc.Sat.print sat

