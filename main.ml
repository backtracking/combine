
let m = [| [|true; true|];
           [|true; true|];
           [|true; true|]|]

let pieces = [
  [|[|true|]; [|true|]|];
  [|[|true; true|]|]
]

let _ = 
  if Tiling.existing_position m 2 1 then 
    Format.printf "Ok@."
  else 
    Format.printf "Not ok@."



