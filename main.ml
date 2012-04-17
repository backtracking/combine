
let m = [| [|true; false|];
           [|false; true|];
           [|false; true|]|]

let _ = Dlx.find_first_solution m


