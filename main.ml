
let m = [| [|true; false; false; true; true |];
           [|true; true; false; true; false |];
           [|true; false; true; true; false |];
           [|false; true; true; true; true |] |]

let _ = Dlx.find_solution m


