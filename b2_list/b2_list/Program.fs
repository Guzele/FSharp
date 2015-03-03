(* List transformation with fold - second week homework
   Author Guzel Garifullina 171
   reverse
   Estimated time 30 minutes
   real time      2 hours   *)

let reverse l = List.fold (fun acc  x -> x :: acc ) [] l 

[<EntryPoint>]
let main argv = 
    let l = [1; 2; 3; 4; 5; 5; 4]
    printfn "Initial list %A"  l
    printfn "Reversed list %A" (reverse  l)
    0 

