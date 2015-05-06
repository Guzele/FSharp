(* Parallel integral- eight week homework
   Author Guzel Garifullina 171
   Estimated time  1 hour
   Real time       30 minutes
*)
open Integral 

[<EntryPoint>]
let main argv =
    let f = sin
    printfn "%f" (integral f 0. (System.Math.PI) 1)
    0

