(* Parallel array multiplication - eighth week homework
   Author Guzel Garifullina 171
   Estimated time 1 hour
   real time      1 hour
*)
open System
open ArrayMult
[<EntryPoint>]
let main argv = 
    let n: int = 1000
    let m = 500
    let l = 5000
    let rnd1 = System.Random(0)
    let rnd2 = System.Random(5)
    let a :  int [,]  = Array2D.init n m ( fun _ _ -> rnd1.Next ())
    let b :  int [,]  = Array2D.init m l ( fun _ _-> rnd2.Next ())
    let c = array_mult  a b 5

    0 
// (n, m, l) (1000,500,5000)   
// 1 thread -  26,365 s
// 2 threads - 13,644 
// 3 threads - 14,045
// 4 threads - 13,523 
// 5 threads - 14,388
