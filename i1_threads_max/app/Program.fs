(* Threads: maximum - eight week homework
   Author Guzel Garifullina 171
   Estimated time 1 hour
   real time      1 hour
*)
open Maximum

[<EntryPoint>]
let main argv = 
    let n = 100000000
    let rnd = System.Random(0)
    let arr = Array.init n ( fun _ -> rnd.Next ())
    printfn "%d" (maximum arr 3 )
   
    0 

(*
number array = 100000000
simple function: 10,26s user 0,12s system 100% cpu 10,366 total
one thread :  10,21s user 0,13s system 100% cpu 10,322 total
two threads:  10,48s user 0,14s system 170% cpu 6,228 total
three threads: 10,53s user 0,16s system 167% cpu 6,381 total
four threas: 10,48s user 0,17s system 162% cpu 6,576 total
five threads: 10,55s user 0,19s system 162% cpu 6,617 total

*)