(* Parallel integral- eight week homework
   Author Guzel Garifullina 171
*)

module Integral

open System
open System.Threading 


let integral f a1 b1 (threadNum : int) =
    let sign a b =
        if a <= b
        then (1., a, b)
        else (-1., b, a)
    let s, a, b = sign a1 b1
    let dx = 0.0000001
    let res = ref 0.0
    let threadNumf = float threadNum
    let step = (b - a ) / threadNumf 

    let integrOfRange (i : int) res =
        let fi = float i
        let left_bord = a + (fi * step)

        let mutable right_bord = a + (fi + 1.0) * step 
        if right_bord >= (a + threadNumf * step )  
            then  right_bord <- b

        let sum = ref (0.0)
        let mutable x = left_bord
        while x <= (right_bord - dx) do
            sum := !sum + (f x) + f (x + dx)
            x <- x + dx
        sum := (!sum) * dx / 2.0

        lock res (fun () -> 
            res := !sum + !res) 

    let threadArr = Array.init threadNum ( fun i ->
        new Thread ( ThreadStart ( fun _ -> 
            integrOfRange i  res))
        )
    for t in threadArr do
        t.Start()
    for t in threadArr do
        t.Join()
    (System.Math.Round (s * (!res), 3))