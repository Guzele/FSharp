(* Threads: maximum - eight week homework
   Author Guzel Garifullina 171
*)
module Maximum
open System

open System.Threading

let correct_maximum (arr : 'T []) =
    let maximum left_bord right_bord =
        let mutable max = arr.[left_bord]
        for i = (left_bord + 1) to right_bord do
            if arr.[i] > max
            then max <- arr.[i]
        max 
    maximum 0 (arr.Length - 1) 

let maximum (arr : 'T []) (threadNum : int)  =
    let res = ref (arr.[0])
    let right_bord_glob = arr.Length - 1
    let step = (right_bord_glob + 1) / threadNum 

    let maxOfRange i res =
        let left_bord = (i * step)

        let mutable right_bord = (i + 1) * step - 1
        if right_bord >= (threadNum * step - 1) 
            then  right_bord <- right_bord_glob

        let max = ref (arr.[left_bord])
        for i = (left_bord + 1) to right_bord do
            if arr.[i] > (!max)
            then max := arr.[i]

        lock res (fun () -> 
            if (!res) < (!max)
            then res := !max) 

    let threadArr = Array.init threadNum ( fun i ->
        new Thread ( ThreadStart ( fun _ -> 
            maxOfRange i  res))
        )
    for t in threadArr do
        t.Start()
    for t in threadArr do
        t.Join()
    (!res)