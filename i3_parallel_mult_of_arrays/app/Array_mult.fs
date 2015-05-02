(* Parallel array multiplication - eighth week homework
   Author Guzel Garifullina 171
*)

module ArrayMult
open System
open System.Threading

let array_mult (arr1 : int [,]) (arr2 : int [,]) (threadNum : int)  =
    let n = arr1.GetUpperBound 0
    let m = arr1.GetUpperBound 1
    let m1 = arr2.GetUpperBound 0
    let l = arr2.GetUpperBound 1
    if (m <> m1)
    then failwith "Wrong arrays"
    let res : int [,] = Array2D.create (n + 1) (l + 1) 0

    let mult_one_line n l =
        let mutable sum = 0 /// for int
        for i = 0 to m do
            sum <- sum + arr1.[n,i] * arr2.[i,l]
        res.[n,l] <- sum


    let multPart i  =
        let step1 = (n + 1) / threadNum 
        let step2 = (l + 1) / threadNum 

        let left_bord1 = (i * step1)
        let mutable right_bord1 = (i + 1) * step1 - 1
        if right_bord1 >= (threadNum * step1 - 1) 
            then  right_bord1 <- n

        let left_bord2 = (i * step2)
        let mutable right_bord2 = (i + 1) * step2 - 1
        if right_bord2>= (threadNum * step2 - 1) 
            then  right_bord2 <- l

        for i = left_bord1 to right_bord1 do
            for j = left_bord2 to right_bord2 do
                mult_one_line i j

    let threadArr = Array.init threadNum ( fun i ->
        new Thread ( ThreadStart ( fun _ -> 
            multPart i ))
        )
    for t in threadArr do
        t.Start()
    for t in threadArr do
        t.Join()
    res