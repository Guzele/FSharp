(* Parallel array multiplication - eighth week homework
   Author Guzel Garifullina 171
*)
module Test
open ArrayMult
open NUnit.Framework

[<TestCase ( 1, Result = true)>] /// simple array
[<TestCase ( 7, Result = true)>] /// if number of threads more
let ``multiplication array to column`` threadNum = 
    let a = Array2D.init 2 3 ( fun i j -> i + j)
    let b = Array2D.init 3 1 ( fun i j -> 1)
    let c = Array2D.init 2 1 ( fun i j -> 3 * (i  + 1))
    (array_mult a b threadNum) =  c

[<TestCase ( 1, Result = true)>] /// simple array
[<TestCase ( 7, Result = true)>] /// if number of threads more
let ``multiplication array to array`` threadNum = 
    let a = Array2D.init 2 3 (fun i j -> i* j)
    let b = Array2D.init 3 3 (fun i j -> i-j)
    let c = Array2D.init 2 3 ( fun i j -> i * (5 - 3  *j ))
    (array_mult a b threadNum) =  c


[<TestCase ( 100, 200, 30, Result = true)>] /// if number of threads more
let ``multiplication random array to array`` n m l  = 
    let a = Array2D.init n m (fun i j -> i* j)
    let b = Array2D.init m l (fun i j -> i - j )
    let c = (array_mult a b 1) 
    let d = (array_mult a b 9) 
    c = d



