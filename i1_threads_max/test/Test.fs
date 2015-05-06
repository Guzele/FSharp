(* Threads: maximum - eight week homework
   Author Guzel Garifullina 171
*)

module Test
open NUnit.Framework
open Maximum
[<TestCase (2, 0, 1, Result = 8)>] /// simple array
[<TestCase (2, 0, 7, Result = 8)>] /// if number of threads more
[<TestCase (20, 5, 5, Result = 20)>] /// the last thread has more elements than others
[<TestCase (20, 6, 5, Result = 20)>] 
[<TestCase (2, 0, 5, Result = 8)>]

let ``maximum simple situations``x i threadNum   = 
    let mutable arr = [|2; 1; 3; 8; 1; 6; 7|]
    arr.[i] <- x
    (maximum arr threadNum)  

/// Random tests

[<TestCase (1000, 1, Result = true)>] 
[<TestCase (100000000, 6, Result = true)>] 
[<TestCase (58385395, 9, Result = true)>] 
let ``random tests`` n threadNum =
    let rnd = System.Random()
    let arr = Array.init n ( fun _ -> rnd.Next ())
    (correct_maximum arr) = (maximum arr threadNum )




