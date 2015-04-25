(* Ring - seventh week homework
   Author Guzel Garifullina 171
*)
module Test
open Factorset
open NUnit.Framework
[<TestCase ( 2, Result = 0)>]
[<TestCase ( 3, Result = 1)>]
[<TestCase ( 5, Result = 0)>]
[<TestCase ( 11, Result = 10)>]
let check_example n =    
    ring n {
        let! a = 2 * 3
        let! b = 4
        return a + b
    }
[<TestCase ( 2, Result = 0)>]
[<TestCase ( 3, Result = 0)>]
[<TestCase ( 5, Result = 4)>]
[<TestCase ( 11, Result = 5)>]
let check_negative n =    
    ring n {
        let! a = - 2 - 1
        let! b = -3
        return a + b
    }