(* Parallel integral- eight week homework
   Author Guzel Garifullina 171
*)
module Test
open Integral
open NUnit.Framework
// sinus
[<TestCase (0., (System.Math.PI), 1, Result = 2.)>] 
[<TestCase (0., System.Math.PI, 2, Result = 2.)>] 
[<TestCase (0., 0., 2, Result = 0.)>] 
let ``integral of sin``a b threadNum   = 
    (integral sin a b  threadNum)  

// cosinus
[<TestCase (0., (System.Math.PI), 1, Result = 1.)>] 
[<TestCase (0., System.Math.PI, 2, Result = 1.)>] 
[<TestCase (0., 0., 2, Result = 0.)>] 
let ``integral of cos``a b threadNum   = 
    (integral cos a (b / 2.)  threadNum)  

// x
[<TestCase (0., 3. , 1, Result = 4.5)>] 
[<TestCase (1., 3., 2, Result = 4.)>] 
[<TestCase (0., 0., 2, Result = 0.)>] 
let ``integral of x``a b threadNum   = 
    let f x = x
    (integral f a b  threadNum) 

// x^2 * 3
[<TestCase (0., 3. , 1, Result = 27.)>] 
[<TestCase (1., 3., 2, Result = 26.)>] 
[<TestCase (0., 1., 2, Result = 1.)>] 
[<TestCase (1., 0., 2, Result = -1.)>] 
let ``integral of 3 * x^2``a b threadNum   = 
    let f x = 3. * x * x
    (integral f a b  threadNum) 
