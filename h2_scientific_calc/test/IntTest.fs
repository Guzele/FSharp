(* Simple calculator- seventh week homework
   Author Guzel Garifullina 171
*)
module IntTest

open NUnit.Framework
open Calculator
[<TestCase ("0", Result = 0)>]
[<TestCase ("13", Result = 13)>]
[<TestCase ("(-13)", Result = -13)>]
[<TestCase ("3 ^ 1 ^ 2", Result = 3)>]
[<TestCase ("1 - 2 - 3", Result =  -4)>]
[<TestCase ("1 + 2 * 3 ^ 2", Result = 19)>]
[<TestCase ("34 -45 +12", Result = 1)>]
[<TestCase ("((-34) + 24) * (23 - 13)", Result = -100)>]
[<TestCase ("(34 + 24) * (23 - 13)", Result = 580)>]
[<TestCase ("((-34) - 24) / (23 - 13)", Result = -5.8)>]
[<TestCase ("120 % 11 +5 ^ (-1)", Result = 10.2)>]
[<TestCase ("120 % 11 +5 ^ (-1)^ 2", Result = 15)>]
let ``calculate without context`` str  = 
     calculate str
