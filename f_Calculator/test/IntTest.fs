(* Calculator - sixth week homework
   Author Guzel Garifullina 171
*)
module IntTest

open NUnit.Framework
open Expr
open Stmt
open Interpreter
/// read from list, check while
[<TestCase (";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres\n", Result="4\n")>]
/// negative argument, write more than 1 argument
[<TestCase (";\n:=\nx\n5\n;\n:=\ns\n-12\n;\n:=\nt\n-\n+\nx\ns\n2\n;\nwrite\nx\n;\nwrite\ns\nwrite\nt\n", Result="5\n-12\n-9\n")>]
/// if then changes parametr but not write
[<TestCase (";\n:=\nx\n5\nif\nx\n:=\nl\n-12\n;\n:=\nl\n2\nwrite\nl\n", Result="")>]
///if else with 2 actions
[<TestCase (";\n:=\nx\n-5\nif\nx\n:=\nl\n12\n;\n:=\nl\n-2\nwrite\nl\n", Result="-2\n")>]
let intTest str =
    (interpritate (str |> inpToStr |> makeProgramTree) [2;2] )

  