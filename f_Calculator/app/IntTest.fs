(* Calculator - sixth week homework
   Author Guzel Garifullina 171
*)
module IntTest

open NUnit.Framework
open Expr
open Stmt
open Interpreter



[<TestCase (";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres\n", Result="4\n")>]
[<TestCase (";\n:=\nx\n5\nif\nx\n:=\nl\n-12\n;\n:=\nl\n2\nwrite\nl\n", Result="")>]
let intTest str =
    (interpritate (str |> inpToStr |> makeProgramTree) [2;2] )

      