(* Interpretator of L - sixth week homework
   Author Guzel Garifullina 171
*)
module IntTest

open NUnit.Framework
open Expr
open Stmt
open Interpreter
/// read from list, check while
(* read(x);
   read(n);
   res := 1;
   while (n) {
     res := res * x;
     n := n - 1
   };
   write(res) 
input list [2;2]*)
[<TestCase (";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres\n", Result="4\n")>]

/// negative argument, write more than 1 argument
(*  x:= 5;
    s:= -12;
    t:= x + s - 2;
    write(x);
    write(s);
    write(t) *)
[<TestCase (";\n:=\nx\n5\n;\n:=\ns\n-12\n;\n:=\nt\n-\n+\nx\ns\n2\n;\nwrite\nx\n;\nwrite\ns\nwrite\nt\n", Result="5\n-12\n-9\n")>]

/// if then changes parametr but not write
(*  x:= 5;
    if (x) {
      l := -12
    }
    else {
      l := 2;
      write(l)} *)
[<TestCase (";\n:=\nx\n5\nif\nx\n:=\nl\n-12\n;\n:=\nl\n2\nwrite\nl\n", Result="")>]

///if else with 2 actions
(*  x:= -5;
    if (x) {
      l := 12
    }
    else {
      l := -2;
      write(l)} *)
[<TestCase (";\n:=\nx\n-5\nif\nx\n:=\nl\n12\n;\n:=\nl\n-2\nwrite\nl\n", Result="-2\n")>]
let intTest str =
    (interpritate (str |> inpToStr |> makeProgramTree) [2;2] )