(* Interpretator of L - sixth week homework
   Author Guzel Garifullina 171
*)

module Stmt 
open Expr

type t = 
       | Read   of string
       | Write  of Expr.Tree
       | Assign of string * Expr.Tree /// :=
       | Seq    of t * t             ///  ; 
       | If     of Expr.Tree * t * t
       | While  of Expr.Tree * t

let inpToStr (s : string) : string list = 
        s.Split '\n'
        |> Array.filter ((<>)"")
        |> Array.toList


let makeProgramTree (l : string list) =
    let rec make l =
        match l with 
            | [] -> failwith "Internal Error"
            | x :: l -> 
                match x with 
                | ";" ->
                    let x1, l1 = (make l)
                    let x2, l2 = (make l1)
                    ( Seq (x1, x2), l2)
                | ":=" -> 
                    let x1 = l.Head
                    let l1 = l.Tail
                    let x2, l2 = (makeOpTree l1)
                    ( Assign (x1, x2), l2)
                | "read" -> 
                    let x1 = l.Head
                    let l1 = l.Tail
                    ( Read (x1), l1)
                | "write" ->
                    let x1, l1 = (makeOpTree l)
                    ( Write(x1), l1)
                | "if"    -> 
                    let exp, l_exp = (makeOpTree l)
                    let x1, l1 = (make l_exp)
                    let x2, l2 = (make l1)
                    ( If (exp, x1, x2), l2)
                | "while" ->
                    let exp, l_exp = (makeOpTree l)
                    let x1, l1 = (make l_exp)
                    ( While (exp, x1), l1)
                | _       -> failwith "Unnkown command"
    fst ( make l) 


