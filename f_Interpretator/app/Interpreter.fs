(* Interpretator of L - sixth week homework
   Author Guzel Garifullina 171
*)

module Interpreter
open System.IO
open Stack
open Expr
open Stmt

type Mode = Mode1 | Mode2
/// mode 1 console output, mode 2 to file
let interpritate (tree : Stmt.t) (list : int list) (mode : Mode) =
    let context = new Context()  
    let l = (new Stack<int> ()) :> IStack<int>
    l.Id <- list
    let output = (new Stack<string> ()) :> IStack<string>
    let rec execute (tree : Stmt.t)  =
        match tree with
        | Read ( str) -> 
            let mutable x = 0 // for init
            if not (l.Empty)
                then x <- l.Pop
                else printfn "Not enougth arguments, please write value of %s" str
                     x <- int (System.Console.ReadLine())
            (context:> IContext).Push str x
        | Write ( expr ) -> 
            let num = expToSolution context expr
            if (mode = Mode1) then printfn "%d" num
            output.Push (string (num))
        | Seq (action1, action2) ->
            execute action1
            execute action2
        | Assign (str, expr) ->
            let x = expToSolution context expr
            (context:> IContext).Push str x
        | If (expr, act_then, act_else) ->
            let x = expToSolution context expr
            if x > 0 
                then execute act_then
                else execute act_else
        | While (expr, action) ->
            let mutable x = expToSolution context expr
            while (x > 0) do 
                execute action
                x <- expToSolution context expr
    execute tree
    output.Rev
    let mutable str = ""
    while not (output.Empty) do
        str <- str + (output.Pop) + "\n"
    str
    /// in both modes returns string with result, so in first mode we should ignore result