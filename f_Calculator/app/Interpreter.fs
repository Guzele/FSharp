(* Calculator - sixth week homework
   Author Guzel Garifullina 171
*)

module Interpreter
open System.IO
open Expr
open Stmt

type IStack<'T> =
    abstract member Id : list<'T> with get,set
    abstract member Empty: bool
    abstract member Pop  : 'T
    abstract member Push : 'T -> unit
    abstract member Rev  : unit

type Stack<'T when 'T : equality> () =
    interface IStack<'T> with
        member val Id = [] with get,set
        member this.Empty = (this:> IStack<'T>).Id = []
        member this.Pop   = 
            match (this:> IStack<'T>).Id with 
            | []     -> failwith "Intrnal Error:Stack Pop"
            | x :: l -> 
                (this:> IStack<'T>).Id <- l
                x
        member this.Push x = 
            (this:> IStack<'T>).Id <- x :: (this:> IStack<'T>).Id
        member this.Rev  = 
            (this:> IStack<'T>).Id <- List.rev ((this:> IStack<'T>).Id)

let interpritate (tree : Stmt.t) (list : int list) =
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



