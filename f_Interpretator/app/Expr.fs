(* Interpretator of L - sixth week homework
   Author Guzel Garifullina 171
 *)

module Expr
open System

type Tree = Num of int |Var of string| BinOp of char * Tree * Tree

let isOperation (str : string) =
    let ch = str.[0]
    match ch with
    |'+' |'*' |'/' |'%' |'^' -> true
    |'-'  -> str.Length = 1
    | _ -> false

let rec makeOpTree ( l : string list) =
    let readNum (str : string) =
        let mutable n = str.Length
        let mutable neg = 1
        let mutable i = 0
        if str.[ i ] = '-'
            then neg <- -1
                 n <- n - 1
                 i <- i + 1
        ( neg * int (str.Substring ( i , n  ) ) )

    match l with 
    | [] -> failwith "Internal Error"
    | elemnt :: l ->
        match elemnt with
        | _ when isOperation elemnt->
            let x, l1 = (makeOpTree l)
            let y, l2 = (makeOpTree l1)
            ( BinOp (elemnt.[0], x, y), l2)
        | _ when (Char.IsDigit elemnt.[0]) || ( elemnt.[0] = '-') ->
            (Num (readNum elemnt), l)
        | _  -> (Var (elemnt), l)


type IContext =
    abstract member Empty   : bool
    abstract member Length  : int
    abstract member FindInd : string -> Option<int>
    abstract member Push    : string -> int -> unit
    abstract member Value   : string -> int

type Context () =
    let mutable names  = [||] 
    let mutable values = [||] 
    interface IContext with
        member this.Empty = names = [||]
        member this.Length = names.Length 
        member this.FindInd str  = 
            let n = (this:> IContext).Length  
            let mutable b = true
            let mutable i = 0
            while ( i < n) && b do
                if names.[i] = str
                then b <- false
                i <- i + 1
            if b 
                then None
                else Some (i - 1)
      
        member this.Push str x = 
            match ((this:> IContext).FindInd str) with
            | None     -> 
                names <- Array.append [|str|] names
                values <- Array.append [|x|] values
            | Some ind -> values.[ind] <- x
        member this.Value str =
            match ((this:> IContext).FindInd str) with
            | None     -> failwith "Error"
            | Some ind -> values.[ind]

let rec expToSolution context tree =
    let operation ch x y =
            match ch with
            |'+' ->  x + y
            |'-' ->  x - y
            |'*' ->  x * y
            |'/' ->  x / y
            |'%' ->  x % y
            |'^' ->  pown x y
            |_   -> failwith "Internal error"

    match tree with
    | Num x -> x
    | Var x -> (context:> IContext).Value x
    | BinOp (ch, tr1, tr2) -> operation ch (expToSolution context tr1)(expToSolution context tr2)
   
 