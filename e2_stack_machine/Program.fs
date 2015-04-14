(* Stack machine - fifth week homework
   Author Guzel Garifullina 171
   Estimated time 30 minutes
   real time      1 hour
 *)

open NUnit.Framework
open System
open System.IO

type Tree = Num of int| Var of string| BinOp of char * Tree * Tree
let inpToInfix (s : string) : string list = 
        s.Split '\n'
        |> Array.filter ((<>)"")
        |> Array.toList
        |> List.rev

let infixToTree (list : string list) =
    let isOperation (str : string) =
        let ch = str.[0]
        match ch with
        |'+' |'*' |'/' |'%' |'^' -> true
        |   '-'  -> str.Length = 1
        | _ -> false
    let readNum (str : string) =
        let mutable n = str.Length
        let mutable neg = 1
        let mutable i = 0
        if str.[ i ] = '-'
            then neg <- -1
                 n <- n - 1
                 i <- i + 1
        ( neg * int (str.Substring ( i , n  ) ) )
    let rec make l =
        match l with 
            | [] -> failwith "Internal Error"
            | s :: l -> 
                match s with 
                | s when isOperation s ->
                    let y, l1 = (make l)
                    let x, l2 = (make l1)
                    (BinOp (s.[0],x, y), l2)
                | s when (Char.IsDigit s.[0]) || (s.[0] = '-') ->   
                    (Num (readNum s), l)
                | _ -> ///context  
                    (Var ( s), l)
    fst ( make list) 

let rec treeToSolution context tree =
    let operation ch x y =
            match ch with
            |'+' ->  x + y
            |'-' ->  x - y
            |'*' ->  x * y
            |'/' ->  x / y
            |'%' ->  x % y
            |'^' -> pown x y
            |_   -> failwith "Internal error"

    let initialize (context : string) (x : string) =
        let n = String.length context
        let mutable i = (context.IndexOf ( x + " ")) + String.length x
        while  (context.[i] = ' ') do
            i <- i + 1
        if context.[i] = '='
            then i <- i + 1
            else failwith "Wrong context"
        while  (context.[i] = ' ') do
            i <- i + 1
        let readNum (context : string) ind  =
            let mutable neg = 1
            let mutable i = ind
            if context.[ i ] = '-'
            then neg <- -1
                 i <- i + 1
            let mutable j = i 
            while ( j < n )  && (Char.IsDigit (context.[j])) do
                j <- j + 1
            
            let num = neg * int (context.Substring ( i , j - i  ) )
            num
        readNum context i

    match tree with
    | Num x -> x
    | Var x -> initialize context x
    | BinOp (ch, tr1, tr2) -> operation ch (treeToSolution context tr1)(treeToSolution context tr2)

let read  (input: string) =
    use stream = new StreamReader (input)
    stream.ReadToEnd ()

let write (output : string) (ans : string) =
    use stream = new StreamWriter (output)
    stream.WriteLine ( ans)  
///number
[<TestCase ("0\n", Result = 0)>]
[<TestCase ("-13\n", Result = -13)>]
/// subtraction
[<TestCase ("2\n1\n-", Result = 1)>]
///negative number
[<TestCase ("2\n-1\n+", Result = 1)>]
/// many operations
[<TestCase ("7\n2\n3\n*\n-\n", Result = 1)>]
[<TestCase ("10\n15\n-\n3\n*\n", Result = -15)>]
[<TestCase ("3\n10\n15\n-\n*\n", Result = -15)>]

let calculate str = 
    str |> inpToInfix |> infixToTree |>  treeToSolution ""

[<EntryPoint>]
let main argv = 
    read ("input.txt")|> inpToInfix |> infixToTree |>  treeToSolution ""|> string |> write "output.txt"
    0     