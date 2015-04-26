(* Scientific calc- seventh week homework
   Author Guzel Garifullina 171
 *)

module Calculator
open System
open Stack

type Char = Num of float|E| Pi | LB | RB | Op of string| Var of string
type Tree = Numb of float | Node of string * Tree * Tree | Node1 of string * Tree


let scan (str : string) =
    let n = str.Length
    let mutable i = 0
    let mutable l  = []

    let readNum ind l =
        let mutable j = ind
        if str.[j] = '-'
        then j <- j + 1
        while ( j < n )  && (Char.IsDigit (str.[j]) || (str.[j] = '.')) do
            j <- j + 1
        if j = ind 
        then failwith "InternalError"
        else 
        let num = float (str.Substring ( ind , j - ind  ) )
        if (num < 0.0)
            then 
                if str.[j] = ')'
                    then (((Num num) :: l), j)
                    else  
                        (((Num num) :: (LB:: l)), (j - 1))

            else
            (((Num num) :: l), j - 1) 


    while i < str.Length do
        let ind = i
        match str.[i] with
        |'+' |'-' |'*' |'/' |'%' |'^' as op -> l <- (Op (string op) ):: l
        |'s' | 'c'| 't'                     ->
            l <- (Op (str.Substring ( i , 3  ) ) ):: l
            i <- i + 2
        | 'l'                               ->
            if str.[i + 1 ] = 'o'
            then
               l <- (Op (str.Substring ( i , 3  ) ) ):: l
               i <- i + 2 
            else
               l <- (Op (str.Substring ( i , 2  ) ) ):: l
               i <- i + 1 
        | ' '  -> ()
        | 'e'  -> l <- E :: l
        | 'P'  -> 
            l <- Pi :: l
            i <- i + 1

        | ch when (Char.IsDigit ch) || (ch = '.') -> 
                let list, index = readNum ind l
                l <- list
                i <- index
           
        | ch when Char.IsLetter ch ->
            let mutable j = i + 1
            while ( j < n )  && (Char.IsLetter (str.[j])) do
                j <- j + 1
            while ( j < n )  && (Char.IsDigit (str.[j])) do
                j <- j + 1

            let s = str.Substring ( i , j - i  )
            l <- (Var s ):: l
            i <- j

        | '('  ->  
            if str.[i + 1] = '-'
            then 
                let list, index = readNum (ind + 1) l
                l <- list
                i <- index
            else l <- LB :: l
        | ')' -> l <- RB :: l
        | _   -> failwith "Internal"
        i <- i + 1
    List.rev l


let isCorrect prev now =
    (*let findType (x : Char) =
        match x with
        |Num _| Var _ -> 1
        |LB           -> 2
        |RB           -> 3
        |Op _         -> 4 
        | minus       -> 5
        | point       -> 6
        | consta      -> 7*)
    match now, prev with
    | 1, (0 | 1 | 2 | 4 | 5 | 6 | 8 ) -> true
    | 2, (0 | 2 | 4 | 5 | 6 | 8 )     -> true
    | 3, (1 | 3 | 6 | 7)             -> true
    | 4, (1 | 3 | 6 | 7 )         -> true
    | 5, (1 | 2 | 3 | 6 | 7 | 8)     -> true
    | 6, (0 | 1 | 2 | 4 | 5 | 8)     -> true
    | 7, (0 | 2 | 4 | 5 | 8)         -> true
    | 8, (0 | 2 | 4 | 5 | 8)         -> true
    | _           -> false


let isBracketCorrect (str : string) =
    let mutable op = 0
    let mutable cl = 0
    for i = 0 to (str.Length - 1) do
        match (str.[i]) with
        | '(' -> op <- op + 1
        | ')' -> cl <- cl + 1
        | _   -> ()
    op = cl
/// no ends with operation
let notEndsOp (str : string) = 
    match str.[(str.Length - 1)] with
    | '+' |'-' |'*' |'/' |'%' |'^'| '('| 'g' | 'n' | 's' | '%' | 'r'  -> false
    | _                            -> true 
let isCorNum (s : string)= 
    let mutable i = 0
    let n = s.Length - 1
    let mutable flag = true
    let mutable amt = 0
    while (i <= n) && flag do
        amt <- 0
        while (i<= n) && ((Char.IsDigit (s.[i])) || ( s.[i] = '.')) do
            if s.[i] = '.'
            then amt <- amt + 1
            i <- i + 1
        if amt > 1
        then flag <- false
        i <- i + 1
    flag

let priority ch = 
    match ch with
    | Op ("+"| "-")      -> 1 
    | Op ("*"| "/"| "%") -> 2
    | Op ("log"| "ln" | "sin" | "cos" | "tan" | "sqr") -> 3
    | Op "^"             -> 4
    | _                  -> 0


let toTree list =
    let oper = (new Stack<Char> ()) :> IStack<Char>
    let num  = (new Stack<Tree> ()) :> IStack<Tree>
    let makeNode operation =
        match operation with
        | Op oper ->
            if (priority operation) <> 3
            then
                let y = num.Pop
                let x = num.Pop
                num.Push (Node (oper, x, y ))
            else
                let y = num.Pop
                num.Push (Node1 (oper, y ))
        | _  -> ()

    let rec context list =
        match list with
        | [] -> 
            while not oper.Empty do
                makeNode (oper.Pop)
        | x :: list ->
            match x with
            | Num x-> num.Push (Numb x)
            | Pi   -> num.Push (Numb (System.Math.PI))
            | E    -> num.Push (Numb (System.Math.E))
            | LB    -> 
                oper.Push x
            | RB    ->
                let mutable operation = oper.Pop
                while operation <> LB do
                    makeNode operation
                    operation <- oper.Pop
            | _     -> 
                let prior = priority x
                let mutable y = LB // just for initialization
                if oper.Empty 
                then 
                    oper.Push x
                else
                     y <- oper.Pop
                     while (not oper.Empty) && ( priority y >= prior) && (prior <> 4) do
                        makeNode y
                        y <- oper.Pop
                     if ( priority y >= prior) && (prior <> 4)
                     then makeNode y
                     else oper.Push y
                     oper.Push x
            context  list        
    context list
    num.Pop

let rec treeToSolution context tree =
    let operation ch x y =
            match ch with
            |"+" ->  x + y
            |"-" ->  x - y
            |"*" ->  x * y
            |"/" ->  x / y
            |"%" ->  x % y
            |"^" ->  x ** y
            | _  ->  failwith "Internal Error"
    let operation1 ch x  =
            match ch with
            |"sin" -> sin x 
            |"cos" -> cos x
            |"tan" -> tan x
            |"log" -> log x
            |"ln" ->  Math.Log(10.0, Math.E)
            |"sqr" -> x ** ( 0.5)
            | _  ->  failwith "Internal Error"

    match tree with
    | Numb x -> x
    | Node (ch, tr1, tr2) -> operation ch (treeToSolution context tr1)(treeToSolution context tr2)
    | Node1  (ch, tr) -> operation1 ch (treeToSolution context tr)

let calculate str =
    treeToSolution [] (str |> scan |> toTree)