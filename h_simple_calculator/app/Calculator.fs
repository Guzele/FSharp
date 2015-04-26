(* Simple Calculator - seventh week homework
   Author Guzel Garifullina 171
 *)

module Calculator
open System
open Stack

type Char = Num of int | LB | RB | Op of char| Var of string
type Tree = Numb of int | Node of char * Tree * Tree


let scan (str : string) =
    let n = str.Length
    let mutable i = 0
    let mutable l  = []

    let readNum ind l =
        let mutable neg = 1
        let mutable i = ind
        if str.[ i ] = '-'
            then neg <- -1
                 i <- i + 1
        let mutable j = i 
        while ( j < n )  && (Char.IsDigit (str.[j])) do
            j <- j + 1
        if j = i 
        then failwith "InternalError"
        else 
        let num = neg * int (str.Substring ( i , j - i  ) )
        if neg = -1 
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
        |'+' |'-' |'*' |'/' |'%' |'^' as op -> l <- (Op op ):: l
        | ' '  -> ()
        | ch when Char.IsDigit ch -> 
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
        | minus       -> 5*)

    match now, prev with
    | 1, (0 | 1 | 2 | 4 | 5) -> true
    | 2, (0 | 2 | 4 | 5) -> true
    | 3, (1 | 3 ) -> true
    | 4, (1 | 3)     -> true
    | 5, (1 | 2| 3)     -> true
    | _             -> false


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
    | '+' |'-' |'*' |'/' |'%' |'^'| '(' -> false
    | _                            -> true 

let priority ch = 
    match ch with
    | Op ('+'| '-')      -> 1 
    | Op ('*'| '/'| '%') -> 2
    | Op '^'             -> 3
    | _                  -> 0


let toTree list =
    let oper = (new Stack<Char> ()) :> IStack<Char>
    let num  = (new Stack<Tree> ()) :> IStack<Tree>
    let makeNode operation =
        match operation with
        | Op oper ->
            let y = num.Pop
            let x = num.Pop
            num.Push (Node (oper, x, y ))
        | _  -> ()

    let rec context list =
        match list with
        | [] -> 
            while not oper.Empty do
                makeNode (oper.Pop)
        | x :: list ->
            match x with
            | Num x-> num.Push (Numb x)
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
                     while (not oper.Empty) && ( priority y >= prior) && (prior <> 3) do
                        makeNode y
                        y <- oper.Pop
                     if ( priority y >= prior) && (prior <> 3)
                     then makeNode y
                     else oper.Push y
                     oper.Push x
            context  list        
    context list
    num.Pop

let rec treeToSolution context tree =
    let operation ch x y =
            match ch with
            |'+' ->  x + y
            |'-' ->  x - y
            |'*' ->  x * y
            |'/' ->  x / y
            |'%' ->  x % y
            |'^' -> 
                let rec pow x y =
                    match y with
                    | y when (y < 0) -> 0
                    | 0              -> 1
                    | _        ->  x * (pow x (y - 1))
                pow x y
            |_   -> failwith "Internal error"

    match tree with
    | Numb x -> x
    | Node (ch, tr1, tr2) -> operation ch (treeToSolution context tr1)(treeToSolution context tr2)


let calculate str =
    treeToSolution [] (str |> scan |> toTree)