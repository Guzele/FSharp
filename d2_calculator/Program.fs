(* Calculator - forth week homework
   Author Guzel Garifullina 171
   Estimated time 2 hours
   real time      5 hours
 *)

open System

type Char = Num of int | LB | RB | Op of char| Var of string
type Tree = Numb of int |Varb of string| Node of char * Tree * Tree

// after bracket ' ' or anyhing

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
        then failwith "Error"
        else 
        let num = neg * int (str.Substring ( i , j - i  ) )
        if neg = -1 
            then 
                if str.[j] = ')'
                    then (((Num num) :: l), j)
                    else  failwith "Error"
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
        | _   -> failwith "Error"
        i <- i + 1
    List.rev l


let isCorrect str =
    let findType (x : Char) =
        match x with
        |Num _| Var _ -> 1
        |LB           -> 2
        |RB           -> 3
        |Op _         -> 4

    let rec isCor prev list bra  =
        match list with
        | []     ->
           if bra = 0
           then true
           else printfn "%d" bra
                false 
        | x :: list ->
            let op = findType x
            match op, prev with
            | 1, (0| 2 | 4) -> isCor op list bra 
            | 2, (0| 2 | 4) -> isCor op list (bra + 1)
            | 3, (0| 1 | 3) -> isCor op list (bra - 1)
            | 4, (0| 1 | 3) -> isCor op list bra
            | _             -> false
    isCor 0 str 0

type IStack<'T> =
    abstract member Id : list<'T> with get,set
    abstract member Empty: bool
    abstract member Pop  : 'T
    abstract member Push : 'T -> unit


type Stack<'T when 'T : equality> () =
    interface IStack<'T> with
        member val Id = [] with get,set
        member this.Empty = (this:> IStack<'T>).Id = []
        member this.Pop   = 
            match (this:> IStack<'T>).Id with 
            | []     -> failwith "Intrnal Error2"
            | x :: l -> 
                (this:> IStack<'T>).Id <- l
                x
        member this.Push x = 
            (this:> IStack<'T>).Id <- x :: (this:> IStack<'T>).Id


let priority ch = 
    match ch with
    | Op ('+'| '-')      -> 1 
    | Op ('*'| '/'| '%') -> 2
    | Op '^'             -> 3
    | _                  -> 0

let toPolish list =
    let stack  = (new Stack<Char> ()) :> IStack<Char>
    let rec context list =
        match list with
        | [] -> 
            let mutable cont = []
            while not stack.Empty do
                cont <- stack.Pop :: cont
            cont
        | x :: list ->
            match x with
            | Num _| Var _ -> (context  list) @ [x]
            | LB    -> 
                stack.Push x
                context  list
            | RB    ->
                let mutable cont = []
                let mutable p = stack.Pop
                while p <> LB do
                    cont <- p :: cont
                    p <- stack.Pop
                (context  list) @ cont
            | _     -> 
                let prior = priority x
                let mutable cont = []
                let mutable y = LB // just for initialization
                if stack.Empty 
                then 
                    stack.Push x
                    context  list
                else
                     y <- stack.Pop
                     while (not stack.Empty) && ( priority y >= prior) && (prior <> 3) do
                        cont <- y :: cont 
                        y <- stack.Pop
                     if (priority y >= prior)&& (prior <> 3)
                     then cont <- y :: cont
                     else stack.Push y
                     stack.Push x
                     (context  list) @ cont
    context list


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
            | Var x-> num.Push (Varb  x)
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
                    | y when (y <= 0) -> 1
                    | _        ->  x * (pow x (y - 1))
                pow x y
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
    | Numb x -> x
    | Varb x -> initialize context x
    | Node (ch, tr1, tr2) -> operation ch (treeToSolution context tr1)(treeToSolution context tr2)
   





[<EntryPoint>]
let main argv = 
    let str = "1 + er8 ^ 2 - ((-35) / 40)"
    let context = "er8 = 3"
    let str1 = "1 -2 ^ 3  +1"
    let list = scan str 
    toTree list
    let list1 = scan str1 
    let f = toTree list 
    treeToSolution context f 
    //printfn "%A" argv
    0 

