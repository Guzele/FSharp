(* Calculator with output- fifth week homework
   Author Guzel Garifullina 171
   Estimated time 1 hour
   real time      2 hours
 *)
open NUnit.Framework
open System
open System.IO

let read  (input: string) =
    use stream = new StreamReader (input)
    stream.ReadToEnd ()

let write (output : string) (ans : string) =
    use stream = new StreamWriter (output)
    stream.WriteLine ( ans)

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
        | ch when Char.IsWhiteSpace ch   -> ()
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
    let l = context list
    List.rev l

let rec polishToString list =
    match list with
    | [] -> ""
    | x :: list ->
        match x with
        | Num x      ->  (string x)+ "\n"+ (polishToString list)   
        | Var x      ->  x + "\n"+ (polishToString list)
        | Op x       ->  (string x) + "\n"+ (polishToString list)
        | _          ->   failwith "Error"
let printPolish (output : string) list =
    write output ( polishToString list)

[<TestCase ("0", Result = "0\n")>]
[<TestCase ("13", Result = "13\n")>]
[<TestCase ("(-13)", Result = "-13\n")>]
[<TestCase ("1 + 2", Result = "1\n2\n+\n")>]
[<TestCase ("34 -45 +12", Result = "34\n45\n-\n12\n+\n")>]
[<TestCase ("((-34) + 24) * (23 - 13)", Result = "-34\n24\n+\n23\n13\n-\n*\n")>]
[<TestCase ("120 % 11 + 5 ^ (-1)", Result = "120\n11\n%\n5\n-1\n^\n+\n")>]
[<TestCase ("5 ^ (-1)^ (-2)", Result = "5\n-1\n-2\n^\n^\n")>]
let calculate str = 
    str |> scan |> toPolish |> polishToString

[<EntryPoint>]
let main argv = 
    read ("input.txt")|> scan |> toPolish |> printPolish "output.txt"
    0 
