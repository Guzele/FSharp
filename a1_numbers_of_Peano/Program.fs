(* Numbers of Peano - first week homework
   Author Guzel Garifullina 
   Estimated time  1 hour
   Real time       3 hours*)

type Peano = Zero | S of Peano

let suc (p : Peano ) = S p 

let minus1 (p : Peano) = 
    match p with 
    | Zero -> Zero
    | S p  -> p

let rec plus a b =
    match a with 
    | Zero -> b
    | S a -> S ( plus a b )

let rec minus a b =
    match a, b with
    | Zero, _   -> Zero
    | S a, Zero -> S a
    | S a, S b  -> minus a b

let rec peanoToInt p =
    match p with
    | Zero -> 0
    | S p  -> 1 + peanoToInt p

let rec mult a b =
    match a, b with
    | Zero, _   -> Zero
    |  _ , Zero -> Zero
    | a , S b -> plus a  (mult a b)

let rec power a b = 
    match a, b with
    | Zero , _ -> Zero
    | _ , Zero -> S Zero
    | a , S b  -> mult a (power a b)

[<EntryPoint>]
let main args = 
    let a  =  ( S ( S ( Zero ) ) ) 
    let b  =  S (a)
    let c  =  mult a b
    printf  "%d\n" ( peanoToInt c )
    let d  =  power a b
    printf  "%d\n" ( peanoToInt d )
    0    