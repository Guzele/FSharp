(* Numbers of Peano - first week homework
   Author Guzel Garifullina *)

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

let peanoToInt p =
    let  rec count p num = 
        match p with
        | Zero -> num
        | S p  -> count p num + 1
    count p 0

let mult a b =
    let rec sum_n_times res a b =
        match b with
        | Zero -> res
        | S b  ->  sum_n_times ( plus res a ) a b   
    match a, b with
    | Zero, _   -> Zero
    |  _ , Zero -> Zero
    | a , S b -> sum_n_times a a b

let power a b = 
    let rec mult_n_times res a b =
        match b with
        | Zero -> res
        | S b  ->  mult_n_times ( mult res a ) a b      
    match a, b with
    | Zero , _ -> Zero
    | _ , Zero -> S Zero
    | a , S b  -> mult_n_times a a b

[<EntryPoint>]
let main args = 
    let a  =  ( S ( S ( Zero ) ) ) 
    let b  =  S (a)
    let c  =  power b a
    printf  "%A\n" ( peanoToInt c )
    0