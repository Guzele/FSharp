(* Binary Search Tree - first week homework
   Author Guzel Garifullina 171*)

type Tree = Nil | Node of Tree * int * Tree

let rec add t x =
    match t with
    | Nil -> Node ( Nil, x, Nil )
    | Node (less , num , more) -> 
        if x < num then Node ( add less x, num , more) 
        else  Node (less, num, add more x)

let rec remove_by_value t x  = 
    let rec find_least_elem t  x =
        match t with
        | Nil  -> x
        | Node (less, num, more) ->  find_least_elem less num
    match t with
    | Nil -> Nil
    | Node (less, num, more) ->
        if   x < num then Node ( remove_by_value less x , num , more)
        elif x > num then Node ( less, num , remove_by_value more x )
        // x = num
        elif (less = Nil) && (more = Nil) then Nil
        elif less = Nil then more
        elif more = Nil then less
        else Node ( less, find_least_elem more num ,  remove_by_value more ( find_least_elem more num ) )
// LCR               
let rec lCR_print t =
    match t with
    | Nil  -> printf ""
    | Node (left, num, right) -> 
        lCR_print left
        printf "%d " num
        lCR_print right
    
// LRC
let rec lRC_print t =
    match t with
    | Nil  -> printf ""
    | Node (left, num, right) -> 
        lRC_print left
        lRC_print right
        printf "%d " num

// CLR
let rec cLR_print t =
    match t with
    | Nil  -> printf ""
    | Node (left, num, right) -> 
        printf "%d " num
        cLR_print left
        cLR_print right
         

[<EntryPoint>]
let main argv = 
    let mutable a = Nil
    a <- add a 10
    a <- add a 5
    a <- add a 15
    a <- add a 2
    a <- add a 7
    a <- add a 1
    a <- add a 3
    a <- add a 4
    a <- add a 6
    a <- add a 9
    a <- add a 8
    a <- add a 12
    a <- add a 18
    printf "LCR is " ; lCR_print a; printf "\n"
    printf "LRC is " ; lRC_print a; printf "\n"
    printf "CLR is " ; cLR_print a; printf "\n"
    a <- remove_by_value a 7
    printf "After removal\n"
    printf "LCR is " ; lCR_print a; printf "\n"
    printf "LRC is " ; lRC_print a; printf "\n"
    printf "CLR is " ; cLR_print a; printf "\n"
    0 