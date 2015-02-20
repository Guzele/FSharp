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
let rec lCR_to_string t =
    match t with
    | Nil  -> "Tree is empty"
    | Node (left, num, right) -> 
       let mutable sl = "" 
       if (left <> Nil) then  sl <-( lCR_to_string left  + " ") 
       let mutable sr =  ""
       if right <> Nil then sr <- " " + lCR_to_string right 
       sl + num.ToString () + sr
     
let rec lCR_print t =  t |> lCR_to_string
                         |> printf "%s\n" 

// LRC
let rec lRC_to_string t =
    match t with
    | Nil  -> "Tree is empty"
    | Node (left, num, right) -> 
       let mutable sl = "" 
       if (left <> Nil) then  sl <-( lRC_to_string left  + " ") 
       let mutable sr =  ""
       if right <> Nil then sr <- lRC_to_string right + " "
       sl + sr + num.ToString ()
     
let rec lRC_print t =  t |> lRC_to_string
                         |> printf "%s\n" 

// CLR
let rec cLR_to_string t =
    match t with
    | Nil  -> "Tree is empty"
    | Node (left, num, right) -> 
       let mutable sl = "" 
       if (left <> Nil) then  sl <- " " + cLR_to_string left   
       let mutable sr =  ""
       if right <> Nil then sr <-  " " + cLR_to_string right
       num.ToString () + sl + sr 
     
let rec cLR_print t =  t |> cLR_to_string
                         |> printf "%s\n" 

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
    a <- remove_by_value a 4
    lCR_print a
    0 