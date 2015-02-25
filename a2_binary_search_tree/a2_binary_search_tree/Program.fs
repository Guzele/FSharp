(* Binary Search Tree - first week homework
   Author Guzel Garifullina 171
   Estimated time 1 hour
   real time      2 hours *)

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
    let mutable tree = Node (Node (Nil,1,Node (Nil,2,Nil)),3,Node (Node (Nil,4,Nil),7,Nil))
    printf "First example \n %A \n" tree
    printf " LCR is " ; lCR_print tree; printf "\n"
    printf " LRC is " ; lRC_print tree; printf "\n"
    printf " CLR is " ; cLR_print tree; printf "\n"

    tree <- remove_by_value tree 3
    printf "After removal 3\n %A \n" tree
    tree <- add tree  5
    printf "After removal 3 and addition 5\n %A \n" tree
    tree <- add tree  8
    tree <- add tree  6
    tree <- add tree  0
    printf "Add 8, 6, 0\n %A \n" tree
    tree <- remove_by_value tree 7
    printf "Delete 7\n %A \n" tree

    0


 (*   tree <- Node ( Node (Node (Node (Nil,1,Nil),2,Node (Nil,3,Node (Nil,4,Nil))),5, Node (Node (Nil,6,Nil),7,Node (Node (Nil,8,Nil),9,Nil))),10, Node (Node (Nil,12,Nil),15,Node (Nil,18,Nil)))
    printf "Second example \n %A \n" tree
    printf " LCR is " ; lCR_print tree; printf "\n"
    printf " LRC is " ; lRC_print tree; printf "\n"
    printf " CLR is " ; cLR_print tree; printf "\n"

    tree <- remove_by_value tree 7
    printf "After removal 7\n %A \n" tree

    tree <- add tree  11
    printf "After removal 7 and addition 11 \n %A \n" tree *)