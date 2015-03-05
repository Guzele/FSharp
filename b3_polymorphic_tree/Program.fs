﻿(* Polymorphic tree - second week homework
   Author Guzel Garifullina 171
   type Tree
   Estimated time 15 minutes
   real time      5  minutes
   map
   Estimated time 15 minutes
   real time      1 hour 
   fold
   Estimated time 30 minutes
   real time      30 minutes + 1 hour
   sum
   Estimated time 30 minutes
   real time      1 hour  *)


type Tree<'A> = Nil | Node of Tree<'A> * 'A * Tree<'A>

let rec map f t =
    match t with
    | Nil -> Nil
    | Node (left, value, right) ->
        Node ( (map f left), (f value), (map f right) )

let rec fold f acc t =
    match t with
    | Nil -> acc 
    | Node (left, value, right) ->
        let left_val = fold f (f acc (value)) left
        fold f left_val right

let rec fold_opt f acc t =
    match t with
    | Nil -> f acc None
    | Node (left, value, right) ->
        let left_val = fold_opt f (f acc (Some value)) left
        fold_opt f left_val right

let inline sum_tree t = 
    let inline sum_opt opt y =
        match opt with
        | None   -> Some y
        | Some x -> Some ( x + y )
    fold (sum_opt) None t

(* the same but with option fold
let inline sum_tree_opt t = 
    let inline sum_opt opt y =
        match opt, y with
        | None, None      -> None
        | None, _         -> y
        | _, None         -> opt
        | Some x, Some y  -> Some ( x + y )
    fold_opt (sum_opt) None t *)




[<EntryPoint>]
let main argv = 

    let tree_int = Node (Node (Nil,1,Node (Nil,2,Nil)),3,Node (Node (Nil,4,Nil),7,Nil))
    printfn "Tree of int is %A" tree_int
    printfn "Add 1 to all elem %A" (map  (fun s -> s + 1) tree_int )

    let tree_float = Node (Node (Nil,1.0 ,Node (Nil,2.0 ,Nil)),3.0 ,Node (Node (Nil,4.0,Nil),7.0,Nil))
    printfn "Tree of float is %A" tree_float
    printfn "Multiply to 2 all elem %A" (map  (fun s -> s * 2.0) tree_float )

    printfn "Result of sum tree of int is %A" (sum_tree tree_int)
    printfn "Result of sum tree of float is %A" (sum_tree tree_float)
    printfn "Empty tree sum is %O" (sum_tree Nil)
    0 
