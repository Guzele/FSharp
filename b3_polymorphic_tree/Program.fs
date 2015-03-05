(* Polymorphic tree - second week homework
   Author Guzel Garifullina 171
   type Tree
   Estimated time 15 minutes
   real time      1 hour  *)

type Tree<'A> = Nil | Node of Tree<'A> * 'A * Tree<'A>

let rec map f t =
    match t with
    | Nil -> Nil
    | Node (left, value, right) ->
        Node ( (map f left), (f value), (map f right) )

[<EntryPoint>]
let main argv = 
    let tree_int = Node (Node (Nil,1,Node (Nil,2,Nil)),3,Node (Node (Nil,4,Nil),7,Nil))
    printfn "Tree of int is %A" tree_int
    printfn "Add 1 to all elem %A" (map  (fun s -> s + 1) tree_int )

    let tree_float = Node (Node (Nil,1.0 ,Node (Nil,2.0 ,Nil)),3.0 ,Node (Node (Nil,4.0,Nil),7.0,Nil))
    printfn "Tree of float is %A" tree_float
    printfn "Multiply to 2 all elem %A" (map  (fun s -> s * 2.0) tree_float )
    0 

