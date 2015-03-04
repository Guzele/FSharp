(* Polymorphic tree - second week homework
   Author Guzel Garifullina 171
   type Tree
   Estimated time 15 minutes
   real time      5 minute  *)

type Tree<'A> = Nil | Node of Tree<'A> * 'A * Tree<'A>

[<EntryPoint>]
let main argv = 
    let a = (Nil, "e", Nil)
    printfn "%A" a
    0 

