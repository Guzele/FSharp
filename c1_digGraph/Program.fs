(* OrgGraph - third week homework
   Author Guzel Garifullina 171
   Interface and realization
   Estimated time 1 hour
   real time      5 hours
 *)


type IGraph<'A> = 
    interface
        abstract length   : int
        ///only close neighbours
        abstract isWay    : int-> int -> bool
    end 

type ArrayGraph<'A> (arr: bool [,]) =
    class
        let n = Array2D.length1 arr 
        interface IGraph<'A> with
            member this.length = n
            member this.isWay i j = arr.[i,j]
    end

type ListGraph<'A> (arr : int list []) =
    class
        let n = arr.Length 

        interface IGraph<'A> with
            member this.length = n
            member this.isWay i j  =
                let rec in_list i l  =
                    match l with
                    | []     -> false
                    | x :: l -> if x = i then true
                                else in_list i l
                in_list j arr.[i]
                         
    end

let connectedWith (graph :IGraph<'A>, i) =
    let n = graph.length
    let array  = Array.create n false
    array.[i] <- true
    let mutable list = []
 
    let rec explore i =
        for j = 0 to (n - 1) do
            if (graph.isWay i j) && ( array.[j] = false)
            then array.[j]<- true 
                 explore j
            else () 
             
    explore i
    array.[i] <- false

    for j = (n - 1) downto 0 do
        if (array.[j] = true) then list <- j :: list
    list

let reachableFrom (graph :IGraph<'A>, i) =
    let n = graph.length
    let array  = Array.create n false
    array.[i] <- true
    let mutable list = []
 
    let rec search i =
        for j = 0 to (n - 1) do
            if j  = i then ()
            elif (graph.isWay j i) && (array.[j] = false)
                       then array.[j] <- true
                            search j
            else ()
    search i
    array.[i] <- false
    for j i = (n - 1) downto 0 do
        if (array.[j] = true) then list <- j :: list
    list


type ILabeledGraph<'A, 'B> =
    interface
        inherit IGraph<'B>
        abstract isWayLab      : 'A -> 'A -> bool
        abstract matchLabToNum : 'A -> 'B
        abstract matchNumToLab : 'B -> 'A
    end

[<EntryPoint>]
let main argv = 
    let array = Array2D.create 5 5 false 
    let ex x y = Array2D.set array x y true 
    ex 0 1
    ex 0 2
    ex 1 4
    ex 2 4
    ex 3 0
    ex 3 2
    ex 4 2 

    let list = [| [ 1; 2]; [4]; [4]; [0;2]; [2] |]
    printfn " The initial graph of array is %A\n" array
    printfn " The initial graph of list is %A\n" list

    let graph_a1 = new ArrayGraph<int>(array) 
    let graph_a = graph_a1  :> IGraph<int> 

    let graph_l1 = new ListGraph<int>(list) 
    let graph_l = graph_l1  :> IGraph<int> 
    printfn " There is a direct way from 2 to 4 %b %b" (graph_a.isWay 2 4)  (graph_l.isWay 2 4)   
    printfn " 0 is connected with %A, %A" (connectedWith (graph_a, 0))  (connectedWith (graph_l, 0))

    printfn "0 is reachable from %A, %A" (reachableFrom (graph_a, 0))  (reachableFrom (graph_l, 0))

    0 


