(* OrgGraph - third week homework
   Author Guzel Garifullina 171
   Tasting
   Estimated time 1 hour
   real time      1 hour
 *)

open NUnit.Framework

type IGraph = 
    interface
        abstract length   : int
        ///only close neighbours
        abstract isWay    : int-> int -> bool
    end 

type ArrayGraph (arr: bool [,]) =
    class
        let n = Array2D.length1 arr 
        interface IGraph with
            member this.length = n
            member this.isWay i j = arr.[i,j]
    end

[<Test>]
let ``01)Length of empty array graph is 0``() =
    let array = Array2D.create 0 0 true 
    let graph = new ArrayGraph(array) :> IGraph 
    Assert.AreEqual (graph.length, 0)

[<TestCase (4, Result = 4)>]
[<TestCase (10, Result = 10)>]
let ``02)Length of non-empty array graph is ``n =
    let array = Array2D.create n n true 
    let graph = new ArrayGraph(array) :> IGraph 
    graph.length

[<Test>]
let ``03)Isway of not-connected array graph is false``() =
    let array = Array2D.create 5 5 false 
    let graph = new ArrayGraph(array) :> IGraph 
    Assert.AreEqual (graph.isWay 4 4, false)
[<Test>]
let ``04)Isway of fully-connected array graph is true``() =
    let array = Array2D.create 5 5 true 
    let graph = new ArrayGraph(array) :> IGraph 
    Assert.AreEqual (graph.isWay 4 2, true)
[<Test>]
let ``05)Isway of only-connected (3,6) array graph of (3,6) is true``() =
    let array = Array2D.create 10 10 false 
    let graph = new ArrayGraph(array) :> IGraph 
    Array2D.set array 3 6 true
    Assert.AreEqual (graph.isWay 3 6, true)
[<Test>]
let ``06)Isway of only-connected (3,6) array graph of (6,3)is false``() =
    let array = Array2D.create 10 10 false 
    let graph = new ArrayGraph(array) :> IGraph 
    Array2D.set array 3 6 true
    Assert.AreEqual (graph.isWay 6 3, false)
[<Test>]
let ``07)Isway of only-not-connected (2,1) array graph of (2,1)is false``() =
    let array = Array2D.create 5 5 true 
    let graph = new ArrayGraph(array) :> IGraph 
    Array2D.set array 2 1 false
    Assert.AreEqual (graph.isWay 2 1, false)

type ListGraph (arr : int list []) =
    class
        let n = arr.Length 

        interface IGraph with
            member this.length = n
            member this.isWay i j  =
                let rec in_list i l  =
                    match l with
                    | []     -> false
                    | x :: l -> if x = i then true
                                else in_list i l
                in_list j arr.[i]                   
    end
[<Test>]
let ``08 Length of empty list graph is 0``() =
    let list = [||] 
    let graph = new ListGraph(list) :> IGraph 
    Assert.AreEqual (graph.length, 0)
[<Test>]
let ``09 Length of 4 elem list graph is 4``() =
    let list = [| [1; 3]; [1]; []; [2] |] 
    let graph = new ListGraph(list) :> IGraph 
    Assert.AreEqual (graph.length, 4)

[<Test>]
let ``10)Isway of not-connected list graph is false``() =
    let list = [| []; []; []; []|] 
    let graph = new ListGraph(list) :> IGraph  
    Assert.AreEqual (graph.isWay 3 0, false)
[<Test>]
let ``11)Isway of fully-connected list graph is true``() =
    let list = [| [0; 1; 2]; [0; 1; 2]; [0; 1; 2]|] 
    let graph = new ListGraph(list) :> IGraph 
    Assert.AreEqual (graph.isWay 0 0, true)
[<Test>]
let ``12)Isway of only-connected (2,3) array graph of (2,3) is true``() =
    let list = [| []; []; [3]; []|]
    let graph = new ListGraph(list) :> IGraph
    Assert.AreEqual (graph.isWay 2 3, true)
[<Test>]
let ``13)Isway of only-connected (2,3) array graph of (3,2)is false``() =
    let list = [| []; []; [3]; []|] 
    let graph = new ListGraph(list) :> IGraph 
    Assert.AreEqual (graph.isWay 3 2, false)
[<Test>]
let ``14)Isway of only-not-connected (2,3) array graph of (2,3)is false``() =
    let list = [| [0; 1; 2; 3]; [0; 1; 2; 3]; [0; 1; 2]; [0; 1; 2; 3]|] 
    let graph = new ListGraph(list) :> IGraph 
    Assert.AreEqual (graph.isWay 2 3, false)

let connectedWith (graph :IGraph, i) =
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

[<Test>]
let ``14)connected with of not-connected array graph is []``() =
    let array = Array2D.create 5 5 false 
    let graph = new ArrayGraph(array) :> IGraph
    Assert.AreEqual (connectedWith (graph, 3), [])

let pop_ex () =
    let array = Array2D.create 5 5 false 
    let ex x y = Array2D.set array x y true 
    ex 0 1
    ex 0 2
    ex 2 4
    ex 3 0
    ex 3 2
    ex 4 2
    array
 
[<TestCase (0, Result = "1; 2; 4; ")>]
[<TestCase (1, Result = "")>]
[<TestCase (2, Result = "4; ")>]
[<TestCase (3, Result = "0; 1; 2; 4; ")>]
[<TestCase (4, Result = "2; ")>]
let ``15)connected with of random array graph is ``i =
    let array = pop_ex () 
    let graph = new ArrayGraph(array) :> IGraph
    let list = connectedWith (graph, i)
    List.fold (fun acc x -> acc + (string x) + "; ") "" list

let list_a = [| [ 1; 2]; []; [4]; [0;2]; [2] |]
[<TestCase (0, Result = "1; 2; 4; ")>]
[<TestCase (1, Result = "")>]
[<TestCase (2, Result = "4; ")>]
[<TestCase (3, Result = "0; 1; 2; 4; ")>]
[<TestCase (4, Result = "2; ")>]
let ``16)connected with of random list graph is ``i = 
    let graph = new ListGraph(list_a) :> IGraph
    let list = connectedWith (graph, i)
    List.fold (fun acc x -> acc + (string x) + "; ") "" list

let reachableFrom (graph :IGraph, i) =
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

[<TestCase (0, Result = "3; ")>]
[<TestCase (1, Result = "0; 3; ")>]
[<TestCase (2, Result = "0; 3; 4; ")>]
[<TestCase (3, Result = "")>]
[<TestCase (4, Result = "0; 2; 3; ")>]
let ``17)reachableFrom of random array graph is ``i =
    let array = pop_ex () 
    let graph = new ArrayGraph(array) :> IGraph
    let list = reachableFrom (graph, i)
    List.fold (fun acc x -> acc + (string x) + "; ") "" list

[<TestCase (0, Result = "1; 2; 4; ")>]
[<TestCase (1, Result = "")>]
[<TestCase (2, Result = "4; ")>]
[<TestCase (3, Result = "0; 1; 2; 4; ")>]
[<TestCase (4, Result = "2; ")>]
let ``18)connected with of random list graph is ``i = 
    let graph = new ListGraph(list_a) :> IGraph
    let list = connectedWith (graph, i)
    List.fold (fun acc x -> acc + (string x) + "; ") "" list

type ILabeledGraph<'A> =
    interface
        inherit IGraph
        abstract isWayLab      : 'A -> 'A -> bool
        abstract matchLabToNum : 'A -> int
        abstract matchNumToLab : int -> 'A
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

    let graph_a1 = new ArrayGraph(array) 
    let graph_a = graph_a1  :> IGraph 

    let graph_l1 = new ListGraph(list) 
    let graph_l = graph_l1  :> IGraph 
    printfn " There is a direct way from 2 to 4 \n array graph output - %b \n list graph output - %b" 
            (graph_a.isWay 2 4)  (graph_l.isWay 2 4)   
    printfn "0 is connected with \n array graph output - %A, \n list graph output - %A"
         (connectedWith (graph_a, 0))  (connectedWith (graph_l, 0))

    printfn "0 is reachable from \n array graph output - %A, \n list graph output - %A"
         (reachableFrom (graph_a, 0))  (reachableFrom (graph_l, 0))

    0 
