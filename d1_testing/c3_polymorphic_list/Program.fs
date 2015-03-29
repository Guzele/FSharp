(* Polymorphic list - third week homework
   Author Guzel Garifullina 171
   testing
   Estimated time 1 hour
   real time      2 hours
 *)
open NUnit.Framework

type List_ADT<'T when 'T: equality> = Nil | Cons of 'T * List_ADT<'T>

type IList<'A> =
    interface
       //abstract id : '_A with get, set
        abstract Length : int with get,set
        abstract InsTop : 'A   -> unit
        abstract InsEnd : 'A   -> unit
        abstract InsNum : 'A   -> int   -> bool
        abstract DelTop : unit -> bool
        abstract DelEnd : unit -> bool 
        abstract DelNum : int  -> bool
        abstract Find   : ('A  -> bool) -> Option<'A> 
        abstract Concat : IList<'A>     -> unit 
        abstract Head   : unit -> Option<'A>
        abstract Last   : unit -> Option<'A> 
        abstract IsEmpty: unit -> bool
        abstract Printn : unit -> unit
    end

type List<'A when 'A: equality> () =
    class
        let mutable id = Nil
        let rec length id =
            match id with
            | Nil -> 0
            | Cons (x, id) -> 1 + (length id)
        member this.Idx  
                with  get () = id
                and set (value) = id <- value                               
                                  do (this :> IList<'A>).Length <- length id              
        interface IList<'A> with
            member val Length = 0 with get,set
            member this.InsTop x  = 
                this.Idx <- Cons (x, this.Idx) 
            member this.InsEnd x =
                let rec push_end l x =
                    match l with
                    | Nil            -> Cons ( x, Nil)
                    | Cons (head, l) -> Cons ( head, (push_end l x) )
                this.Idx <- push_end this.Idx x
            member this.InsNum x i =
                let rec push_num l x i =
                    match l, i with 
                    | _, 0              ->  Cons (x,l)
                    | Nil, _            -> failwith "Error, not enoght elements in list"
                    | Cons (head, l), _ -> Cons (head, push_num l x (i-1))
                if (this :> IList<'A>).Length < i 
                then false
                elif (this :> IList<'A>).Length = i
                then (this :> IList<'A>).InsEnd x
                     true
                else this.Idx <- push_num this.Idx x i
                     true
            member this.DelTop () =
                match this.Idx with
                | Nil -> false
                | Cons (head, l) -> 
                    this.Idx <- l
                    true
            member this.DelEnd () = 
                let rec Del_end l =
                    match l with
                    | Nil           -> failwith "Error, not enoght elements in list"
                    | Cons (x, Nil) -> Nil
                    | Cons (x, l)   -> Cons (x, Del_end l)
                match this.Idx with
                | Nil -> false
                | _   ->
                    this.Idx <- Del_end this.Idx
                    true
            member this.DelNum i = 
                let rec Del_num l i =
                    match l, i with
                    | Nil, _ -> failwith "Error, not enoght elements in list"
                    | Cons (x, l), 0 -> l
                    | Cons (x, l), _ -> Cons (x, Del_num l (i-1))
                let n = (this:> IList<'A>).Length
                if n <= i
                then false
                else this.Idx <- Del_num this.Idx i
                     true
            member this.Find func =
               let rec find_elem f l = 
                   match l with
                   | Nil -> None
                   | Cons (x, l) -> 
                       if f x then Some x
                       else find_elem f l
               find_elem func this.Idx
            member this.Head () =
                match this.Idx with
                | Nil -> None
                | Cons (x, _) -> Some x
            member this.Last () = 
                let rec last l=
                    match l with
                    | Nil   -> None
                    | Cons ( x , Nil) -> Some x
                    | Cons (x, l)     -> last l
                last this.Idx
            member this.IsEmpty() = (this.Idx = Nil)
            member this.Concat l = 
                let temp= new List<'A> ()
                temp.Idx <- this.Idx
                while l.IsEmpty() = false do
                    let x = (l.Head ()).Value
                    (temp:> IList<'A>).InsEnd x
                    ignore (l.DelTop () )
                this.Idx <- temp.Idx
            member this.Printn () = 
                printfn "%A\n" this.Idx
    end
[<Test>]
let ``01)Length of empty ATD list is 0``() =
    let list = new List<int> ()
    list.Idx <- Nil 
    Assert.AreEqual ((list:> IList<int>).Length, 0) 
[<Test>]
let ``02)Length of 1 elem ATD list is 1``() =
    let list = new List<int> ()
    list.Idx <- Cons ( 1, Nil)
    Assert.AreEqual ((list:> IList<int>).Length, 1) 
[<Test>]
let ``03)Length of 4 elem ATD list is 1``() =
    let list = new List<int> ()
    list.Idx <- Cons(7, Cons (2, Cons (14, Cons ( 3, Nil) ) ) )
    Assert.AreEqual ((list:> IList<int>).Length, 4)
[<Test>]
let ``04)InsTop 5 to empty ATD list is Cons(5, Nil)``() =
    let list = new List<int> ()
    list.Idx <- Nil
    (list:> IList<int>).InsTop 5
    Assert.AreEqual (list.Idx, Cons(5, Nil))
[<Test>]
let ``05)InsTop 5 then 4 to empty ATD list is  Cons(4, Cons(5, Nil))``() =
    let list = new List<int> ()
    list.Idx <- Nil
    (list:> IList<int>).InsTop 5
    (list:> IList<int>).InsTop 4
    Assert.AreEqual (list.Idx, Cons (4, Cons(5, Nil)))
[<Test>]
let ``06)After InsTop 5 then 4 to empty ATD list length is 2``() =
    let list = new List<int> ()
    list.Idx <- Nil
    (list:> IList<int>).InsTop 5
    (list:> IList<int>).InsTop 4
    Assert.AreEqual ((list:> IList<int>).Length, 2)
[<Test>]
let ``07)InsEnd 5 to empty ATD list is Cons(5, Nil)``() =
    let list = new List<int> ()
    list.Idx <- Nil
    (list:> IList<int>).InsEnd 5
    Assert.AreEqual (list.Idx, Cons(5, Nil))
[<Test>]
let ``08)InsEnd 5 then 4 to empty ATD list is  Cons(5, Cons(4, Nil))``() =
    let list = new List<int> ()
    list.Idx <- Nil
    (list:> IList<int>).InsEnd 5
    (list:> IList<int>).InsEnd 4
    Assert.AreEqual (list.Idx, Cons (5, Cons(4, Nil)))
[<Test>]
let ``09)After InsEnd 5 then 4 to empty ATD list length is 2``() =
    let list = new List<int> ()
    list.Idx <- Nil
    (list:> IList<int>).InsEnd 5
    (list:> IList<int>).InsEnd 4
    Assert.AreEqual ((list:> IList<int>).Length, 2)

[<Test>]
let ``10)InsNum  5 to 0-s place to empty ATD list is true``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).InsNum 5 0, true)
[<Test>]
let ``11)InsNum  5 to 0-s place to empty ATD list is Cons(5, Nil)``() =
    let list = new List<int> ()
    list.Idx <- Nil
    ignore ((list:> IList<int>).InsNum 5 0 )
    Assert.AreEqual (list.Idx, Cons (5,Nil))
[<Test>]
let ``12)InsNum  5 to 3-s place to empty ATD list is false``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).InsNum 5 3 , false)
[<Test>]
let ``13)InsNum  5 to 3-s place to Cons (5,Nil) ATD list is Cons (5,Nil)``() =
    let list = new List<int> ()
    list.Idx <- Cons (5,Nil)
    ignore ((list:> IList<int>).InsNum 5 3 )
    Assert.AreEqual (list.Idx , Cons (5,Nil))
[<Test>]
let ``14)InsNum  0 to 0-s place to Cons (5,Nil) ATD list is Cons( 0, Cons (5,Nil))``() =
    let list = new List<int> ()
    list.Idx <- Cons (5,Nil)
    ignore ((list:> IList<int>).InsNum 0 0 )
    Assert.AreEqual (list.Idx , Cons( 0, Cons (5,Nil)))
[<Test>]
let ``15)InsNum  4 to 2-s place to Cons( 0, Cons (5,Nil)) ATD list is Cons( 0, Cons (5,Cons(4,Nil)))``() =
    let list = new List<int> ()
    list.Idx <- Cons( 0, Cons (5,Nil))
    ignore ((list:> IList<int>).InsNum 4 2 )
    Assert.AreEqual (list.Idx , Cons( 0, Cons (5,Cons(4,Nil))))
[<Test>]
let ``16)InsNum 5 to 0-s, 6 to 0-s,7 to 1-st to Nil ATD list is Cons( 6, Cons( 7, Cons (5,Nil)))``() =
    let list = new List<int> ()
    list.Idx <- Nil
    ignore ((list:> IList<int>).InsNum 5 0 )
    ignore ((list:> IList<int>).InsNum 6 0 )
    ignore ((list:> IList<int>).InsNum 7 1 )
    Assert.AreEqual (list.Idx , Cons( 6, Cons( 7, Cons (5,Nil))))
[<Test>]
let ``17)InsNum 5 to 0-s, 6 to 0-s,7 to 1-st to Nil ATD list length is``() =
    let list = new List<int> ()
    list.Idx <- Nil
    ignore ((list:> IList<int>).InsNum 5 0 )
    ignore ((list:> IList<int>).InsNum 6 0 )
    ignore ((list:> IList<int>).InsNum 7 1 )
    Assert.AreEqual ((list:> IList<int>).Length , 3)
[<Test>]
let ``18)DelTop to empty ATD list is false``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).DelTop () , false)
[<Test>]
let ``19)DelTop to Cons (5, Nil) ATD list is true``() = 
    let list = new List<int> ()
    list.Idx <- Cons (5, Nil)
    Assert.AreEqual ((list:> IList<int>).DelTop () , true)
[<Test>]
let ``20)DelTop to Cons (5, Nil) ATD list is Nil``() = 
    let list = new List<int> ()
    list.Idx <- Cons (5, Nil)
    ignore ((list:> IList<int>).DelTop ())
    if list.Idx = Nil
        then Assert.AreEqual (Nil, Nil)
        else Assert.Fail ()
[<Test>]
let ``21)DelTop to Cons( 6,(Cons (5, Nil))) ATD list is Cons (5, Nil)``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelTop ())
    Assert.AreEqual (list.Idx , Cons (5, Nil))
[<Test>]
let ``22)DelTop to Cons( 6,(Cons (5, Nil))) ATD list length is 1``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelTop ())
    Assert.AreEqual ((list:> IList<int>).Length , 1)
[<Test>]
let ``23)DelEnd to empty ATD list is false``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).DelEnd () , false)
[<Test>]
let ``24)DelEnd to Cons (5, Nil) ATD list is true``() = 
    let list = new List<int> ()
    list.Idx <- Cons (5, Nil)
    Assert.AreEqual ((list:> IList<int>).DelEnd () , true)
[<Test>]
let ``25)DelEnd to Cons (5, Nil) ATD list is Nil``() = 
    let list = new List<int> ()
    list.Idx <- Cons (5, Nil)
    ignore ((list:> IList<int>).DelTop ())
    if list.Idx = Nil
        then Assert.AreEqual (Nil, Nil)
        else Assert.Fail ()
[<Test>]
let ``26)DelEnd to Cons( 6,(Cons (5, Nil))) ATD list is Cons (6, Nil)``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelEnd ())
    Assert.AreEqual (list.Idx , Cons (6, Nil))
[<Test>]
let ``27)DelEnd to Cons( 6,(Cons (5, Nil))) ATD list length is 1``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelEnd ())
    Assert.AreEqual ((list:> IList<int>).Length , 1)
[<Test>]
let ``28)DelNum to empty ATD list is false``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).DelNum 0 , false)
[<Test>]
let ``29)DelNum 0 to Cons (5, Nil) ATD list is true``() = 
    let list = new List<int> ()
    list.Idx <- Cons (5, Nil)
    Assert.AreEqual ((list:> IList<int>).DelNum 0, true)
[<Test>]
let ``30)DelNum 0 to Cons( 6,(Cons (5, Nil))) ATD list is Cons (5, Nil)``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelNum 0)
    Assert.AreEqual (list.Idx , Cons (5, Nil))
[<Test>]
let ``31)DelNum 1 to Cons( 6,(Cons (5, Nil))) ATD list is Cons (6, Nil)``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelNum 1)
    Assert.AreEqual (list.Idx , Cons (6, Nil))
[<Test>]
let ``32)DelNum 0  to Cons( 6,(Cons (5, Nil))) ATD list length is 1``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelNum 0)
    Assert.AreEqual ((list:> IList<int>).Length , 1)
[<Test>]
let ``33)DelNum 30  to Cons( 6,(Cons (5, Nil))) ATD list is Cons( 6,(Cons (5, Nil)))``() = 
    let list = new List<int> ()
    list.Idx <- Cons( 6,(Cons (5, Nil)))
    ignore ((list:> IList<int>).DelNum 30)
    Assert.AreEqual (list.Idx , Cons( 6,(Cons (5, Nil))))
[<Test>]
let ``34)Find even element in empty ATD list is None``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).Find (fun x -> x % 2 = 0), None)
[<Test>]
let ``35)Find even element in odd ATD list is None``() =
    let list = new List<int> ()
    list.Idx <- Cons(1, Cons (9, Cons(3, Cons(9, Nil))))
    Assert.AreEqual ((list:> IList<int>).Find (fun x -> x % 2 = 0), None)
[<Test>]
let ``36)Find even element in Cons(1, Cons (60, Cons(3, Cons(9, Nil)))) ATD list is Some 60)``() =
    let list = new List<int> ()
    list.Idx <- Cons(1, Cons (60, Cons(3, Cons(9, Nil))))
    Assert.AreEqual ((list:> IList<int>).Find (fun x -> x % 2 = 0), Some 60)
[<Test>]
let ``37)Find even element in Cons(4, Cons (9, Cons(3, Cons(18, Nil)))) ATD list is Some 4``() =
    let list = new List<int> ()
    list.Idx <- Cons(4, Cons (9, Cons(3, Cons(18, Nil))))
    Assert.AreEqual ((list:> IList<int>).Find (fun x -> x % 2 = 0), Some 4)
[<Test>]
let ``38)Find even element in Cons(1, Cons (9, Cons(3, Cons(4, Nil)))) ATD list is Some 18``() =
    let list = new List<int> ()
    list.Idx <- Cons(1, Cons (9, Cons(3, Cons(18, Nil))))
    Assert.AreEqual ((list:> IList<int>).Find (fun x -> x % 2 = 0), Some 18)
[<Test>]
let ``39)Concat 2 Nil ATD lists is Nil``() =
    let list = new List<int> ()
    list.Idx <- Nil
    let list1 = new List<int> ()
    list1.Idx <- Nil
    (list:> IList<int>).Concat list1
    if list.Idx = Nil
        then Assert.AreEqual (Nil, Nil)
        else Assert.Fail ()
[<Test>]
let ``40)Concat Cons(2, Cons (1, Nil)) with Nil ATD list is Cons(2, Cons (1, Nil)))``() =
    let list = new List<int> ()
    list.Idx <- Cons(2, Cons (1, Nil))
    let list1 = new List<int> ()
    list1.Idx <- Nil
    (list:> IList<int>).Concat list1
    Assert.AreEqual (list.Idx,Cons(2, Cons (1, Nil)))
[<Test>]
let ``41)Concat Nil with Cons(2, Cons (1, Nil))) ATD list is Cons(2, Cons (1, Nil))``() =
    let list = new List<int> ()
    list.Idx <- Nil
    let list1 = new List<int> ()
    list1.Idx <- Cons(2, Cons (1, Nil))
    (list:> IList<int>).Concat list1
    Assert.AreEqual (list.Idx,Cons(2, Cons (1, Nil)))
[<Test>]
let ``42)Concat Cons(3, Cons (4, Nil)) with Cons(2, Cons (1, Nil))) ATD list is ,Cons(3, Cons (4, Cons (2, Cons (1, Nil))))``() =
    let list = new List<int> ()
    list.Idx <- Cons(3, Cons (4, Nil))
    let list1 = new List<int> ()
    list1.Idx <- Cons(2, Cons (1, Nil))
    (list:> IList<int>).Concat list1
    Assert.AreEqual (list.Idx,Cons(3, Cons (4, Cons (2, Cons (1, Nil)))))
[<Test>]
let ``43)Concat Cons(3, Cons (4, Nil)) with Cons(2, Cons (1, Nil))) ATD list length is 4``() =
    let list = new List<int> ()
    list.Idx <- Cons(3, Cons (4, Nil))
    let list1 = new List<int> ()
    list1.Idx <- Cons(2, Cons (1, Nil))
    (list:> IList<int>).Concat list1
    Assert.AreEqual ((list:> IList<int>).Length, 4)
[<Test>]
let ``44)Head of empty ATD list is None``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).Head(), None)
[<Test>]
let ``45)Head of Cons (4, Nil) ATD list is Some 4``() =
    let list = new List<int> ()
    list.Idx <- Cons (4, Nil)
    Assert.AreEqual ((list:> IList<int>).Head(), Some 4)
[<Test>]
let ``46)Head of Cons(3, Cons (4, Cons (2, Cons (1, Nil)))) ATD list is Some 3``() =
    let list = new List<int> ()
    list.Idx <- Cons(3, Cons (4, Cons (2, Cons (1, Nil))))
    Assert.AreEqual ((list:> IList<int>).Head(), Some 3)
[<Test>]
let ``47)IsEmpty for empty ATD list is true``() =
    let list = new List<int> ()
    list.Idx <- Nil
    Assert.AreEqual ((list:> IList<int>).IsEmpty (), true)
[<Test>]
let ``48)IsEmpty for Cons (4, Nil) ATD list is false``() =
    let list = new List<int> ()
    list.Idx <- Cons (4, Nil)
    Assert.AreEqual ((list:> IList<int>).IsEmpty(), false)

type ArrayList<'A when 'A: equality> () =
    class
        let mutable id = [||]
        member this.Idx  
                with  get () = id
                and set (value) = id <- value                               
                                  do (this :> IList<'A>).Length <- id.Length 
        interface IList<'A> with
            member val Length = 0 with get,set
            member this.InsTop x  = 
                this.Idx <- Array.append [|x|] this.Idx
            member this.InsEnd x = 
                this.Idx <- Array.append this.Idx [|x|]
            member this.InsNum x i =
                let mas = this.Idx
                let n = (this:> IList<'A>).Length
                if  n < i 
                then false
                else let t = Array.append (Array.sub mas 0 i) [|x|] 
                     this.Idx <- Array.append t (Array.sub mas i (n - i ))
                     true
            member this.DelTop () =
                let mas = this.Idx
                let n = (this:> IList<'A>).Length
                match this.Idx with
                | [| |] -> false
                |  _   ->  this.Idx <-  Array.sub mas 1 (n - 1)
                           true
            member this.DelEnd () = 
                let n = (this:> IList<'A>).Length
                match this.Idx with
                | [||] -> false
                |  _   -> this.Idx <- Array.sub this.Idx 0  (n- 1)
                          true
            member this.DelNum i = 
                let mas = this.Idx
                let n =  (this:> IList<'A>).Length
                if  n <= i 
                then false
                else 
                    this.Idx <- Array.append 
                                (Array.sub mas 0 i) (Array.sub mas (i + 1) (n - i - 1))
                    true
            member this.Find func =
                let n = (this:> IList<'A>).Length
                let rec find i n =
                    if i = n 
                    then None
                    elif func (this.Idx.[i])
                    then Some (this.Idx.[i])
                    else find (i + 1) n
                    
                find 0 n
                    
            member this.Head () =
                match this.Idx with
                | [||] -> None
                | _  ->  Some this.Idx.[0]
            member this.Last () = 
                match this.Idx with
                | [||] -> None
                | _  ->  Some this.Idx.[(this:> IList<'A>).Length - 1]
            member this.IsEmpty() = (this.Idx = [||])
            member this.Concat (second: IList<'A> ) = 
                let temp= new ArrayList<'A> ()
                temp.Idx <- this.Idx
                while second.IsEmpty() = false do
                    let x = (second.Head ()).Value
                    (temp:> IList<'A>).InsEnd x
                    ignore ( second.DelTop ())
                this.Idx <- temp.Idx 
            member this.Printn () = 
                printfn "%A\n" this.Idx
    end 
[<Test>]
let ``49)Length of empty array list is 0``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||] 
    Assert.AreEqual ((arr:> IList<int>).Length, 0) 
[<Test>]
let ``50)Length of 1 elem array list is 1``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|21|]
    Assert.AreEqual ((arr:> IList<int>).Length, 1) 
[<Test>]
let ``51)Length of 4 elem array list is 1``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5; 3; 3; 7|]
    Assert.AreEqual ((arr:> IList<int>).Length, 4)
[<Test>]
let ``52)InsTop 5 to empty array list is [|5|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    (arr:> IList<int>).InsTop 5
    Assert.AreEqual (arr.Idx, [|5|])
[<Test>]
let ``53)InsTop 5 then 4 to empty array list is [|4; 5|``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    (arr:> IList<int>).InsTop 5
    (arr:> IList<int>).InsTop 4
    Assert.AreEqual (arr.Idx, [|4; 5|])
[<Test>]
let ``54)After InsTop 5 then 4 to empty array list length is 2``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    (arr:> IList<int>).InsTop 5
    (arr:> IList<int>).InsTop 4
    Assert.AreEqual ((arr:> IList<int>).Length, 2)
[<Test>]
let ``55)InsEnd 5 to empty array list is [|5|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    (arr:> IList<int>).InsEnd 5
    Assert.AreEqual (arr.Idx, [|5|])
[<Test>]
let ``56)InsEnd 5 then 4 to empty array list is  [|5; 4|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    (arr:> IList<int>).InsEnd 5
    (arr:> IList<int>).InsEnd 4
    Assert.AreEqual (arr.Idx, [|5; 4|])
[<Test>]
let ``57)After InsEnd 5 then 4 to empty array list length is 2``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    (arr:> IList<int>).InsEnd 5
    (arr:> IList<int>).InsEnd 4
    Assert.AreEqual ((arr:> IList<int>).Length, 2)
[<Test>]
let ``58)InsNum  5 to 0-s place to empty array list is true``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    Assert.AreEqual ((arr:> IList<int>).InsNum 5 0, true)
[<Test>]
let ``59)InsNum  5 to 0-s place to empty array list is [|5|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <-[||]
    ignore ((arr:> IList<int>).InsNum 5 0 )
    Assert.AreEqual (arr.Idx, [|5|]) 
[<Test>]
let ``60)InsNum  5 to 3-s place to empty array list is false``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    Assert.AreEqual ((arr:> IList<int>).InsNum 5 3 , false)
[<Test>]
let ``61)InsNum  5 to 3-s place to Cons [|5|] array list is Cons [|5|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5|]
    ignore ((arr:> IList<int>).InsNum 5 3 )
    Assert.AreEqual (arr.Idx , [|5|])
[<Test>]
let ``62)InsNum  0 to 0-s place to [|5|] array list is [|0;5|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <-[|5|]
    ignore ((arr:> IList<int>).InsNum 0 0 )
    Assert.AreEqual (arr.Idx , [|0; 5|])
[<Test>]
let ``63)InsNum  4 to 2-s place to [|0; 5|] array list is [|0; 5; 4|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|0; 5|]
    ignore ((arr:> IList<int>).InsNum 4 2 )
    Assert.AreEqual (arr.Idx , [|0; 5; 4|])
[<Test>]
let ``64)InsNum 5 to 0-s, 6 to 0-s,7 to 1-st to [||] array list is [| 6; 7; 5|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    ignore ((arr:> IList<int>).InsNum 5 0 )
    ignore ((arr:> IList<int>).InsNum 6 0 )
    ignore ((arr:> IList<int>).InsNum 7 1 )
    Assert.AreEqual (arr.Idx , [| 6; 7; 5|])
[<Test>]
let ``65)InsNum 5 to 0-s, 6 to 0-s,7 to 1-st to [||] array list length is``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    ignore ((arr:> IList<int>).InsNum 5 0 )
    ignore ((arr:> IList<int>).InsNum 6 0 )
    ignore ((arr:> IList<int>).InsNum 7 1 )
    Assert.AreEqual ((arr:> IList<int>).Length , 3)
[<Test>]
let ``66)DelTop to empty array list is false``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    Assert.AreEqual ((arr:> IList<int>).DelTop () , false)
[<Test>]
let ``67)DelTop to [|5|] array list is true``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5|]
    Assert.AreEqual ((arr:> IList<int>).DelTop () , true)
[<Test>]
let ``68)DelTop to [|5|] array list is [||]``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5|]
    ignore ((arr:> IList<int>).DelTop ())
    Assert.AreEqual (arr.Idx , [||])
[<Test>]
let ``69)DelTop to [|6; 5|] array list is Cons [|5|]``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|]
    ignore ((arr:> IList<int>).DelTop ())
    Assert.AreEqual (arr.Idx ,[|5|])
[<Test>]
let ``70)DelTop to [|6; 5|] array list length is 1``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <-[|6; 5|] 
    ignore ((arr:> IList<int>).DelTop ())
    Assert.AreEqual ((arr:> IList<int>).Length , 1)
[<Test>]
let ``71)DelEnd to [||] array list is false``() =
    let arr = new ArrayList<int> ()
    arr.Idx <-[||] 
    Assert.AreEqual ((arr:> IList<int>).DelEnd () , false)
[<Test>]
let ``72)DelEnd to [|5|]  array list is true``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5|] 
    Assert.AreEqual ((arr:> IList<int>).DelEnd () , true)
[<Test>]
let ``73)DelEnd to [|5|]  array list is [||] ``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5|] 
    ignore ((arr:> IList<int>).DelTop ())
    Assert.AreEqual (arr.Idx ,[||])
[<Test>]
let ``74)DelEnd to [|6; 5|]  array list is [|6|] ``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|] 
    ignore ((arr:> IList<int>).DelEnd ())
    Assert.AreEqual (arr.Idx , [|6|] )
[<Test>]
let ``75)DelEnd to [|6; 5|] array list length is 1``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|] 
    ignore ((arr:> IList<int>).DelEnd ())
    Assert.AreEqual ((arr:> IList<int>).Length , 1)
[<Test>]
let ``76)DelNum to empty array list is false``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||] 
    Assert.AreEqual ((arr:> IList<int>).DelNum 0 , false)
[<Test>]
let ``77)DelNum 0 to [|5|]  array list is true``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|5|] 
    Assert.AreEqual ((arr:> IList<int>).DelNum 0, true)
[<Test>]
let ``78)DelNum 0 to [|6; 5|] ) array list is [|5|] ``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|] 
    ignore ((arr:> IList<int>).DelNum 0)
    Assert.AreEqual (arr.Idx , [|5|] )
[<Test>]
let ``79)DelNum 1 to [|6; 5|] array list is [|6|] ``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|] 
    ignore ((arr:> IList<int>).DelNum 1)
    Assert.AreEqual (arr.Idx , [|6|])
[<Test>]
let ``80)DelNum 0  to [|6; 5|]  array list length is 1``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|] 
    ignore ((arr:> IList<int>).DelNum 0)
    Assert.AreEqual ((arr:> IList<int>).Length , 1)
[<Test>]
let ``81)DelNum 30  to [|6; 5|] array list is [|6; 5|]``() = 
    let arr = new ArrayList<int> ()
    arr.Idx <- [|6; 5|]
    ignore ((arr:> IList<int>).DelNum 30)
    Assert.AreEqual (arr.Idx , [|6; 5|])
[<Test>]
let ``82)Find even element in empty array list is None``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    Assert.AreEqual ((arr:> IList<int>).Find (fun x -> x % 2 = 0), None)
[<Test>]
let ``83)Find even element in odd array list is None``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|1; 3; 3; 9|]
    Assert.AreEqual ((arr:> IList<int>).Find (fun x -> x % 2 = 0), None)
[<Test>]
let ``84)Find even element in [|1; 60; 3; 9|] array list is Some 60)``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|1; 60; 3; 9|]
    Assert.AreEqual ((arr:> IList<int>).Find (fun x -> x % 2 = 0), Some 60)
[<Test>]
let ``85)Find even element in [|4; 9; 3; 18|] array list is Some 4``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|4; 9; 3; 18|] 
    Assert.AreEqual ((arr:> IList<int>).Find (fun x -> x % 2 = 0), Some 4)
[<Test>]
let ``86)Find even element in [|1; 9; 3; 18|] array list is Some 18``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|1; 9; 3; 18|]
    Assert.AreEqual ((arr:> IList<int>).Find (fun x -> x % 2 = 0), Some 18)
[<Test>]
let ``87)Concat 2 Nil array lists is Nil``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    let arr1 = new ArrayList<int> ()
    arr1.Idx <- [||]
    (arr:> IList<int>).Concat arr1
    Assert.AreEqual (arr.Idx , [||])
[<Test>]
let ``88)Concat [|2; 1|] with [||] array list is [|2; 1|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|2; 1|]
    let arr1 = new ArrayList<int> ()
    arr1.Idx <- [||]
    (arr:> IList<int>).Concat arr1
    Assert.AreEqual (arr.Idx,[|2; 1|])
[<Test>]
let ``89)Concat [||] with [|2; 1|] array list is [|2; 1|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    let arr1 = new ArrayList<int> ()
    arr1.Idx <- [|2; 1|]
    (arr:> IList<int>).Concat arr1
    Assert.AreEqual (arr.Idx,[|2; 1|])
[<Test>]
let ``90)Concat [|3; 4|] with [|2; 1|] array list is ,[|3; 4; 2; 1|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|3; 4|]
    let arr1 = new ArrayList<int> ()
    arr1.Idx <- [|2; 1|]
    (arr:> IList<int>).Concat arr1
    Assert.AreEqual (arr.Idx, [|3; 4; 2; 1|])
[<Test>]
let ``91)Concat [|3; 4|] with [|2; 1|] array list length is 4``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|3; 4|]
    let arr1 = new ArrayList<int> ()
    arr1.Idx <- [|2; 1|]
    (arr:> IList<int>).Concat arr1
    Assert.AreEqual ((arr:> IList<int>).Length, 4)
[<Test>]
let ``92)Head of empty array list is None``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    Assert.AreEqual ((arr:> IList<int>).Head(), None)
[<Test>]
let ``93)Head of [|4|] array arr is Some 4``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|4|]
    Assert.AreEqual ((arr:> IList<int>).Head(), Some 4)
[<Test>]
let ``94)Head of [|3; 4; 2; 1|] array list is Some 3``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|3; 4; 2; 1|]
    Assert.AreEqual ((arr:> IList<int>).Head(), Some 3)
[<Test>]
let ``95)IsEmpty for empty array list is true``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [||]
    Assert.AreEqual ((arr:> IList<int>).IsEmpty (), true)
[<Test>]
let ``96)IsEmpty for [|4|] array list is false``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|4|]
    Assert.AreEqual ((arr:> IList<int>).IsEmpty(), false)
[<Test>]
let ``97)Concat Cons(3, Cons (4, Nil)) with [|2; 1|], ATD list is ,Cons(3, Cons (4, Cons (2, Cons (1, Nil))))``() =
    let list = new List<int> ()
    list.Idx <- Cons(3, Cons (4, Nil))
    let list1 = new ArrayList<int> ()
    list1.Idx <- [|2; 1|]
    (list:> IList<int>).Concat list1
    Assert.AreEqual (list.Idx,Cons(3, Cons (4, Cons (2, Cons (1, Nil)))))

[<Test>]
let ``98)Concat [|3; 4|] with Cons(2, Cons (1, Nil)) array list is ,[|3; 4; 2; 1|]``() =
    let arr = new ArrayList<int> ()
    arr.Idx <- [|3; 4|]
    let arr1 = new List<int> ()
    arr1.Idx <- Cons(2, Cons (1, Nil))
    (arr:> IList<int>).Concat arr1
    Assert.AreEqual (arr.Idx, [|3; 4; 2; 1|])

[<EntryPoint>]
let main argv =
    let ls1= new List<int> ()
    ls1.Idx <- Cons (4, Cons (5, Cons ( 1, Nil) ) )
    printfn "List at the beginning"
    let ls = ls1:> IList<int>
    ls.Printn ()

    printfn "Insert 10 to beginning"
    ls.InsTop 10
    ls.Printn ()

    printfn "Insert 21 and 31 to the end"
    ls.InsEnd 21
    ls.InsEnd 31
    ls.Printn ()

    printfn "Insert 13 to the third place (numeration from 0)"
    ignore (ls.InsNum 13 3)
    ls.Printn ()

    printfn "Delete head"
    ignore (ls.DelTop () )
    ls.Printn ()

    printfn "Delete end"
    ignore (ls.DelEnd () )
    ls.Printn ()

    printfn "Delete second elem"
    ignore ( ls.DelNum 2 )
    ls.Printn ()

    printfn "Find 1 find %A " (ls.Find (fun x -> x = 1))

    let ls_= new List<int> ()
    ls_.Idx <- Cons (2, Cons (0, Cons ( 54, Nil) ) )
    let ls2 = ls_:> IList<int>
    printfn "Check concat \n The second list is"
    ls2.Printn ()
    printfn "The result is"
    ls.Concat ls2
    ls.Printn ()

    printfn "\n\nNow check array!!!!"
    let arr1= new ArrayList<int> ()
    arr1.Idx <- [|4; 5; 1; 6; 7|]
    printfn "Array at the beginning"
    let arr = arr1:> IList<int>
    arr.Printn ()

    printfn "Insert 10 to beginning"
    arr.InsTop 10
    arr.Printn ()

    printfn "Insert 21 and 31 to the end"
    arr.InsEnd 21
    arr.InsEnd 31
    arr.Printn ()

    printfn "Insert 13 to the third place (numeration from 0)"
    printfn "%d" arr.Length
    ignore (arr.InsNum 13 3)
    arr.Printn ()

    printfn "Delete head"
    ignore (arr.DelTop ())
    arr.Printn ()

    printfn "Delete end"
    ignore (arr.DelEnd () )
    arr.Printn ()

    printfn "Delete second elem"
    ignore (arr.DelNum 2 )
    arr.Printn ()

    printfn "Find 1 find %A " (arr.Find (fun x -> x = 1))

    let arr_= new ArrayList<int> ()
    arr_.Idx <- [| 1; 3; 6; 7|]
    let arr2 = arr_:> IList<int>
    printfn "Check concat \n The second array is"
    arr2.Printn ()
    printfn "The result is"
    arr.Concat arr2
    arr.Printn ()

    0     