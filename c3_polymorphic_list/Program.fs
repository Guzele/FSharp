(* Polymorphic list - third week homework
   Author Guzel Garifullina 171
   Interface and realization with ADT
   Estimated time 1 hour
   real time      3 hours
 *)

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
                if n < i
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
                if  n < i 
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