(* Polymorphic list - third week homework
   Author Guzel Garifullina 171
   Interface and realization with ADT
   Estimated time 1 hour
   real time      3 hours
 *)

type List_ADT<'T when 'T: equality> = Nil | Cons of 'T * List_ADT<'T>

type IList<'A> =
    interface
       // abstract id : '_A with get, set
        abstract insTop : 'A -> unit
        abstract insEnd : 'A -> unit
        abstract insNum : 'A -> int -> unit
        abstract delTop : unit -> unit
        abstract delEnd : unit -> unit
        abstract delNum : int -> unit
        abstract find   : ('A -> bool) -> 'A
        abstract concat : IList<'A> -> unit 
        abstract head   : unit -> 'A
        abstract last   : unit -> 'A
        abstract isEmpty: unit -> bool
        abstract printn : unit -> unit
    end

type List<'A when 'A: equality> () =
    class
        member val id  = Nil with set, get
        interface IList<'A> with
            member this.insTop x  = 
                this.id <- Cons (x, this.id) 
            member this.insEnd x =
                let rec push_end l x =
                    match l with
                    | Nil            -> Cons ( x, Nil)
                    | Cons (head, l) -> Cons ( head, (push_end l x) )
                this.id <- push_end this.id x
            member this.insNum x i =
                let rec push_num l x i =
                    match l, i with 
                    | _, 0              ->  Cons (x,l)
                    | Nil, _            -> failwith "Error, not enoght elements in list"
                    | Cons (head, l), _ -> Cons (head, push_num l x (i-1))
                this.id <- push_num this.id x i
            member this.delTop () =
                match this.id with
                | Nil -> failwith "Error, not enoght elements in list"
                | Cons (head, l) -> this.id <- l
            member this.delEnd () = 
                let rec del_end l =
                    match l with
                    | Nil           -> failwith "Error, not enoght elements in list"
                    | Cons (x, Nil) -> Nil
                    | Cons (x, l)   -> Cons (x, del_end l)
                this.id <- del_end this.id
            member this.delNum i = 
                let rec del_num l i =
                    match l, i with
                    | Nil, _ -> failwith "Error, not enoght elements in list"
                    | Cons (x, l), 0 -> l
                    | Cons (x, l), _ -> Cons (x, del_num l (i-1))
                this.id <- del_num this.id i
            member this.find func =
               let rec find_elem f l = 
                   match l with
                   | Nil -> failwith "Error, not enoght elements in list"
                   | Cons (x, l) -> 
                       if f x then x
                       else find_elem f l
               find_elem func this.id
            member this.head () =
                match this.id with
                | Nil -> failwith "Error, not enoght elements in list"
                | Cons (x, _) -> x
            member this.last () = 
                let rec last l=
                    match l with
                    | Nil   -> failwith "Error, not enoght elements in list"
                    | Cons ( x , Nil) ->  x
                    | Cons (x, l)     -> last l
                last this.id
            member this.isEmpty() = (this.id = Nil)
            member this.concat l = 
                let temp= new List<'A> ()
                temp.id <- this.id
                while l.isEmpty() = false do
                    let x = l.head ()
                    (temp:> IList<'A>).insEnd x
                    l.delTop ()
                this.id <- temp.id
            member this.printn () = 
                printfn "%A\n" this.id
    end      

[<EntryPoint>]
let main argv =
    let ls1= new List<int> ()
    ls1.id <- Cons (4, Cons (5, Cons ( 1, Nil) ) )
    printfn "List at the beginning"
    let ls = ls1:> IList<int>
    ls.printn ()

    printfn "Insert 10 to beginning"
    ls.insTop 10
    ls.printn ()

    printfn "Insert 21 and 31 to the end"
    ls.insEnd 21
    ls.insEnd 31
    ls.printn ()

    printfn "Insert 13 to the third place (numeration from 0)"
    ls.insNum 13 3
    ls.printn ()

    printfn "Delete head"
    ls.delTop ()
    ls.printn ()

    printfn "Delete end"
    ls.delEnd ()
    ls.printn ()

    printfn "Delete second elem"
    ls.delNum 2
    ls.printn ()

    printfn "Find 1 find %d " (ls.find (fun x -> x = 1))

    let ls_= new List<int> ()
    ls_.id <- Cons (2, Cons (0, Cons ( 54, Nil) ) )
    let ls2 = ls_:> IList<int>
    printfn "Check concat \n The second list is"
    ls2.printn ()
    printfn "The result is"
    ls.concat ls2
    ls.printn ()

    0     