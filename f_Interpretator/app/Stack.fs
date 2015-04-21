module Stack

type IStack<'T> =
    abstract member Id : list<'T> with get,set
    abstract member Empty: bool
    abstract member Pop  : 'T
    abstract member Push : 'T -> unit
    abstract member Rev  : unit

type Stack<'T when 'T : equality> () =
    interface IStack<'T> with
        member val Id = [] with get,set
        member this.Empty = (this:> IStack<'T>).Id = []
        member this.Pop   = 
            match (this:> IStack<'T>).Id with 
            | []     -> failwith "Intrnal Error:Stack Pop"
            | x :: l -> 
                (this:> IStack<'T>).Id <- l
                x
        member this.Push x = 
            (this:> IStack<'T>).Id <- x :: (this:> IStack<'T>).Id
        member this.Rev  = 
            (this:> IStack<'T>).Id <- List.rev ((this:> IStack<'T>).Id)
