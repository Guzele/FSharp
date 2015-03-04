(* List transformation with fold - second week homework
   Author Guzel Garifullina 171
   reverse
   Estimated time 30 minutes
   real time      2 hours
   filter
   Estimated time 30 minutes
   real time      30 minutes   
   map
   Estimated time 5 minutes
   real time      5 minutess  *)

let reverse l = List.fold (fun acc  x -> x :: acc ) [] l 

let filter f l =
    let l = List.fold (fun acc x -> if f x then x :: acc
                                    else acc) [] l  
    reverse l

let is_even x = (x % 2 = 0)

let map f l =
    let l = List.fold (fun acc x -> (f x) :: acc) [] l
    reverse l


[<EntryPoint>]
let main argv = 
    let l = [1; 2; 3; 6; 4; 5; 5; 10; 4]
    printfn "Initial list %A"  l
    printfn "Reversed list %A" (reverse  l)
    printfn "Filter list only even %A" ( filter (is_even ) l ) 
    printfn "Adding '1' using map %A" ( map (fun x -> x + 1) l )
    0 

