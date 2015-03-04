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
   real time      5 minutess 
   Horner's rule
   Estimated time 45 minutes
   real time      40 minutess  *)

let reverse l = List.fold (fun acc  x -> x :: acc ) [] l 

let filter f l =
    let l = List.fold (fun acc x -> if f x then x :: acc
                                    else acc) [] l  
    reverse l

let is_even x = (x % 2 = 0)

let map f l =
    let l = List.fold (fun acc x -> (f x) :: acc) [] l
    reverse l

//p(x) = a_0 + x(a_1 + x(a_2 + ... + x(a_{n-1} + a_n x)...))
// [a_n; a_{n-1}; ..., a_1; a_0]
let  Horner_rule l x =
    match l with
    | []       -> 0
    | a_n :: l ->  List.fold (fun acc a -> a + x * acc) a_n l

[<EntryPoint>]
let main argv = 
    let mutable l = [1; 2; 3; 4; 5; 6; 7; 8]
    printfn "Initial list %A"  l
    printfn "Reversed list %A" (reverse  l)
    printfn "Filter list only even %A" ( filter (is_even ) l ) 
    printfn "Adding '1' using map %A\n" ( map (fun x -> x + 1) l )

    l <- [1; 2; 3]
    printfn "Horner's rule for list %A" l
    printfn "Polynomial starting with leading coefficient"
    printfn "Value of polynomial, when x = -1, is %i" (Horner_rule l -1)

    0 

