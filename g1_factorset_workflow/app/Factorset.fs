(* Factorset - seventh week homework
   Author Guzel Garifullina 171
*)

module Factorset

type Factorize (n) =
    let make_ring x =
        let modul = x % n
        if modul < 0 
        then (n + modul)
        else modul
    member this.Bind (x, f) = f (make_ring x)
    member this.Return x = (make_ring x)

let ring x = new Factorize(x)
