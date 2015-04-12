(* Calculator - sixth week homework
   Author Guzel Garifullina 171
   Estimated time 8 hours
   real time      8 hours
*)
open System
open System.IO
open Expr
open Stmt
open Interpreter


[<EntryPoint>]
let main argv = 
    if argv.Length < 1
    then printfn "Not enough arguments.\n"
         exit 1

    let inStream = new StreamReader(argv.[0])
    let mutable str = inStream.ReadToEnd ()
    inStream.Dispose ()

    let del_sp (str : string) = 
        let mutable s = str
        while (s.IndexOf '\r') >= 0 do
            s <- s.Remove ((s.IndexOf '\r'), 1)
        s

    str <- del_sp str


    let mutable list = []
    if (argv.Length) = 1
        then (interpritate (str |> inpToStr |> makeProgramTree) list ) |> printfn "%s"
             exit 0

    use stream = new StreamReader (argv.[1])
    let mutable str_list = stream.ReadToEnd ()
    str_list <- del_sp str_list


    list <- List.map (fun x -> int x) (inpToStr str_list)
    if (argv.Length) = 2
        then (interpritate (str |> inpToStr |> makeProgramTree) list ) |> printfn "%s"
             exit 0
        else use stream = new StreamWriter (argv.[2])
             (interpritate (str |> inpToStr |> makeProgramTree) list ) |> string |> ( stream.WriteLine)

    0 

