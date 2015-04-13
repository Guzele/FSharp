(* Interpretator of L - sixth week homework
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

    ///program file
    let inStream = new StreamReader(argv.[0])
    let mutable str = inStream.ReadToEnd ()
    inStream.Dispose ()

    /// delete /r
    let del_sp (str : string) = 
        let mutable s = str
        while (s.IndexOf '\r') >= 0 do
            s <- s.Remove ((s.IndexOf '\r'), 1)
        s

    str <- del_sp str

    let mutable list = []
    if (argv.Length) = 1
        then ignore (interpritate (str |> inpToStr |> makeProgramTree) list Mode1) /// Mode1 - console output
             exit 0

    /// input file
    use stream = new StreamReader (argv.[1])
    let mutable str_list = stream.ReadToEnd ()
    str_list <- del_sp str_list


    list <- List.map (fun x -> int x) (inpToStr str_list)
    if (argv.Length) = 2
        then ignore (interpritate (str |> inpToStr |> makeProgramTree) list Mode1)
             exit 0
        else use stream = new StreamWriter (argv.[2])
             /// Mode2 is file output 
             (interpritate (str |> inpToStr |> makeProgramTree) list Mode2) |> ( stream.WriteLine)

    0 

