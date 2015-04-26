(* Scientific calc - sevenh week homework
   Author Guzel Garifullina 171
*)
module Gui

open Calculator
open System
open System.Drawing
open System.Windows.Forms

let sizeX = 75
let sizeY = 60
let mutable ans = ("", [0])
let mutable len = [0]

let inputLabel  =
    let field = new Label  (Text = "",
                            Location = new Point (0, 0), 
                            Size = new Size(5 * sizeX, sizeY),
                            BackColor = Color.SlateGray,
                            ForeColor = Color.White,
                            TextAlign = ContentAlignment.MiddleLeft,
                            Font = new Font("Ariel", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy)
                       

                            )
    field
let textPanel = new Label(Text = "Waiting for input",
                    Anchor = AnchorStyles.Left,
                    Height = 20,
                    TextAlign = ContentAlignment.MiddleLeft)
let panel  = 
     let pan = new FlowLayoutPanel(Visible = true,
                                   Height = 20,
                                   Dock = DockStyle.Bottom,
                                   BorderStyle = BorderStyle.FixedSingle) 
     
     let progress = new ProgressBar(Visible = false,
                                Anchor = (AnchorStyles.Left ||| AnchorStyles.Top),
                                Value = 0)
     pan.Controls.Add(progress)
     pan.Controls.Add(textPanel)
     pan

let findAnswer () =
    textPanel.Text <- "Running..."
    let mutable str = fst ans
    if (isBracketCorrect str) && (notEndsOp str) && (isCorNum str)
    then
        let res = calculate str
        let res_str = string res
        if res < 0.0
        then 
            let mutable list = [5; 2; 0]
            let mutable leng_list = [1; 1; 0]
            for i = 1 to (res_str.Length - 1) do
                list<- 1 :: list
                leng_list<- 1 :: leng_list
            str <-  "(" + (string res) + ")"
            ans <- (str,  (3 :: list))
            len <- 1  :: leng_list
        else 
            let mutable list = [0]
            let mutable leng_list = [0]
            for i = 1 to (res_str.Length ) do
                list<- 1 :: list
                leng_list<- 1 :: leng_list
            str <-  string res
            ans <- (str, list)
            len <- leng_list
        inputLabel.Text <- str
        textPanel.Text <- "Waiting for input"
    else MessageBox.Show "Wrong expression" |> ignore
        
let eqButton  =
    let but = new Button (Text = "=",
                            Location = new Point (5 * sizeX ,  sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.DarkOrange,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            Enabled = false,
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> findAnswer () )
    but

let delete (but : Button) =
    let mutable str = fst ans
    let leng = len.Head
    str <- str.Remove(str.Length - leng )
    let prev = (snd ans).Tail
    if str = ""
    then 
        but.Enabled <- false
        eqButton.Enabled  <- false
    ans <- (str, prev)
    len <- len.Tail
    inputLabel.Text <- str
let delButton  =
    let but = new Button (Text = "<-",
                            Location = new Point (5 * sizeX ,  2 * sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup,
                            Enabled = false)
    but.Click.Add (fun e -> delete but )
    but

let cleanScreen () =
    ans <- ("", [0])
    len <- [0]
    eqButton.Enabled <- false
    delButton.Enabled <- false
    inputLabel.Text <- ""

let cleanButton  =
    let but = new Button (Text = "AC",
                            Location = new Point (5 * sizeX ,  0), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.MediumSpringGreen,//MediumPurple,
                            ForeColor = Color.DarkSlateBlue,
                            FlatStyle = FlatStyle.Popup,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy))
    but.Click.Add (fun e -> cleanScreen () )
    but
let add ch prior leng =
    delButton.Enabled <- true
    eqButton.Enabled <- true

    let leng_prev = len.Head
    let mutable str = fst ans
    let mutable prev = snd ans
    match (prev.Head), prior with 
    | (4 , 4) | (4 , 5)| (5 , 4) | ( 5 , 5)   ->
        str <- str.Remove(str.Length - leng_prev)
        str <- str + ch
        prev <- prior :: (prev.Tail)
        len  <- leng :: (len.Tail)
        ans  <- (str, prev)
        inputLabel.Text <- str

    | _               -> 
        str <- str + ch
        if not (isCorrect (prev.Head) prior)
        then 
            MessageBox.Show "Wrong expression" |> ignore
            str <-str.Remove(str.Length - leng)
            ans <- (str,  prev)
        else
            ans <- (str, (prior :: prev))
            len  <- leng :: len
        inputLabel.Text <- str

let openBracketButton  =
    let but = new Button (Text = "(",
                            Location = new Point (1 * sizeX ,   sizeY * 5), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 2 1)
    but
let closedBracketButton =
    let but = new Button (Text = ")",
                            Location = new Point (2 * sizeX ,  5 * sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 3 1)
    but

let plusButton =
    let but = new Button (Text = "+",
                            Location = new Point (5 * sizeX ,  3 * sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 1)
    but
let minusButton  =
    let but = new Button (Text = "-",
                            Location = new Point (5 * sizeX ,  4 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 5 1)
    but
let multButton  =
    let but = new Button (Text = "*",
                            Location = new Point ( 3 * sizeX , sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 1)
    but

let divButton  =
    let but = new Button (Text = "/",
                            Location = new Point (4 * sizeX ,  sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 1)
    but

let powButton  =
    let but = new Button (Text = "^",
                            Location = new Point ( 2 * sizeX, sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 1)
    but

let logButton  =
    let but = new Button (Text = "log",
                            Location = new Point ( 0 , 2 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add ( (but.Text) + "(" ) 8 4)
    but

let lnButton  =
    let but = new Button (Text = "ln",
                            Location = new Point ( sizeX , 2 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add ((but.Text) + "(" ) 8 3 )
    but

let sqrButton  =
    let but = new Button (Text = "sqr",
                            Location = new Point ( 0 , 3 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add ((but.Text) + "(" ) 8 4)
    but

let modButton  =
    let but = new Button (Text = "%",
                            Location = new Point ( sizeX , 3 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 1)
    but

let sinButton  =
    let but = new Button (Text = "sin",
                            Location = new Point ( 0 , 4 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add ((but.Text) + "(" ) 8 4)
    but

let cosButton  =
    let but = new Button (Text = "cos",
                            Location = new Point ( sizeX , 4 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add ((but.Text) + "(" ) 8 4)
    but

let tanButton  =
    let but = new Button (Text = "tan",
                            Location = new Point ( 0 , 5 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add ((but.Text) + "(" ) 8 4)
    but
let digitButtons value x y : Button = 
     let but = new Button (Text = string value,
                           Location = new Point (x,  y), 
                           Size = new Size(sizeX, sizeY),
                           BackColor = Color.PaleTurquoise,
                           ForeColor = Color.DarkSlateBlue,
                           Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                           FlatStyle = FlatStyle.Popup)
     but.Click.Add (fun e -> add (but.Text) 1 1)
     but

let piButton  = 
     let but = new Button (Text = "Pi",
                           Location = new Point ( 0 , sizeY), 
                           Size = new Size(sizeX, sizeY),
                           BackColor = Color.PaleTurquoise,
                           ForeColor = Color.DarkSlateBlue,
                           Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                           FlatStyle = FlatStyle.Popup)
     but.Click.Add (fun e -> add (but.Text) 7 2)
     but
let eButton  = 
     let but = new Button (Text = "e",
                           Location = new Point ( sizeX , sizeY), 
                           Size = new Size(sizeX, sizeY),
                           BackColor = Color.PaleTurquoise,
                           ForeColor = Color.DarkSlateBlue,
                           Font = new Font("Curlz MT", 18.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                           FlatStyle = FlatStyle.Popup)
     but.Click.Add (fun e -> add (but.Text) 7 1)
     but
let pointButton  = 
     let but = new Button (Text = ".",
                           Location = new Point ( 3 * sizeX , 5 *  sizeY), 
                           Size = new Size(sizeX, sizeY),
                           BackColor = Color.PaleTurquoise,
                           ForeColor = Color.DarkSlateBlue,
                           Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                           FlatStyle = FlatStyle.Popup)
     but.Click.Add (fun e -> add (but.Text) 6 1)
     but

                                            
let exitButton =
    let but = new Button (Text = "Exit!",
                            Location = new Point (sizeX * 5  , 5 * sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.HotPink,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Times New Roman", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> Application.Exit () )
    but

let mainForm =
    let form = new Form(Text = "Calculator",
                        Visible = false, 
                        TopMost = true, 
                        Size = new Size(sizeX * 6, sizeY * 6 + 47),
                        StartPosition = FormStartPosition.CenterScreen,
                        BackColor = Color.LightGray,
                        ForeColor = Color.Black)

    form.Controls.Add(inputLabel)
    form.Controls.Add (cleanButton )
    form.Controls.Add (digitButtons 0 (4 * sizeX) (5 * sizeY))
    for i  = 1 to 9 do
        form.Controls.Add (digitButtons i (((i - 1) % 3 + 2) * sizeX)  (((i - 1) / 3 + 2 ) * sizeY) )
    form.Controls.Add (piButton)
    form.Controls.Add (eButton)
    form.Controls.Add (pointButton)
    form.Controls.Add (openBracketButton )    
    form.Controls.Add (closedBracketButton)
    form.Controls.Add (eqButton)
    form.Controls.Add (delButton)
    form.Controls.Add (plusButton)
    form.Controls.Add (minusButton)
    form.Controls.Add (multButton)
    form.Controls.Add (divButton)
    form.Controls.Add (powButton)

    form.Controls.Add (logButton)
    form.Controls.Add (lnButton)
    form.Controls.Add (sqrButton)
    form.Controls.Add (modButton)
    form.Controls.Add (sinButton)
    form.Controls.Add (cosButton)
    form.Controls.Add (tanButton)

    form.Controls.Add (exitButton)
    form.Controls.Add(panel)
    form


