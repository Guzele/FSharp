(* Simple calculator - sevenh week homework
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

let inputLabel  =
    let field = new Label  (Text = "",
                            Location = new Point (0, 0), 
                            Size = new Size(3 * sizeX, sizeY),
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
    if (isBracketCorrect str) && (notEndsOp str)
    then
        let res = calculate str
        if res < 0
        then 
            let mutable list = [5; 2; 0]
            for i = 1 to (str.Length - 1) do
                list<- 1 :: list
            str <-  "(" + (string res) + ")"
            ans <- (str,  (3 :: list)) 
        else 
            let mutable list = [0]
            for i = 1 to (str.Length ) do
                list<- 1 :: list
            str <-  string res
            ans <- (str, list) 
        inputLabel.Text <- str
        textPanel.Text <- "Waiting for input"
    else MessageBox.Show "Wrong expression" |> ignore
        
let eqButton  =
    let but = new Button (Text = "=",
                            Location = new Point (3 * sizeX ,  sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.DarkOrange,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            Enabled = false,
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> findAnswer () )
    but

let delete (but : Button) =
    eqButton.Enabled <- true
    let mutable str = fst ans
    str <- str.Remove(str.Length - 1)
    let prev = (snd ans).Tail
    if str = ""
    then 
        but.Enabled <- false
        eqButton.Enabled  <- false
    ans <- (str, prev)
    inputLabel.Text <- str
let delButton  =
    let but = new Button (Text = "<-",
                            Location = new Point (3 * sizeX ,  2 * sizeY ), 
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
    eqButton.Enabled <- false
    delButton.Enabled <- false
    inputLabel.Text <- ""

let cleanButton  =
    let but = new Button (Text = "AC",
                            Location = new Point (3 * sizeX ,  0), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.MediumSpringGreen,//MediumPurple,
                            ForeColor = Color.DarkSlateBlue,
                            FlatStyle = FlatStyle.Popup,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy))
    but.Click.Add (fun e -> cleanScreen () )
    but
let add ch prior =
    delButton.Enabled <- true
    eqButton.Enabled <- true
    let mutable str = fst ans
    let prev = snd ans
    if (prior = prev.Head) && (prior = 4) /// operation
    then 
        str <- str.Remove(str.Length - 1)
        str <- str + ch
        inputLabel.Text <- str
    else
        str <- str + ch
        if not (isCorrect (prev.Head) prior)
        then 
            MessageBox.Show "Wrong expression" |> ignore
            str <-str.Remove(str.Length - 1)
            ans <- (str,  prev)
        else
            ans <- (str, (prior :: prev))
        inputLabel.Text <- str

let openBracketButton  =
    let but = new Button (Text = "(",
                            Location = new Point (sizeX ,  5 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 2 )
    but
let closedBracketButton =
    let but = new Button (Text = ")",
                            Location = new Point (2 * sizeX ,  5 * sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 12.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 3 )
    but

let plusButton =
    let but = new Button (Text = "+",
                            Location = new Point (3 * sizeX ,  3 * sizeY ), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 )
    but
let minusButton  =
    let but = new Button (Text = "-",
                            Location = new Point (3 * sizeX ,  4 * sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 5 )
    but
let multButton  =
    let but = new Button (Text = "*",
                            Location = new Point ( sizeX , sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 )
    but
let divButton  =
    let but = new Button (Text = "/",
                            Location = new Point (2 * sizeX ,  sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 )
    but
let powButton  =
    let but = new Button (Text = "^",
                            Location = new Point (0, sizeY), 
                            Size = new Size(sizeX, sizeY),
                            BackColor = Color.Turquoise,
                            ForeColor = Color.DarkSlateBlue,
                            Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                            FlatStyle = FlatStyle.Popup)
    but.Click.Add (fun e -> add (but.Text) 4 )
    but

let digitButtons value x y : Button = 
     let but = new Button (Text = string value,
                           Location = new Point (x,  y), 
                           Size = new Size(sizeX, sizeY),
                           BackColor = Color.PaleTurquoise,
                           ForeColor = Color.DarkSlateBlue,
                           Font = new Font("Curlz MT", 16.00f, FontStyle.Bold, GraphicsUnit.Point, 0uy),
                           FlatStyle = FlatStyle.Popup)
     but.Click.Add (fun e -> add (but.Text) 1 )
     but

                                            
let exitButton =
    let but = new Button (Text = "Exit!",
                            Location = new Point (sizeX * 3  , 5 * sizeY ), 
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
                        Size = new Size(sizeX * 4, sizeY * 6 + 47),
                        StartPosition = FormStartPosition.CenterScreen,
                        BackColor = Color.LightGray,
                        ForeColor = Color.Black)

    form.Controls.Add(inputLabel)
    form.Controls.Add (cleanButton )
    form.Controls.Add (digitButtons 0 0 (5 * sizeY))
    for i  = 1 to 9 do
        form.Controls.Add (digitButtons i ((i - 1) % 3 * sizeX)  (((i - 1) / 3 + 2 ) * sizeY) )
    form.Controls.Add (openBracketButton )    
    form.Controls.Add (closedBracketButton)
    form.Controls.Add (eqButton)
    form.Controls.Add (delButton)
    form.Controls.Add (plusButton)
    form.Controls.Add (minusButton)
    form.Controls.Add (multButton)
    form.Controls.Add (divButton)
    form.Controls.Add (powButton)
    form.Controls.Add (exitButton)
    form.Controls.Add(panel)
    form


