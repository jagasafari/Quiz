module Quiz.TextToKeyboard

open System
open System.Globalization

let tryConvertToConsoleKey (character: Char) =
    let charString = character.ToString()
    Enum.TryParse(charString, true)    

let getDigits() = ['0'..'9']

let mapKeyboardChar (ch: Char) =
    if Char.IsLetter ch then 
        let (_, consoleKey) = tryConvertToConsoleKey ch
        let letterConsoleKey = Ck consoleKey
        if Char.IsLower ch then letterConsoleKey else Cm (cm.Shift, consoleKey)
    else 
        match ch with
        | '0' -> Ck ck.D0
        | '1' -> Ck ck.D1
        | '2' -> Ck ck.D2
        | '3' -> Ck ck.D3
        | '4' -> Ck ck.D4
        | '5' -> Ck ck.D5
        | '6' -> Ck ck.D6
        | '7' -> Ck ck.D7
        | '8' -> Ck ck.D8
        | '9' -> Ck ck.D9
        | '-' -> Ck ck.OemMinus
        | '/' -> Ck ck.Oem2
        | '`' -> Ck ck.Oem3
        | '}' -> Cm (cm.Shift, ck.Oem6)
        | '#' -> Cm (cm.Shift, ck.D3)
        | '.' -> Ck ck.OemPeriod
        | '[' -> Ck ck.Oem4
        | ']' -> Ck ck.Oem5
        | '(' -> Cm (cm.Shift, ck.D9)
        | ')' -> Cm (cm.Shift, ck.D0)
        | '{' -> Cm (cm.Shift, ck.Oem4)
        | '*' -> Cm (cm.Shift, ck.D8)
        | ':' -> Cm (cm.Shift, ck.Oem1)
        | '!' -> Cm (cm.Shift, ck.D1)
        | ',' -> Ck ck.OemComma
        | ' ' -> Ck ck.Spacebar
        | '%' -> Cm (cm.Shift, ck.D5)
        | '$' -> Cm (cm.Shift, ck.D4)
        | '"' -> Cm (cm.Shift, ck.Oem7)
        | '=' -> Ck ck.OemPlus
        | '@' -> Cm (cm.Shift, ck.D2)
        | '<' -> Cm (cm.Shift, ck.OemComma)
        | ''' -> Ck ck.Oem7
        | ';' -> Ck ck.Oem1
        | '?' -> Cm (cm.Shift, ck.Oem2)
        | '>' -> Cm (cm.Shift, ck.OemPeriod)
        | _ -> Ck ck.D8

type KeyType = Tab | Escape 

let startsWith pattern (str: string) = str.StartsWith pattern

let trimString (str: string) len = if str.Length >= len then str.Substring(len) else ""

let strLength (str: string) = str.Length

let retrieveCtrlKey (str: string) = Enum.Parse (typeof<ConsoleKey>, (str.[5]) |> string )

let rec getConsoleKeys acc str = 
    let nextCall key str trimStr = getConsoleKeys (acc@[key]) (trimString str (strLength trimStr))
    match str with
    | s when s |> startsWith "Tab" -> nextCall (Ck ck.Tab) s "Tab"
    | s when s |> startsWith "Escape" -> nextCall (Ck ck.Escape) s "Escape"
    | s when s |> startsWith "Spacebar" -> nextCall (Ck ck.Spacebar) s "Spacebar"
    | s when s |> startsWith "Delete" -> nextCall (Ck ck.Delete) s "Delete"
    | s when s |> startsWith "Ctrl-" -> 
        let _, ctrlKey = tryConvertToConsoleKey (s.[5])
        nextCall (Cm (cm.Control, ctrlKey)) s "Ctrl-X"
    | s  when s = "" -> acc
    | s -> nextCall (s.[0]|> mapKeyboardChar) s "X"

let preProcess (str:string) = str.TrimStart('@', ' ')
