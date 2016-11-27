module Quiz.Tests.TextToKeyboardTests

open System
open NUnit.Framework
open Swensen.Unquote
open Quiz
open Quiz.TextToKeyboard

[<TestCase("Tab")>]
let ``string starts with tab`` str =
    str |> getConsoleKeys [] =! [Ck ck.Tab]

[<TestCase("Escape")>]
let ``text expected answer starts with Esc`` str =
   str |> getConsoleKeys [] =! [Ck ck.Escape] 
  
[<TestCase("ffeferf", 3, "ferf")>]
[<TestCase("", 0, "")>]
[<TestCase("", 20, "")>]
let ``trim start of the string`` str len expected =
    trimString str len =! expected

[<TestCase("TabEscape")>]
let ``first Tab then Escape`` str = 
    str |> getConsoleKeys [] =! [Ck ck.Tab; Ck ck.Escape]

[<TestCase("EscapeTab")>]
let ``first Escape then Tab`` str =
    str |> getConsoleKeys [] =! [Ck ck.Escape; Ck ck.Tab]

[<TestCase("EscapeEscapeEscape")>]
let ``triple Escape`` str =
    str |> getConsoleKeys [] =! [Ck ck.Escape; Ck ck.Escape; Ck ck.Escape]

[<TestCase("")>]
let ``entire answer string has been processed, call base case`` str =
    str |> getConsoleKeys [Ck ck.A] =! [Ck ck.A]

[<TestCase("Tab")>]
let ``accumulate Tab`` str =
    str |> getConsoleKeys [Ck ck.A] =! [Ck ck.A; Ck ck.Tab]

[<TestCase("Ctrl-B")>]
let ``ctrl key`` str =
    str |> getConsoleKeys [] =! [Cm (cm.Control, ck.B)]

[<TestCase("Ctrl-B", "B")>]
[<TestCase("Ctrl-F", "F")>]
[<TestCase("Ctrl-R", "R")>]
let ``retrive ctrl key`` str expected =
    retrieveCtrlKey str =! Enum.Parse(typeof<ConsoleKey>, expected)

[<TestCase("Ctrl-F")>]
let ``starts with ctrl key``str =
    str |> getConsoleKeys [] =! [Cm (cm.Control, ck.F)]

[<TestCase>]
let ``answer keys starts with ConsoleKey`` () =
    "a" |> getConsoleKeys [] =! [Ck ck.A]

[<Test>]
let ``getting digits chars`` () = getDigits() |> List.exists (fun e -> e = '2') =! true
