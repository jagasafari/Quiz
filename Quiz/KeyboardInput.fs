module Quiz.KeyboardInput

open System
open TextToKeyboard

let isControlModifier (keyInfo: ConsoleKeyInfo) =
    (keyInfo.Modifiers &&& ConsoleModifiers.Control) = ConsoleModifiers.Control

let isShiftModifier (keyInfo: ConsoleKeyInfo) =
    (keyInfo.Modifiers &&& ConsoleModifiers.Shift) = ConsoleModifiers.Shift

let isModifier (keyInfo: ConsoleKeyInfo) =
    isControlModifier keyInfo || isShiftModifier keyInfo

let getModifier (keyInfo: ConsoleKeyInfo) =
    match keyInfo with
    | x when isShiftModifier x -> ConsoleModifiers.Shift
    | _ -> ConsoleModifiers.Control

let readKeys (readKey: unit -> ConsoleKeyInfo) =
    let rec getEnteredKeys keys =
        let key = readKey()
        match key with
        | x when isModifier x ->
            let modifier = getModifier x
            getEnteredKeys (keys@[Cm (modifier, key.Key)])
        | x when x.Key <> ConsoleKey.Enter ->
            getEnteredKeys (keys@[Ck key.Key])
        | _ -> keys

    getEnteredKeys []

let readKey() = Console.ReadKey()

let readKeysUntilEnter() = readKeys readKey

