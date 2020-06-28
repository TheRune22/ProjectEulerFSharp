open ProjectEulerFSharp

let printAnswer (solutions : (unit -> int) []) n =
    printfn "Problem %i answer: %i" n <| solutions.[n - 1] ()

let printAnswers (solutions : (unit -> int) []) =
    List.iter (printAnswer solutions) [1 .. solutions.Length]

let printCurrentAnswer (solutions : (unit -> int) []) =
    printAnswer solutions <| solutions.Length

[<EntryPoint>]
let main argv =
    printCurrentAnswer AnonymousSolutions.Solutions
    0


