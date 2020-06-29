open ProjectEulerFSharp

let printAnswer (solutions : (unit -> unit) []) n =
    printf "Problem %i answer: " n
    solutions.[n - 1] ()

let printAnswers (solutions : (unit -> unit) []) =
    List.iter (printAnswer solutions) [1 .. solutions.Length]

let printCurrentAnswer (solutions : (unit -> unit) []) =
    printAnswer solutions <| solutions.Length

[<EntryPoint>]
let main argv =
    printCurrentAnswer AnonymousSolutions.Solutions
    0


