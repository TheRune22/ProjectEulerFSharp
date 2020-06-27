﻿module ProjectEulerFSharp.Solutions

let public Problem1Solution () =
    let int1 = 3
    let int2 = 5
    let limit = 1000
    let maxMult1 = (limit - 1) / int1
    let maxMult2 = (limit - 1) / int2
    let integerSum n =
        (n * (n + 1)) / 2
    let union =
        integerSum (maxMult1 / int2) * int2 * int1
    integerSum maxMult1 * int1 + integerSum maxMult2 * int2 - union

let public Problem2Solution () =
    let limit = 4000000
    let fib n = 1.0 / sqrt 5.0 * (pown ((1.0 + sqrt 5.0) / 2.0) n - pown ((1.0 - sqrt 5.0) / 2.0) n) |> round |> int
    let rec recHelper acc i =
        let currentFib = fib i
        if currentFib > limit then acc
        else recHelper (acc + currentFib) (i + 3)
    recHelper 0 0

let public Problem3Solution () =
    let n = 600851475143UL
    let rec recHelper current div =
        if current % div = 0UL then
            let next = current / div
            if next = 1UL then
                div
            else recHelper next div
        else
            recHelper current (div + 1UL)
    recHelper n 2UL |> int

let public Problem4Solution () =
    0

let public Solutions =
    [|
        Problem1Solution;
        Problem2Solution;
        Problem3Solution;
        Problem4Solution
    |]

let public PrintAnswer n =
    printfn "Problem %i Answer: %i" n <| Solutions.[n - 1] ()

let public PrintAnswers () =
    List.iter PrintAnswer [1 .. Solutions.Length]

let public PrintCurrentAnswer () =
    PrintAnswer <| Solutions.Length