module ProjectEulerFSharp.AnonymousSolutions

let public Solutions =
    [|
        fun () ->
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
        
        fun () ->
            let limit = 4000000
            let fib n = 1.0 / sqrt 5.0 * (pown ((1.0 + sqrt 5.0) / 2.0) n - pown ((1.0 - sqrt 5.0) / 2.0) n) |> round |> int
            let rec recHelper acc i =
                let currentFib = fib i
                if currentFib > limit then acc
                else recHelper (acc + currentFib) (i + 3)
            recHelper 0 0
        
        fun () ->
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

        fun () ->
            let isPalindrome n =
                let s = n.ToString ()
                let rec recHelper i acc =
                    if i > s.Length / 2 then acc
                    else
                        acc && s.[i] = s.[s.Length - 1 - i]
                        |> recHelper (i + 1)
                recHelper 0 true
            
            let upperLimit = 999
            let rec recHelper int1 int2 stop best =
                if int1 < stop then best
                else if int2 < stop then recHelper (int1 - 1) (int1 - 1) stop best
                else
                    let product = int1 * int2
                    if product |> isPalindrome then
                        let newBest = max best product
                        recHelper int1 (int2 - 1) (newBest |> float |> sqrt |> int) newBest
                    else
                        recHelper int1 (int2 - 1) stop best
            recHelper upperLimit upperLimit 0 0

        fun () ->
            let limit = 20
            let getMultiple x =
                log (float limit) / log (float x)
                |> int
                |> pown x
            let rec recHelper i primes acc =
                if i > limit then acc
                else if List.forall (fun x -> i % x <> 0) primes then
                    getMultiple i
                    |> Checked.(*) acc
                    |> recHelper (i + 1) (i :: primes)
                else
                    recHelper (i + 1) primes acc
            recHelper 2 [] 1
    |]