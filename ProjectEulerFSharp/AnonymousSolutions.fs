module ProjectEulerFSharp.AnonymousSolutions

open System

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
            |> printfn "%i"
        
        fun () ->
            let limit = 4000000
            let fib n = 1.0 / sqrt 5.0 * (pown ((1.0 + sqrt 5.0) / 2.0) n - pown ((1.0 - sqrt 5.0) / 2.0) n) |> round |> int
            let rec recHelper acc i =
                let currentFib = fib i
                if currentFib > limit then acc
                else recHelper (acc + currentFib) (i + 3)
            recHelper 0 0
            |> printfn "%i"
        
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
            recHelper n 2UL
            |> int
            |> printfn "%i"

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
            |> printfn "%i"

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
            |> printfn "%i"
        
        fun () ->
            let n = 100
            let sumOfSquares = n * (n + 1) * (2 * n + 1) / 6
            let squareOfSum = pown ((n * (n + 1)) / 2) 2
            squareOfSum - sumOfSquares
            |> printfn "%i"
        
        fun () ->
            let targetPrime = 10001
            let arraySize = 1000
            let rec recHelper start numPrimes (primes : int list) =
                if numPrimes = targetPrime then primes.Head
                else
                    // use List instead?
                    let arePrime = Array.create arraySize true
                    let iterator prime =
                        let startIndex = (prime - start % prime) % prime
                        if startIndex > arraySize - 1 then ()
                        else
                            List.iter (fun x -> arePrime.[x] <- false) [startIndex .. prime .. arraySize - 1]
                    List.iter iterator primes
                    let rec getPrimes numPrimes (primes : int list) =
                        if numPrimes = targetPrime then (numPrimes, primes)
                        else
                            let nextPrimeOption = Option.bind (fun x -> Some (x + start)) (Array.tryFindIndex id arePrime)
                            if nextPrimeOption.IsNone then (numPrimes, primes)
                            else
                                let nextPrime = nextPrimeOption.Value
                                arePrime.[nextPrime - start] <- false
                                // let startIndex = (float nextPrime ** 2.0) - float start |> round |> int
                                let startIndex = pown (uint64 nextPrime) 2 - uint64 start |> int
                                if startIndex > arraySize - 1 || startIndex < 0 then
                                    ()
                                else
                                    List.iter (fun x -> arePrime.[x] <- false) [startIndex .. nextPrime .. arraySize - 1]
                                getPrimes (numPrimes + 1) (nextPrime :: primes)
                    let nextNumPrimes, nextPrimes = getPrimes numPrimes primes
                    recHelper (start + arraySize) nextNumPrimes nextPrimes
            recHelper 2 0 []
            |> printfn "%i"
        
        fun () ->
            let number =
                "73167176531330624919225119674426574742355349194934
                96983520312774506326239578318016984801869478851843
                85861560789112949495459501737958331952853208805511
                12540698747158523863050715693290963295227443043557
                66896648950445244523161731856403098711121722383113
                62229893423380308135336276614282806444486645238749
                30358907296290491560440772390713810515859307960866
                70172427121883998797908792274921901699720888093776
                65727333001053367881220235421809751254540594752243
                52584907711670556013604839586446706324415722155397
                53697817977846174064955149290862569321978468622482
                83972241375657056057490261407972968652414535100474
                82166370484403199890008895243450658541227588666881
                16427171479924442928230863465674813919123162824586
                17866458359124566529476545682848912883142607690042
                24219022671055626321111109370544217506941658960408
                07198403850962455444362981230987879927244284909188
                84580156166097919133875499200524063689912560717606
                05886116467109405077541002256983155200055935729725
                71636269561882670428252483600823257530420752963450"
            let windowSize = 13
            Seq.map uint64 number
            |> Seq.filter (fun x -> uint64 '0' <= x && x <= uint64 '9')
            |> Seq.windowed windowSize
            |> Seq.map (Array.fold (fun acc x -> x - uint64 '0' |> Checked.(*) acc) 1UL)
            |> Seq.max
            |> printfn "%i"
        
        fun () ->
            let targetSum = 1000
            let rec recHelper b =
                let a = targetSum * (2 * b - targetSum) / (2 * b - 2 * targetSum)
                let c = (2 * b * targetSum - 2 * pown b 2 - pown targetSum 2) / (2 * b - 2 * targetSum)
                let sum = a + b + c
                if sum = targetSum then a * b * c
                else recHelper (b + 1)
            recHelper 1
            |> printfn "%i"
        
        fun () ->
            // TODO: optimize (sqrt limit as max, filter multiples of small primes (2, 3))
            let limit = 2000000
            let arraySize = 1000
            let rec recHelper start sum (primes : int list) =
                if start > limit then sum
                else
                    // use List instead?
                    let arePrime = Array.create arraySize true
                    let iterator prime =
                        let startIndex = (prime - start % prime) % prime
                        if startIndex > arraySize - 1 then ()
                        else
                            List.iter (fun x -> arePrime.[x] <- false) [startIndex .. prime .. arraySize - 1]
                    List.iter iterator primes
                    let rec getPrimes sum (primes : int list) =
                        let nextPrimeOption = Option.bind (fun x -> Some (x + start)) (Array.tryFindIndex id arePrime)
                        if nextPrimeOption.IsNone || nextPrimeOption.Value > limit then (sum, primes)
                        else
                            let nextPrime = nextPrimeOption.Value
                            arePrime.[nextPrime - start] <- false
                            // let startIndex = (float nextPrime ** 2.0) - float start |> round |> int
                            let startIndex = pown (uint64 nextPrime) 2 - uint64 start |> int
                            let stopIndex = min (arraySize - 1) (limit - start - 1)
                            if startIndex > stopIndex || startIndex < 0 then
                                ()
                            else
                                List.iter (fun x -> arePrime.[x] <- false) [startIndex .. nextPrime .. stopIndex]
                            getPrimes (Checked.(+) sum <| uint64 nextPrime) (nextPrime :: primes)
                    let nextSum, nextPrimes = getPrimes sum primes
                    recHelper (start + arraySize) nextSum nextPrimes
            recHelper 2 0UL []
            |> printfn "%i"
    |]