module ProjectEulerFSharp.AnonymousSolutions

open System.Collections
open System.Collections.Generic

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
        
        fun () ->
            let input =
                "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
                49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
                81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
                52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
                22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
                32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
                24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
                78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
                16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
                86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
                19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
                04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
                88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
                04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
                20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
                20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
                01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
            let productLength = 4
            let filteredInput =
                input |> Seq.filter (fun x -> '0' <= x && x <= '9')
            let inputSize = Seq.length filteredInput / 2
            let sideLength = float inputSize |> sqrt |> int
            let inputArray =
                Seq.splitInto inputSize filteredInput
                |> Seq.map (fun x -> (x.[0] |> char |> string) + (x.[1] |> char |> string) |> int)
                |> Seq.splitInto sideLength
                |> array2D
            
            let addVertical listOfLists =
                List.fold (fun currentList currentNum -> (Seq.cast inputArray.[currentNum, *] |> Seq.toList) :: currentList) listOfLists [0 .. sideLength - 1]
            
            let addHorizontal listOfLists =
                List.fold (fun currentList currentNum -> (Seq.cast inputArray.[*, currentNum] |> Seq.toList) :: currentList) listOfLists [0 .. sideLength - 1]
            
            let rec addDiagonal1 (startY, startX) listOfLists =
                let rec getNextDiagonal (currentY, currentX) numList =
                    if currentY < 0 || currentX >= sideLength then
                        numList
                    else
                        getNextDiagonal (currentY - 1, currentX + 1) (inputArray.[currentY, currentX] :: numList)
                
                if startX > sideLength - productLength then
                    listOfLists
                else if startY <> sideLength - 1 then
                    (getNextDiagonal (startY, startX) []) :: listOfLists
                    |> addDiagonal1 (startY + 1, startX)
                else
                    (getNextDiagonal (startY, startX) []) :: listOfLists
                    |> addDiagonal1 (startY, startX + 1)
            
            let rec addDiagonal2 (startY, startX) listOfLists =
                let rec getNextDiagonal (currentY, currentX) numList =
                    if currentY >= sideLength || currentX >= sideLength then
                        numList
                    else
                        getNextDiagonal (currentY + 1, currentX + 1) (inputArray.[currentY, currentX] :: numList)
                
                if startX > sideLength - productLength then
                    listOfLists
                else if startY <> 0 then
                    (getNextDiagonal (startY, startX) []) :: listOfLists
                    |> addDiagonal2 (startY - 1, startX)
                else
                    (getNextDiagonal (startY, startX) []) :: listOfLists
                    |> addDiagonal2 (startY, startX + 1)
            
            let listOfLists =
                addVertical []
                |> addHorizontal
                |> addDiagonal1 (productLength - 1, 0)
                |> addDiagonal2 (sideLength - productLength, 0)
            // printfn "%A" listOfLists
            let maxProductInList maxSeen listOfNums =
                let rec recHelper maxSeen listOfNums (numbers : Queue<int>) currentProduct=
                    if listOfNums = [] then
                        maxSeen
                    else if listOfNums.Head = 0 then
                        numbers.Clear ()
                        recHelper maxSeen listOfNums.Tail numbers 1
                    else
                        let nextProduct =
                            if numbers.Count < productLength then
                                currentProduct * listOfNums.Head
                            else
                                numbers.Dequeue ()
                                |> (/) currentProduct
                                |> (*) listOfNums.Head
                        
                        numbers.Enqueue listOfNums.Head
                        if numbers.Count = productLength then
                            recHelper (max maxSeen nextProduct) listOfNums.Tail numbers nextProduct
                        else
                            recHelper maxSeen listOfNums.Tail numbers nextProduct
                recHelper 0 listOfNums (Queue<int> productLength) 1
                |> max maxSeen
            List.fold maxProductInList 0 listOfLists
            |> printfn "%A"
    |]