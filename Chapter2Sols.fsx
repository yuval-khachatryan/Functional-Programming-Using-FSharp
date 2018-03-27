(* Solutions for chapter 2 in Functional programming using F#
   Exercises numbers are given in comments before the functions *)

// ex 1
let f n = (n % 2 = 0 || n % 3 = 0) && (n % 5 <> 0)

// ex 2
let rec powstr (str, n) = 
    match n with
    | _ when n < 0 -> ""
    | 0 -> ""
    | 1 -> str
    | _ when n % 2 = 0 -> powstr (str, n/2) + powstr (str, n/2)
    | _ -> powstr(str, n/2) + powstr (str, n/2 + 1)

// ex 2 - tail recursive version
let tailrecPowStr = 
    let rec iter (str, remainder, n) = 
        match n with
        | _ when n <= 0 -> remainder
        | n when n % 2 = 0 -> iter (str + str, remainder, n / 2)
        | _ -> iter (str, remainder + str, n - 1)
    fun (str, n) -> iter (str, "", n)

// ex 3 - returns whether ch is i-th char of the string str
let isIthChar (str:string, i, ch) = 
    ch = str.[i]

// ex 4 - returns number of occurences of the character ch in string str 
// in positions with j > i
let occFromIth (str, i, ch) = 
    let rec iter (str, i, ch, acc) = 
        match i with
        | _ when i >= String.length str -> acc
        | _ when isIthChar (str, i, ch) -> iter (str, i+1, ch, acc+1)
        | _ -> iter (str, i+1, ch, acc)
    iter (str, i, ch, 0)

// ex 5 - returns the number of occurences of the characer ch in string str
let occInString (str, ch) = occFromIth (str, 0, ch)

// ex 6 - returns whether number n is not divisible by d
let notDivisible (d, n) = not (n % d = 0)

// ex 7 - three parts
// part a - checks whether all numbers in range a <= x <= b are **not** divisors of c
let test (a,b,c) = 
    let rec iter (d, status) =
        match (d, status) with
        | (_, false) -> false
        | (_, _) when d > b -> true
        | (_, _) -> iter (d+1, notDivisible (d, c) && status )
    iter (a, true)

// helper function - finds square root of integer
let intSqrt n = 
    n |> float |> System.Math.Sqrt |> int

// part b check whether number is prime
let prime n =
    match n with 
    | 0 | 1 -> false
    | 2 -> true // border case
    | _ -> test (2, (intSqrt n) + 1, abs n) 

// part c find smallest prime larger than n
let rec nextPrime n = 
    if prime (n+1) then (n+1)
    else nextPrime (n+1)

// exercise 8
// generate binomial coefficients - naive version
let rec bin (n, k) = 
    if k < 0 || k > n then 0
    elif k = 0 || k = n then 1
    else bin (n-1, k-1) + bin (n-1, k)

// the following versions are using idioms and techniques not covered in the book - I've added them for fun
// memorization version - using 2d array
let  fastBin n =
    let results = Array2D.init (n+1) (n+1) (fun _ _ -> -1L)
    let rec lookup m k = 
        if k < 0 || k > n then 0L
        elif results.[m, k] > -1L then
             results.[m, k]
        elif k > m then
             results.[m, k] <- 0L
             results.[m, k]
        elif k = 0 || k = m then
             results.[m, k] <- 1L
             results.[m, k]
        else results.[m, k] <- (lookup (m-1) k) + (lookup (m-1) (k-1))
             results.[m, k]
    fun k -> lookup n k

// hashmap version
let hashFastBin =
    let results = new System.Collections.Generic.Dictionary<int * int, int64>()
    let rec lookup m k = 
        if k < 0 || m < k then 0L
        elif k = 0 || m = k then 1L
        else let mutable res = -1L
             let foundIt = results.TryGetValue((m, k), &res)
             if foundIt then res
             else res <- (lookup (m-1) (k-1) ) + (lookup (m-1) k)
                  results.Add((m, k), res)
                  res
    fun (n, k) -> lookup n k

// resize array version
let resArrayFastBin = 
    let results = new ResizeArray<int64 []>()
    let rec lookup m k = 
        // check whether values for m have been calculated
        if results.Count < m + 1 then
            for i in results.Count .. m do
                results.Add (Array.init (i+1) (fun _ -> -1L) )
        if k < 0 || m < 0 || m < k then 0L
        elif results.[m].[k] > -1L then results.[m].[k]
        elif k = 0 || m = k then 
            results.[m].[k] <- 1L
            1L
        else results.[m].[k] <- (lookup (m-1) (k-1) ) + (lookup (m-1) k)
             results.[m].[k]
    fun (n, k) -> lookup n k

// Exercise 2 10 should either fail to halt or result in integer overflow. The reason it happens is because
// fact(-1) is evaluated before it is passed to function

// Exercises 11 - increase and decrease float x by n percent
let VAT n x = 
    n |> float
      |> (fun a -> 1.0 + a / 100.0) 
      |> (fun a -> x * a) 

//inverse function of VAT
let unVAT n x = 
    n |> float 
      |> (fun a -> 1.0 + a / 100.0)
      |> (fun a -> 1.0 / a)
      |> (fun a -> x * a)

// exercose 12 - find minimal n such that f(n) = 0.
// I am not sure what the author's meant - but unless
// there are not other assumptions about the function - then the only
// correct Solution is to scan all the possible integers and return the first one that vives 0
let min f =
    let rec iter = 
        function
        | System.Int32.MaxValue -> System.Int32.MaxValue
        | n when (f n = 0) -> n
        | n -> iter (n+1)
    iter 0

// exercise 13 currying and uncurrying

// curry f is the function g where g x is the function h where h y = f (x, y).
let curry f  = 
    (fun x -> (fun y -> f (x,y) ))

// uncurry g is the function f where f (x, y) is the value h y for the function h = g x.
let uncurry g = 
    fun (x, y) -> g x y
