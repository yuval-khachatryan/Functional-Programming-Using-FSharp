// Exercises in chapter 1 (most of them...)

// Exercise 1
let g n = n + 4

// exercise 2
let h (x,y) = System.Math.Sqrt (x * x + y * y)

// g and h as funcion expressions
let gf = function
    | n -> n + 4

let hf = function
    | (x, y) -> System.Math.Sqrt (x * x + y * y)

// sum function exercise 4
let rec sum = function
    | 0 -> 0
    | n -> n + sum (n - 1)

// sum function exercise 5
let rec fib = 
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib (n-2)

// tail recursive version of fibonacci
let fibt =
    let rec iter = 
        function
        | (0L, current, prev) -> current
        | (n, current, prev) -> iter (n-1L, current + prev, current)
    function
    | 0L -> 0L
    | 1L -> 1L
    | n -> iter ( (n-2L, 1L, 1L) )

// sum from pair - exercise 6
let rec sump = 
    function
    | (m, 0) -> m
    | (m, n) -> m + sump (m+1, n-1)
