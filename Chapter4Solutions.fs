open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001
open System.Runtime.Remoting.Metadata.W3cXsd2001

(* Solutions to chapter 4 *)

// a helper function used in following exercises
let rec makeSequence length element updateElement temp =
    match length with
    | _ when length <= 0 -> temp
    | _ -> makeSequence (length-1) (updateElement element) updateElement (element::temp)


// Exercise 1 : declare function upto : int -> int list such that upto n = [1; 2; ... ;n]
let upto n = makeSequence n n (fun n -> n-1) []

// Exercise 2: declare function downto : int -> int list such that downto1 n = [n; n-1; ...; 1]
let downto1 n = makeSequence n 1 (fun n -> n+1) []

// Exercise 3: declare function evenN n which generates the list of first n non-negative numbers
let evenN n = makeSequence n (2 * n) (fun n -> n-2) [] 

// Exercise 4: declaration of alternating sum using only two clauses
let rec altsum = 
    function 
    | [] -> 0
    | x::xs -> x - (altsum xs)

// Exercise 5: Remove odd numbered elements from the list
let rec rmodd lst = 
    match lst with
    | [] -> []
    | [x] -> [x]
    | x0::x1::xs -> x0 :: (rmodd xs)
 
 // tail recursive version
let rec rmodd2 lst = 
    let rec reverseOdd tmp =
        function
        | [] -> tmp
        | [x] -> x::tmp
        | x0::x1::xs -> reverseOdd (x0::tmp) xs
    let rec reverse tmp =
        function
        | [] -> tmp
        | x::xs -> reverse (x::tmp) xs
    lst |> reverseOdd [] |> reverse []

// Exercise 6 - remove odd numbers from an integer list
// easily done using filter
let rec removeEvens lst = 
    match lst with
    | [] -> []
    | x::xs when x % 2 = 0 -> removeEvens xs
    | x::xs -> x :: (removeEvens xs)

// Exercise 7 - find number of occurences of x in a list xs
let multiplicity x xs =
    let rec iter e l r =
        match l with
        | [] -> r
        | x::xs when x = e -> iter e xs (1+r)
        | _::xs -> iter e xs r
    iter x xs 0

// Exercise 8 - split function - split a list into a pair of even and odd numbered elements
let split lst = 
    match lst with
    | [] -> ([], [])
    | x::xs -> (rmodd2 (x::xs), rmodd2 xs)

// split function without using predefined functions

let rec split2 lst = 
    let append a b (x, y) = (a::x, b::y) 
    match lst with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x0::x1::xs -> append x0 x1 (split2 xs) 

// Exercise 9 - zip function such that
// zip([x0 ;x1 ; . . . ;xn−1 ],[y0 ;y1 ; . . . ;yn−1 ]) = [(x0 , y0 );(x1 , y1 ); . . . ;(xn−1 , yn−1 )]
let rec zip (lst1, lst2) =
    match (lst1, lst2) with
    | ([], []) -> ([])
    | (_::_, []) | ([], _::_) -> failwith "Lists don't have equal length"
    | (x::xs, y::ys) -> ( (x, y) :: zip (xs, ys) )

//Exercise 10 - prefix function. 
//The value of the expression prefix [x0 ;x1 ; . . . ;xm ] [y0 ;y1 ; . . . ;yn ] is true if m ≤ n
//and xi = yi for 0 ≤ i ≤ m, and false otherwise.
let rec prefix lst1 lst2 = 
    match (lst1, lst2) with
    | ([], _) -> true
    | (x::xs, y::ys) when x = y -> prefix xs ys
    | _ -> false

