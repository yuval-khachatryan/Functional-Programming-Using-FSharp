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
let reverse lst =
    let rec iter tmp =
        function
        | [] -> tmp
        | x::xs -> iter (x::tmp) xs
    iter [] lst

let rec rmodd2 lst = 
    let rec reverseOdd tmp =
        function
        | [] -> tmp
        | [x] -> x::tmp
        | x0::x1::xs -> reverseOdd (x0::tmp) xs
    lst |> reverseOdd [] |> reverse 

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

// Exercise 11 - Weakly accending lists - all functions assume that we work with accending lists - no error checking in functions themselves

//checks whether list is weakly assceninding
let rec isAscending lst = 
    match lst with
    | x0::x1::xs when x0 > x1 -> false
    | x0::x1::xs when x0 <= x1 -> isAscending (x1::xs)
    | _ -> true

// part 1 calculate occurences of element x in list xs
let count (xs, x) = 
    let rec iter lst elem temp = 
        match lst with
        | x::xs when x < elem -> iter xs elem temp
        | x::xs when x = elem -> iter xs elem (temp + 1)
        | _ -> temp
    iter xs x 0

// part 2 insert element into list
let rec insert (xs, x) = 
    match xs with
    | y::ys when y < x -> y::(insert (ys, x) )
    | y::ys when x <= y -> x::y::ys
    | _ -> [x]

// tail recursive version
let insert2 (xs, x) = 
    let rec iter lst elem tmp = 
        match lst with 
        | [] -> reverse (elem::tmp)
        | y::ys when y < elem -> iter ys elem (y::tmp)
        | _ -> (reverse tmp) @ (elem::lst)
    iter xs x []

// part 3 - intersection of two lists
let rec intersect (lst1, lst2) = 
    match (lst1, lst2) with
    | (x::xs, y::ys) when x = y -> x::(intersect (xs, ys) )
    | (x::xs, y::ys) when x < y -> intersect (xs, y::ys)
    | (x::xs, y::ys) when x > y -> intersect (x::xs, ys)
    | _ -> []

// tail recursive version
let intersect2 (lst1, lst2) = 
    // return intersection in descending order
    let rec iter lst1 lst2 tmp = 
        match (lst1, lst2) with
        | (x::xs, y::ys) when x = y -> iter xs ys (x::tmp)
        | (x::xs, y::ys) when x < y -> iter xs lst2 tmp
        | (x::xs, y::ys) when x > y -> iter lst1 ys tmp
        | _ -> tmp
    iter lst1 lst2 [] 
    |> reverse

// part 4 - "union" of two weakly ascending lists - viewed as multisets
let plus (lst1, lst2) =
    // return intersection in descending order
    let rec iter lst1 lst2 tmp = 
        match (lst1, lst2) with 
        | (x::xs, y::ys) when x <= y -> iter xs ys (y::x::tmp)
        | (x::xs, y::ys) -> iter xs ys (x::y::tmp)
        | _ -> tmp
    // reverse the result from the previous function
    iter lst1 lst2 [] 
    |> reverse

// part 5 - remove elements of one list from another
let rec minus (lst1, lst2) = 
    let rec iter lst1 lst2 tmp = 
    // return the minus set in descending order
        match (lst1, lst2) with
        | (x::xs, y::ys) when x = y -> iter xs ys tmp
        | (x::xs, y::ys) when x < y -> iter xs (y::ys) (x::tmp)
        | (x::xs, y::ys) when x > y -> iter (x::xs) ys tmp
        | (x::xs, []) -> iter xs [] (x::tmp)
        | (_, _) -> tmp
    // reverse the order of the result 
    iter lst1 lst2 [] |> reverse

// Exercise 12 - sums a elements of list xs satisfying the preidicate p
let sum (p, xs) = 
    let rec iter pr lst res = 
        match lst with 
        | [] -> res
        | y::ys when pr y -> iter pr ys (res + y)
        | _::ys -> iter pr ys res
    iter p xs 0

// Exercise 13 - Naive list sort function

// part 1 - find the smallest element
let smallest : (int list -> int) = 
    let rec iter xs m = 
        match xs with 
        | [] -> m
        | y::ys when y < m -> iter ys y
        | _::ys -> iter ys m
    function 
    | [] -> failwith "No smallest element in empty list"
    | x::xs -> iter xs x

// part 2 delete the smallest element
let delete (x, xs) = 
    let rec iter x xs tmp = 
        match xs with 
        | [] -> tmp
        | y::ys when y = x -> (reverse tmp) @ ys
        | y::ys -> iter x ys (y::tmp)
    iter x xs []

// naive sort function using the previous two  routinines
let naiveSort = 
    let rec iter lst tmp = 
        match lst with 
        | [] -> reverse tmp
        | _ -> let s = smallest lst in iter (delete (s, lst)) (s::tmp)
    fun lst -> iter lst []

// Exercise 14 - Find dmallest element in integer list
let safeSmallest lst = 
    try Some (smallest lst)
    with 
    | _ -> None

// Exercise 15 - reverse a list of lists
let revrev lst = 
    let rec iter l tmp = 
        match l with 
        | [] -> tmp
        | x::xs -> iter xs (reverse x :: tmp)
    iter lst []
    
// Exercise 16
// part 1
// explanation f (x, [y0;...;yn]) returns new list ([y0+x, y0+x-1;...; y0+x-n])
let rec f = function
    | (x, []) -> []
    | (x, y::ys) -> (x+y)::f(x-1, ys);;

// part 2
// explanation g [(x0, x1); ...; (x_n, y_n)] returns
// [(x0, y0); (y0, x0); ... ; (x_n, y_n); (y_n, x_n)]
let rec g = function 
    | [] -> []
    | (x,y)::s -> (x,y)::(y,x)::g s

// part 3 
// explanation: appends to the end of the list the list in reversed order
let rec h = function 
    | [] -> []
    | x::xs -> x:: (h xs) @ [x]

// Exercise 18
// explanation: 
// the type is ('a -> bool) -> 'a list -> 'a list
// the function makes a new list out of the given list xs
// such that its first elements are elements of xs that satisfy p
// and last elements are elemsnts of xs that don't satisfy p in reversed order 
let rec p q = function 
    | [] -> []
    | x::xs -> let ys = p q xs
               if q x then x::ys else ys@[x]

// exercise 19 - f is replaced by k because of the name collision
// explanation:
// type: ('a -> 'a) -> 'a list -> 'a list
// apply g g^2 g^4 g^8... and so on on the elements of list
let rec k g = function
    | []    -> []
    | x::xs -> g x :: k (fun y -> g (g y )) xs

// Exercise 19
// If isMember (c1, c2) m is false we need to call is Member again...
// In order to fix that then when we traverse the map we need to check
// for both countries... But worse case is still the same ... namely if the map
// is the last one 
let rec areNb m c1 c2 = 
    match m with 
    | [] -> false 
    | (x, y)::_ when (x = c1 && y = c2) || (x = c2 && y = c1) -> true 
    | _::xys -> areNb xys c1 c2
