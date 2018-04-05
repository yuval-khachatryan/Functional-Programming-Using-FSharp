(* Solutions for chapter 3 - exercise 3 to 7 *)

(* Exercise 3 - Complex numbers *)
type Complex = {Real: float; Imaginary: float}

// addition and multiplication
let (.+.) z1 z2 = {Real = z1.Real + z2.Real; Imaginary = z1.Imaginary + z2.Imaginary}
let (.*.) z1 z2 = {Real = z1.Real * z2.Real - z1.Imaginary * z2.Imaginary;
                   Imaginary = z1.Real * z2.Imaginary + z1.Imaginary * z2.Real}


// inverse operators
// additive inverse
let (~-.) z = {Real = -z.Real; Imaginary = -z.Imaginary}

let inverse z = 
    let normSquared = z.Real * z.Real + z.Imaginary * z.Imaginary
    {Real = z.Real / normSquared; Imaginary = -z.Imaginary / normSquared}

// substraction and division
let (.-.) z1 z2 = z1 .+. (-. z2)
let (./.) z1 z2 = z1 .*. (inverse z2)

// exercise 4 - stright line ax + b
// declares a function to mirror stright line across x and y axes
// and representation of stright line as a string
type StrigthLine = float * float

let mirrorX (a, b) = (-a, -b)
let mirrorY (a, b) = (-a, b)
let toString (a, b) = (string a) + "x + " + (string b)

// Exercise 5 - Solves quadratic equation with solution having a type that captures possibilities for solutioins
// namely: no roots, one root, two roots and corresponding solve function
// Solution of quadratic eqation caputuring the options
type Solution = TwoRoots of float * float | OneRoot of float | NoRoots
type QuadraticEquation = float * float * float

let solve (a, b, c) = 
    let disc = b * b - 4.0 * a * c
    match disc with
    | 0.0 -> OneRoot (-b / (2.0 * a) )
    | _ when disc > 0.0 -> TwoRoots ( (-b + System.Math.Sqrt disc) / (2.0 * a), (-b - System.Math.Sqrt disc) / (2.0 * a)  ) 
    | _ -> NoRoots

/// Exercise 6 - Sort of has been done... Look at exercise 1,2 solutions

/// Exercise 7
type Shape = | Circle of float
             | Square of float
             | Triangle of float * float * float

let isShape s =
    match s with
    | Circle r -> r > 0.0
    | Square s -> s > 0.0
    | Triangle (a,b,c) ->
        a > 0.0 && b > 0.0 && c > 0.0 
        && a < b + c && b < a + c && c < a + b

let area = 
    function
    | Circle r when r > 0.0 -> System.Math.PI * r * r
    | Square s when s > 0.0 -> s * s
    | Triangle (a, b, c) 
        when a > 0.0 && b > 0.0 && c > 0.0 
        && a < b + c && b < a + c && c < a + b ->
            let s = (a + b + c) / 2.0
            sqrt (s * (s - a) * (s - b) * (s - c))
    | _ -> failwith "not a legal shape"