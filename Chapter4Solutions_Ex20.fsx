(* Solutions to exercises 20 - 23 in chapter 4 of Functional programming using F# *)

type Country = string
type Map = (Country * Country) list
type Colour = Country list
type Colouring = Colour list


// Helpper functions for exercise 20
let rec isMember elem lst = 
    match lst with
    | [] -> false
    | x::xs when x = elem -> true
    | _::xs -> isMember elem lst


let rec extractCountries map = 
    let addElem x ys = if isMember x ys then ys else (x::ys) 
    match map with
    | [] -> []
    | (c1, c2)::c -> addElem c1 (addElem c2 (extractCountries c))

// Exercise 20
let colMap map =
    // checks whether two countries are neighbors in the map
    let areNb country1 country2 = isMember (country1, country2) map || isMember (country2, country1) map

    // checks whehter country has a neighour of given colour
    let rec canBeExtBy colour country = 
        match colour with
        | [] -> true
        | country'::countries -> not (areNb country country') && (canBeExtBy countries country)
    
    // extends the colouring
    let rec extColouring colouring country =
        match colouring with
        | [] -> [[country]]
        | colour::colours -> if canBeExtBy colour country
                             then (country::colour)::colours
                             else colour::(extColouring colours country)
    
    // colours the countries
    let rec colCountries = function
        | [] -> []
        | country::countries -> extColouring (colCountries countries) country
    
    colCountries (extractCountries map)
