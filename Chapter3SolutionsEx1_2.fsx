(* Solutions for chapter 3 - this is exercise 1 and 2 - the files is split to avoid new unnecessary operators*)

// Exercise 1 - tuple version
type TimeTuple = int * int * string

// checks whether we a legal time
let isLegal (hh, mm, ampm) =
    (1 <= hh && hh <= 12) && (0 <= mm && mm <= 59) && (ampm = "AM" || ampm = "PM")

// checkes whether first time1 comes before time2
// assumes we have valid times already
let (.<.) time1 time2 = 
    match (time1, time2) with
    | (_, _) when (not (isLegal time1) ) || (not (isLegal time2) ) -> failwith "Illegal time argument"
    | (_, _, "AM"), (_, _, "PM") -> true
    | (_, _, "PM"), (_, _, "AM") -> false
    | (h1, m1, _), (h2, m2, _) -> (h1 % 12 < h2 % 12) || (h1 = h2 && m1 < m2)

// record version
type Meridem = AM | PM
type TimeRecord = {Hour : int; Minute: int; AmPm : Meridem}

let legalTime time = 
    (1 <= time.Hour && time.Hour <= 12) && (0 <= time.Minute && time.Minute <= 59) 

let ( <. ) time1 time2 = 
    time1.AmPm < time2.AmPm 
    || (time1.AmPm = time2.AmPm && time1.Hour % 12 < time2.Hour % 12)
    || (time1.AmPm = time2.AmPm && time1.Hour = time2.Hour && time1.Minute = time2.Minute)

(* Exercise 2: The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of
integers, and declare the functions when a representation by records is used. Declare the func-
tions in infix notation with proper precedences, and use patterns to obtain readable declarations. *)

//  currency expression as tuple
type MoneyTuple = int * int * int

// convert to pounds
let tupleMoneyToPence (pounds, shillings, pence) =
    pounds * 240 + shillings * 12 + pence

// convert back
let penceToMoneyTuple coins = 
    let pounds = coins / 240
    let shillings = (coins % 240) / 12
    let pence = coins % 12
    (pounds, shillings, pence)

// we transform values to some other type then apply the operator and then transfrom them back
// it is usually assumed that convert and convertBack are inverses of one of other and that 
// convert : 'a -> 'b where 'a and 'b are isomorphic
let combine operator convert convertBack value1 value2 = 
    operator (convert value1) (convert value2) |> convertBack

let (.++.) = combine (+) tupleMoneyToPence penceToMoneyTuple
let (.--.) = combine (-) tupleMoneyToPence penceToMoneyTuple

type MoneyRecord = {Pounds: int; Shillings: int; Pence: int}

let recordMoneyToPence money = 
    money.Pounds * 240 + money.Shillings * 12 + money.Pence

let penceToMoneyRecord coins = 
    let pounds = coins / 240
    let shillings = (coins % 240) / 12
    let pence = coins % 12
    {Pounds = pounds; Shillings = shillings; Pence = pence}

let (..++..) = combine (+) recordMoneyToPence penceToMoneyRecord
let (..--..) = combine (-) recordMoneyToPence penceToMoneyRecord
