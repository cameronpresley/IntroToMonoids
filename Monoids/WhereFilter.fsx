module Filters =
    let isEven x = x % 2 = 0
    
    let doesntContainZero (x:int) = not (x.ToString().Contains("0"))
    
    let hasNoRepeats (x:int) = 
        let noDupesLength = x.ToString() |> Seq.distinct |> Seq.length
        x.ToString().Length = noDupesLength

    // int -> int array
    let private digitize (x:int) =
        x.ToString() |> Seq.map(fun i -> System.Int32.Parse(i.ToString())) |> Seq.toArray

    let firstDigitIs4MoreThanSecond (x:int) =
        let digits = digitize x
        digits.[0] = digits.[1] + 4

    let thirdDigitIs1LessThanSixth (x:int) =
        let digits = digitize x
        digits.[2] = digits.[5] - 1

    let fourthFifthProductOfFirstSixth (x:int) =
        let digits = digitize x
        (digits.[3] * 10) + digits.[4] = (digits.[0] * digits.[5])

open Filters


[100_000 .. 1_000_000]
    |> List.filter isEven
    |> List.filter doesntContainZero
    |> List.filter hasNoRepeats
    |> List.filter firstDigitIs4MoreThanSecond
    |> List.filter thirdDigitIs1LessThanSixth
    |> List.filter fourthFifthProductOfFirstSixth

[100_000 .. 1_000_000]
    |> List.filter (fun x -> isEven(x) 
                          && doesntContainZero(x) 
                          && hasNoRepeats(x) 
                          && firstDigitIs4MoreThanSecond(x)
                          && thirdDigitIs1LessThanSixth(x)
                          && fourthFifthProductOfFirstSixth(x))

let identity _ = true
let combine a b = fun x -> a(x) && b(x)

let filter = 
    [
        isEven; 
        doesntContainZero; 
        hasNoRepeats; 
        firstDigitIs4MoreThanSecond; 
        thirdDigitIs1LessThanSixth; 
        fourthFifthProductOfFirstSixth
    ] |> List.fold combine identity 

[100_000 .. 1_000_000] |> List.filter filter



