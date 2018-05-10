// Learn more about F# at http://fsharp.org

open System
open FsCheck

[<EntryPoint>]
let main argv =
    let zeroIsIdForAdd (a:int) = (a + 0 = a) && (0 + a = a)
    let addIsAssociative (a:int) (b:int) (c:int) =
        (a + (b + c)) = ((a + b) + c)

    Check.Quick zeroIsIdForAdd
    Check.Quick addIsAssociative

    let oneIsIdForMult (a:int) = (a * 1 = a) && (1 * a = a)
    let multIsAssociative (a:int) (b:int) (c:int) =
        (a * (b * c)) = ((a * b) * c)
    
    Check.Quick oneIsIdForMult
    Check.Quick multIsAssociative

    let falseIsIdForOr (a:bool) = ((false || a) = a) && (a || false = a)
    let orIsAssociative (a:bool) (b:bool) (c:bool) =
        (a || (b || c)) = ((a || b) || c)
        
    Check.Quick falseIsIdForOr
    Check.Quick orIsAssociative

    let trueIsIdForAnd (a:bool) = ((true && a) = a) && ((a && true) = a)
    let andIsAssociative (a:bool) (b:bool) (c:bool) =
        (a && (b && c)) = ((a && b) && c)

    Check.Quick trueIsIdForAnd
    Check.Quick andIsAssociative

    let emptyIsIdForConcat (xs:'a list) = ([] @ xs = xs) && (xs @ [] = xs)
    let concatIsAssociative (a:'a list) (b: 'a list) (c: 'a list) =
        (a @ (b @ c)) = ((a @ b) @ c)

    Check.Quick emptyIsIdForConcat
    Check.Quick concatIsAssociative

    let emptyIsIdForConcat (s:string) = ("" + s = s) && (s + "" = s)
    let concatIsAssociative (a:string) (b:string) (c:string) =
        (a + (b + c)) = ((a + b) + c)

    Check.Quick emptyIsIdForConcat
    Check.Quick concatIsAssociative

    let identity x = x
    let idIsIdForFuncComp(fn:'a->'a) (a:'a) =
        ((identity >> fn) a = fn a) && ((fn >> identity) a = fn a)
    let compIsAssociative (a:'a->'a) (b:'a->'a) (c:'a->'a) (input:'a) =
        (a >> (b >> c)) input = ((a >> b) >> c) input

    Check.Quick idIsIdForFuncComp
    Check.Quick compIsAssociative

    let combine (a:'a->bool) (b:'a->bool) = 
        fun x -> a(x) && b(x)
    let (^&^) = combine
    
    let identity (x:'a) = true
    let idIsIdForCombine(fn:'a->bool) (a:'a) =
        ((identity ^&^ fn) a = fn a) && ((fn ^&^ identity) a = fn a)
    Check.Quick idIsIdForCombine

    let combineIsAssociative(a:'a->bool) (b:'a->bool) (c:'a->bool) (input:'a) =
        (a ^&^ (b ^&^ c)) input = ((a ^&^ b) ^&^ c) input
    Check.Quick combineIsAssociative

    0 // return an integer exit code
