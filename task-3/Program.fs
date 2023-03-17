// Простые делители числа 13195 - это 5, 7, 13 и 29.
// Каков самый большой делитель числа 600851475143, являющийся простым числом?
let x = 600851475143L

let rec ans (num: int64) (d: int64) =
    match num with
    | 0L -> 0L //error
    | 1L -> d
    | _ when num % d <> 0 -> ans num (d + 1L)
    | _ -> ans (num / d) d

let answer = ans x 2L
printfn $"the answer is {answer}" //the answer is 6857
