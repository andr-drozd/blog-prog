// 2520 - самое маленькое число, которое делится без остатка на все числа от 1 до 10.
// Какое самое маленькое число делится нацело на все числа от 1 до 20?

let divbylist lst (x: int) =
    lst |> List.fold (fun acc a -> if acc % a = 0 then acc / a else acc) x

let muls =
    [ 2..20 ]
    |> List.fold (fun s x -> (divbylist s x) :: s) []

let answer = muls |> List.reduce ( * )

printfn $"the answer is {answer}" //the answer is 232792560
