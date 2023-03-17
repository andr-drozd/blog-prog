(*
Задача 20
Сумма цифр факториала
n! означает n × (n − 1) × ... × 3 × 2 × 1

Например, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
и сумма цифр в числе 10! равна 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Найдите сумму цифр в числе 100!.  
*)
let fact = [1..100] |> List.map bigint |> List.reduce (*)

let answer = fact.ToString().ToCharArray() |> Array.sumBy (string>>int)

printfn $"the answer is {answer}" //the answer is 648

printfn $"{fact:N0}"