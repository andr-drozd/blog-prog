(*
Задача 16
Сумма цифр степени
2^15 = 32768, сумма цифр этого числа равна 3 + 2 + 7 + 6 + 8 = 26.

Какова сумма цифр числа 2^1000?  
*)

let s = bigint.Pow (bigint 2, 1000)
printfn $"{s:N0}"
let answer = s.ToString().ToCharArray() |> Array.sumBy (string>>int)
printfn $"the answer is {answer}" //the answer is 1366