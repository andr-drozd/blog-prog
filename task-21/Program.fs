(*
Задача 21

Дружественные числа

Пусть d(n) определяется как сумма делителей n (числа меньше n, делящие n нацело).
Если d(a) = b и d(b) = a, где a ≠ b, то a и b называются дружественной парой, а каждое из чисел a и b - дружественным числом.

Например, делителями числа 220 являются 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 и 110, поэтому d(220) = 284.
Делители 284 - 1, 2, 4, 71, 142, поэтому d(284) = 220.

Подсчитайте сумму всех дружественных чисел меньше 10000.  
*)

let isq = float >> sqrt >> int

let divs n =
    seq {
        yield 1
        for i in 2 .. isq n do
            if n % i = 0 then
                yield i
                let d = n / i
                if i <> d then
                    yield d
    }

let solv =
    seq {
        for i in 1..9999 do
            let d1 = divs i |> Seq.sum
            let d2 = divs d1 |> Seq.sum
            if d2 = i then
                yield i
    }

let answer = solv |> Seq.sum

printfn $"the answer is {answer}" //the answer is 40285
