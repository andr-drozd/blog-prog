﻿(*
Задача 14
Самая длинная последовательность Коллатца
Следующая повторяющаяся последовательность определена для множества натуральных чисел:

n → n/2 (n - четное)
n → 3n + 1 (n - нечетное)

Используя описанное выше правило и начиная с 13, сгенерируется следующая последовательность:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
Получившаяся последовательность (начиная с 13 и заканчивая 1) содержит 10 элементов.
Хотя это до сих пор и не доказано (проблема Коллатца (Collatz)), предполагается,
что все сгенерированные таким образом последовательности оканчиваются на 1.

Какой начальный элемент меньше миллиона генерирует самую длинную последовательность?

Примечание: Следующие за первым элементы последовательности могут быть больше миллиона.
*)

let solv num = 
  let rec loop (curr: int64) len = 
    if curr = 1 then len
    else loop (if curr % 2L = 0 then curr / 2L else curr * 3L + 1L) len + 1
  loop (int64 num) 1

let time_from (t: System.DateTime) =
    (System.DateTime.Now - t).TotalMilliseconds

let t0 = System.DateTime.Now

let answer = seq {for i in 1..999_999 -> i, solv i} |> Seq.maxBy snd
printfn $"the answer is {fst answer:N0}, time = {time_from t0:N0} ms" //the answer is 837 799, time = 2 843 ms
