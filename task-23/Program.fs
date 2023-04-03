(*
Задача 23
Неизбыточные суммы
Совершенным числом называется число, у которого сумма его делителей равна самому числу. Например, сумма делителей числа 28 равна 1 + 2 + 4 + 7 + 14 = 28, что означает, что число 28 является совершенным числом.

Число n называется недостаточным, если сумма его делителей меньше n, и называется избыточным, если сумма его делителей больше n.

Так как число 12 является наименьшим избыточным числом (1 + 2 + 3 + 4 + 6 = 16), наименьшее число, которое может быть записано как сумма двух избыточных чисел, равно 24. Используя математический анализ, можно показать, что все целые числа больше 28123 могут быть записаны как сумма двух избыточных чисел. Эта граница не может быть уменьшена дальнейшим анализом, даже несмотря на то, что наибольшее число, которое не может быть записано как сумма двух избыточных чисел, меньше этой границы.

Найдите сумму всех положительных чисел, которые не могут быть записаны как сумма двух избыточных чисел.  
*)

let t0 = System.DateTime.Now
let timeTaken (from: System.DateTime) = (System.DateTime.Now - from).TotalMilliseconds


let delims n = 
  let isq = float>>sqrt>>int
  seq {
    yield 1
    for i in 2..isq n do
      if n%i = 0 then
        let d = n/i
        yield i
        if d<>i then yield d
  }

let izb = [
  for i in 12..28123 do
    let sd = delims i 
    if Seq.sum sd > i then 
      yield i
]

printfn $"1.izb {izb.Length}, time = {timeTaken t0:N0} ms"

// printfn $"2.sums, time = {timeTaken t0:N0} ms"

let sums2 = 
  let rec loop acc (left: int list) = 
    match left with
    | a::b -> 
      let s = 
        List.takeWhile (fun x -> x<28123-a) left
        |> List.map (fun x -> x+a)
      loop (s::acc) b
    | _ -> acc

  loop [] izb |> List.concat

printfn $"2.sums2 {sums2.Length}, time = {timeTaken t0:N0} ms"

let solv = List.except sums2 izb

printfn $"3.solv time = {timeTaken t0:N0} ms"

printfn "%A" solv
let answer = List.sum solv
printfn "the answer is %d" answer

printfn $"time = {timeTaken t0:N0} ms"
