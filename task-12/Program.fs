(*
Задача 12

Треугольное число с большим количеством делителей

Последовательность треугольных чисел образуется путем сложения натуральных чисел.
К примеру, 7-е треугольное число равно 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
Первые десять треугольных чисел:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Перечислим делители первых семи треугольных чисел:

 1: 1
 3: 1, 3
 6: 1, 2, 3, 6
10: 1, 2, 5, 10
15: 1, 3, 5, 15
21: 1, 3, 7, 21
28: 1, 2, 4, 7, 14, 28
Как мы видим, 28 - первое треугольное число, у которого более пяти делителей.

Каково первое треугольное число, у которого более пятисот делителей?
*)

let divlist1 a =
    seq {
        yield! seq { 1 .. a / 3 }
        a / 2
        a
    }
    |> Seq.filter (fun x -> a % x = 0)


let isq = float >> sqrt >> int

let divlist (a: int) =
    seq {
        for b in 1 .. isq a do
            if a % b = 0 then
              yield b
              if a/b <> b then yield a/b
    }

let divsnum n =
    let d0, d1 =
        if n % 2 = 0 then
            divlist (n / 2), divlist (n + 1)
        else
            divlist n, divlist ((n + 1) / 2)

    (Seq.length d0) * (Seq.length d1)

let time_from (t: System.DateTime) =
    (System.DateTime.Now - t).TotalMilliseconds

let t0 = System.DateTime.Now

let rec solv n =
    if divsnum n <= 500 then solv (n + 1) else n * (n + 1) / 2

let answer = solv 1

printfn "answer is %d, time %.0f ms" answer (time_from t0) //answer is 76576500, time 1512 ms
//76 576 500 = 12375*12376/2
