(*
Задача 9
Особая тройка Пифагора
Тройка Пифагора - три натуральных числа a < b < c, для которых выполняется равенство
a^2 + b^2 = c^2
Например, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
Существует только одна тройка Пифагора, для которой a + b + c = 1000.
Найдите произведение abc.
*)

//200^2 + 375^2 = 425^2

let sq a = a * a

let a = seq { 2..500 }

let ab =
    a |> Seq.pick (fun x ->
        let x1 =
            seq { x + 1 .. 999 - 2 * x }
            |> Seq.tryFind (fun y -> sq x + sq y = sq (1000 - x - y))

        match x1 with
        | Some y -> Some (x, y)
        | None -> None)

let answer = ab |> fun (a, b) -> a * b * (1000 - a - b) 

printfn $"the answer is {answer}" //the answer is 31875000
