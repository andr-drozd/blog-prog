// Число-палиндром с обеих сторон (справа налево и слева направо) читается одинаково.
// Самое большое число-палиндром, полученное умножением двух двузначных чисел – 9009 = 91 × 99.
// Найдите самый большой палиндром, полученный умножением двух трехзначных чисел.

let isPal (x: int) =
    let s = x.ToString().ToCharArray()
    s = Array.rev s

let s3 x = seq { x .. -1 .. 100 }

let folder x =
    s3 x
    |> Seq.map (( * ) x)
    |> Seq.skipWhile (not << isPal)
    |> Seq.tryHead

let answer = s3 999 |> Seq.map folder |> Seq.choose id |> Seq.max

printfn $"the answer is {answer}" //the answer is 906609
