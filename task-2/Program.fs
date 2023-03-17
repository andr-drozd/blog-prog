let fib = Seq.unfold (fun (a, b) -> Some(b, (b, a + b))) (1, 2)

let answer = fib |> Seq.takeWhile (fun x-> x < 4_000_000)
            |> Seq.filter (fun x-> x%2 = 0)
            |> Seq.sum

printfn $"the answer is {answer}"          