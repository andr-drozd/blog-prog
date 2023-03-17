
let s b = Seq.initInfinite (fun i -> i*b)
          |> Seq.takeWhile (fun x -> x < 1000)

let answer = Seq.append (s 3) (s 5)
          |> Seq.distinct
          |> Seq.sum

printfn $"the answer is {answer}" //the answer is 233168