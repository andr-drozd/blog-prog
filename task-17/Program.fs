(*
Задача 17
Счет букв в числительных
Если записать числа от 1 до 5 английскими словами (one; two; three; four; five); то используется всего 3 + 3 + 5 + 4 + 4 = 19 букв.

Сколько букв понадобится для записи всех чисел от 1 до 1000 (one thousand) включительно?


Примечание: Не считайте пробелы и дефисы. Например; число 342 (three hundred and forty-two) состоит из 23 букв; число 115 (one hundred and fifteen) - из 20 букв. Использование "and" при записи чисел соответствует правилам британского английского.  
*)
let voc = 
        [ 1, "one"
          2, "two"
          3, "three"
          4, "four"
          5, "five"
          6, "six"
          7, "seven"
          8, "eight"
          9, "nine"
          10, "ten"
          11, "eleven"
          12, "twelve"
          13, "thirteen"
          14, "fourteen"
          15, "fifteen"
          16, "sixteen"
          17, "seventeen"
          18, "eighteen"
          19, "nineteen"
          20, "twenty"
          30, "thirty"
          40, "forty"
          50, "fifty"
          60, "sixty"
          70, "seventy"
          80, "eighty"
          90, "ninety"
          1000, "one thousand" ] |> Map.ofList 

let rec word x =
    match x with
    | _ when Map.containsKey x voc -> voc[x]
    | _ when x < 100 -> voc[x / 10 * 10] +  "-" + word (x % 10)
    | _ when x < 1000 ->
        voc[x / 100]
        + " hundred"
        + if x % 100 <> 0 then " and " + word (x % 100) else ""
    | _ -> "unknown"

let s = seq { for i in 1..1000 -> word i }

let letters_only (s: string) =
    s.ToCharArray() |> Array.sumBy (fun c -> if c = ' ' || c = '-' then 0 else 1)

let answer = Seq.sumBy letters_only s

printfn $"the answer is {answer}" //the answer is 21124
