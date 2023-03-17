
// Задача 7
// 10001-е простое число
// Выписав первые шесть простых чисел, получим 2, 3, 5, 7, 11 и 13. Очевидно, что 6-е простое число - 13.

// Какое число является 10001-м простым числом?

// open System

let undiv lst a = lst |> List.forall (fun x -> a%x <> 0)

let rec check acc curr = 

  if List.length acc >= 10001 then acc else
  if undiv acc curr then check (curr::acc) (curr+2)
  else check acc (curr+2)

let time1 = System.DateTime.Now

let answer = check [2] 3 |> List.head

let time2 = System.DateTime.Now

let dt = (time2-time1).TotalMilliseconds

printfn $"the answer is {answer}, time = {dt:f0} ms" //the answer is 104743, time = 2651 ms