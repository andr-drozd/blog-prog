// Задача 10
// Сложение простых чисел
// Сумма простых чисел меньше 10 равна 2 + 3 + 5 + 7 = 17.
// Найдите сумму всех простых чисел меньше двух миллионов.

let undiv lst a =
    lst |> List.forall (fun x -> a % x <> 0)

let rec check acc curr =
    if curr >= 200_000 then acc
    else if undiv acc curr then check (curr :: acc) (curr + 2)
    else check acc (curr + 2)

let time1 = System.DateTime.Now

let answer = check [ 2 ] 3 |> List.sumBy int64

let time2 = System.DateTime.Now

let dt = (time2 - time1).TotalMilliseconds

printfn $"the answer is {answer}, time = {dt:f0} ms" //the answer is 1709600813, time = 4799 ms

let timeFrom (t: System.DateTime) = (System.DateTime.Now - t).TotalMilliseconds

let t0 = System.DateTime.Now
let plus2 x = x + 2
let nums = Array.init (2_000_000-3) (plus2)
for i in [0..nums.Length/2] do
  if nums[i] <> 0 then
    for j in [i+nums[i]..nums[i]..nums.Length-1] do nums[j] <-0
let answer2 = Array.sumBy int64 nums

printfn $"the answer2 is {answer2}, time = {timeFrom t0:f0} ms" //the answer2 is 1709600813, time = 51 ms
