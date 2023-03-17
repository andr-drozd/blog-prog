﻿(*
Задача 15
Пути через таблицу
Начиная в левом верхнем углу сетки 2×2 и имея возможность двигаться только вниз или вправо,
существует ровно 6 маршрутов до правого нижнего угла сетки.

Сколько существует таких маршрутов в сетке 20×20?  
*)

let rec fact n = 
  if n=1 then bigint 1 else bigint n * fact (n-1)

let answer =
  let f20 = fact 20
  (fact 40)/(f20*f20)

printfn $"the answer is {answer:N0}" //the answer is 137 846 528 820