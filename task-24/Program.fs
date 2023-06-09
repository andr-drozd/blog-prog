﻿(*
Задача 24
Словарные перестановки
Перестановка - это упорядоченная выборка объектов. К примеру, 3124 является одной из возможных перестановок из цифр 1, 2, 3 и 4. Если все перестановки приведены в порядке возрастания или алфавитном порядке, то такой порядок будем называть словарным. Словарные перестановки из цифр 0, 1 и 2 представлены ниже:

012   021   102   120   201   210

Какова миллионная словарная перестановка из цифр 0, 1, 2, 3, 4, 5, 6, 7, 8 и 9?  

0123456789
-------798
-------879
-------897
-------978
-------987
*)



let solv =
    let fact n = seq { 1..n } |> Seq.reduce (*)

    let rec loop acc (curr: int list) (left: int) =
        match curr.Length with
        | 0 -> acc
        | 1
        | _ when left = 0 -> acc @ curr
        | _ ->
            let l = fact (curr.Length - 1)
            let n = left / l
            let d = List.item n curr
            loop (acc @ [ d ]) (List.removeAt n curr) (left - n * l)

    loop [] [ 0..9 ] 1000000

printfn "the answer is %A" solv //the answer is [2; 7; 8; 3; 9; 1; 5; 6; 0; 4]
