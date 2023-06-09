﻿(*
Задача 26
Обратные циклы
Единичная дробь имеет 1 в числителе. Десятичные представления единичных дробей со знаменателями от 2 до 10 даны ниже:

1/2	=	0.5
1/3	=	0.(3)
1/4	=	0.25
1/5	=	0.2
1/6	=	0.1(6)
1/7	=	0.(142857)
1/8	=	0.125
1/9	=	0.(1)
1/10	=	0.1
Где 0.1(6) значит 0.166666..., и имеет повторяющуюся последовательность из одной цифры. Заметим, что 1/7 имеет повторяющуюся последовательность из 6 цифр.

Найдите значение d < 1000, для которого 1/d в десятичном виде содержит самую длинную повторяющуюся последовательность цифр.

*)

let solv =
    let rec stolb a b acc2 =
        let d = a % b

        if d = 0 then
            0, acc2
        else if List.contains d acc2 then
            1 + List.findIndex (fun x -> x = d) acc2, acc2
        else
            stolb (10 * d) b (d :: acc2)

    seq { for x in 2..1000 -> x, stolb 1 x [] |> fst }

let answer = solv |> Seq.maxBy snd |> fst
printfn $"the answer is {answer}"  //the answer is 983
