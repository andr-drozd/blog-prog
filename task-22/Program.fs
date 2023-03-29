(*
Задача 22
Очки за имена
Используйте names.txt (щелкнуть правой кнопкой мыши и выбрать 'Save Link/Target As...'),
текстовый файл размером 46 КБ, содержащий более пяти тысяч имен.
Начните с сортировки в алфавитном порядке.
Затем подсчитайте алфавитные значения каждого имени и умножьте это значение
на порядковый номер имени в отсортированном списке для получения количества очков имени.

Например, если список отсортирован по алфавиту, имя COLIN (алфавитное значение которого
3 + 15 + 12 + 9 + 14 = 53) является 938-м в списке.
Поэтому, имя COLIN получает 938 × 53 = 49714 очков.

Какова сумма очков имен в файле?  
*)

let cleanQuotes (s: string) = s.Replace ("\"", "")
let alphaValue (c: char) = c - 'A' |> int |> (+) 1
let namePoints (s: string) = s.ToCharArray() |> Array.sumBy alphaValue

let answer = 
  (System.IO.File.ReadAllText "names.txt").Split ","
  |> Array.map cleanQuotes
  |> Array.sort
  |> Array.mapi (fun i x -> namePoints x * (i+1))
  |> Array.sum

printfn "the answer is %d" answer //the answer is 871198282

