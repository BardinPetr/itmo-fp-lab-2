# Отчет по лабораторной работе №1 по предмету "Функциональное программирование"

> Выполнил: студент группы P3319 Бардин Петр Алексеевич

## Постановка задачи

### Задача 9

Найти тройку натуральных чисел для которой выполняется:

$$
\begin{cases}
a < b < c, \\
a^2+b^2=c^2, \\
a+b+c=1000
\end{cases}
$$

Утверждается, что она существует и единственна.

Вывести $a\cdot b \cdot c$

### Задача 21

Пусть $d(n)$ - сумма всех натуральных делителей числа кроме него самого.

Различные числа $a, b$ - _дружественные_, если $d(a) = b, d(b) = a$.

Вывести сумму всех дружественных чисел меньше 10000.

## Реализации

### Задача 9

<details>
  <summary>Код решений</summary>

#### Реализация на хвостовой рекурсии

```ocaml
(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian a b c = (a * a) + (b * b) = c * c

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by
    recursively iterating upwards over [b] if possible, then over [a] and so on,
    then checking [is_pythagorian] for [c] calculated from sum*)
let solve sum =
  let rec find_triplet sum a b =
    if a >= sum then
      failwith "Solution not found"
    else
      let c = sum - a - b in
      if is_pythagorian a b c then
        a * b * (sum - a - b)
      else if b < sum then
        find_triplet sum a (b + 1)
      else
        find_triplet sum (a + 1) (a + 2)
  in
  find_triplet sum 1 2
```

#### Модульная реализация

```ocaml
(** [triplet_list sum] is a list of triplets ([a],[b],[c]) where [a] < [b] < [c]
    and ([a] + [b] + [c] = [sum]) *)
let gen_triplet_list sum =
  let rec triplet_list_aux sum a b acc =
    let c = sum - a - b in
    if a >= sum / 3 then
      (* limit with sum/3+(sum/3+1)+(sum/3+2) = sum+3 *)
      acc
    else if b >= c then
      (* go to next a, and b after a *)
      triplet_list_aux sum (a + 1) (a + 2) ((a, b, c) :: acc)
    else
      triplet_list_aux sum a (b + 1) ((a, b, c) :: acc)
  in
  triplet_list_aux sum 1 2 []

(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian (a, b, c) = (a * a) + (b * b) = c * c

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by firstly
    generating list of triplets matching restrictions over a,b,c and then
    filtering with [is_pythagorian]*)
let solve sum =
  sum
  |> gen_triplet_list
  |> List.filter is_pythagorian
  |> List.hd
  |> fun (x, y, z) -> x * y * z

```

#### Реализация с использованием отображений

```ocaml
(** [solve sum] searches for pythagorian triplet with items sum equals [sum] *)
let solve sum =
  List.init (sum - 2) (( + ) 1)
  (* get all pairs where [a] < [b] < [sum] *)
  |> List.map (fun a -> List.init (sum - a - 1) (fun b -> (a, a + b + 1)))
  |> List.flatten
  (* calculate [c] from [sum] *)
  |> List.map (fun (a, b) -> (a, b, sum - a - b))
  (* filter by [a]<[b]<[c] *)
  |> List.filter (fun (_, b, c) -> b < c)
  (* calculate product for triples that matches pythongorial equation *)
  |> List.filter_map (fun (a, b, c) ->
         if (a * a) + (b * b) == c * c then
           Some (a * b * c)
         else
           None)
  |> List.hd
```

#### Реализация с синтаксисом циклов

```ocaml
(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian a b c = (a * a) + (b * b) = c * c

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by iterating
    [a] and [b] in bounds of [sum] and checking [is_pythagorian]
    @return product of triplet components*)
let solve sum =
  (* as OCaml do not have loop exit command,
      we need to store result in variable by reference*)
  let out = ref 0 in
  try
    for a = 1 to sum - 1 do
      for b = a + 1 to sum - 1 do
        let c = sum - a - b in
        if is_pythagorian a b c then (
          out := a * b * c;
          (* raise error to stop iteration, as by task there is only one such triplet*)
          raise Exit
        )
      done
    done;
    failwith "Solution not found"
  with Exit -> !out
```

#### Реализация на бесконечных списках

```ocaml
(** [is_pythagorian a b c] checks that [a] [b] [c] forms pythagorian triplet *)
let is_pythagorian (a, b, c) = (a * a) + (b * b) = c * c

(** [generate_inf_pairs] is infinite sequence of infinite sequences of integer
    pairs ([a], [b]) such that [a] < [b] *)
let generate_inf_pairs =
  Seq.ints 1 |> Seq.map (fun a -> Seq.ints (a + 1) |> Seq.map (fun b -> (a, b)))

(** [to_triple_with_sum sum (a, b)] makes triplet by adding component such that
    sum of triplet equals to [sum]*)
let to_triple_with_sum sum (a, b) = (a, b, sum - a - b)

(** [generate_triples_up_to_sum sum] if finite sequence of integer triples ([a],
    [b], [c]) such that [a] < [b] < [c] limited by ([a]+[b]+[c]) = [sum] *)
let generate_triples_up_to_sum sum =
  generate_inf_pairs
  (* limit a *)
  |> Seq.take sum
  (* limit b and c by cheching a < b < c *)
  |> Seq.flat_map (fun i ->
         i
         |> Seq.map (to_triple_with_sum sum)
         |> Seq.take_while (fun (_, b, c) -> b < c))

(** [solve sum] finds one pythagorian triplet sum of which is [sum] by filtering
    triplets with [is_pythagorian]*)
let solve sum =
  generate_triples_up_to_sum sum
  |> Seq.find is_pythagorian
  |> Option.map (fun (x, y, z) -> x * y * z)
  |> Option.get

```

#### Реализация на Python

```python
sum = 1000

for a in range(1, sum):
  for b in range(a + 1, sum):
    c = sum - a - b
    if a**2 + b**2 == c**2:
      print(a * b * c)
```

</details>

### Задача 21

<details>
  <summary>Код решений</summary>

#### Реализация на рекурсии

```ocaml
(** [div_sum n] is sum of divisors of [n] calculated by recursively iterating
    numbers under [n] *)
let div_sum n =
  let rec div_sum_rec n cur =
    if cur >= n then
      0
    else
      (* if this is divisor add it to sum of divisors greater *)
      (if n mod cur = 0 then
         cur
       else
         0)
      + div_sum_rec n (cur + 1)
  in
  div_sum_rec n 1

(** [solve max] finds sum of amicible numbers under [max] by checking all
    numbers to have pair with [div_sum]*)
let solve max =
  let rec amicible_sum_aux cur =
    if cur >= max then
      0
    else
      let cur_sum = div_sum cur in
      (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
      (if cur <> cur_sum && div_sum cur_sum = cur then
         cur
       else
         0)
      + amicible_sum_aux (cur + 1)
  in
  amicible_sum_aux 1
```

#### Реализация на хвостовой рекурсии

```ocaml
(** [div_sum n] is sum of divisors of [n] implemented with tail recursion by
    accumulating sum*)
let div_sum n =
  let rec div_sum_aux cur acc =
    if cur >= n then
      acc
    else
      div_sum_aux (cur + 1)
        (acc
        +
        if n mod cur = 0 then
          cur
        else
          0)
  in
  div_sum_aux 1 0

(** [solve max] finds sum of amicible numbers under [max] implemented with tail
    recursion by accumulating sum of numbers under [max] that are amicible bt
    matching with [div_sum]*)
let solve max =
  let rec amicible_sum_aux cur acc =
    if cur >= max then
      acc
    else
      (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
      let pair = div_sum cur in
      amicible_sum_aux (cur + 1)
        (* for amicible pair add number to sum of amicible pairs greater that that *)
        (if cur <> pair && div_sum pair = cur then
           acc + cur
         else
           acc)
  in
  amicible_sum_aux 1 0
```

#### Модульная реализация

```ocaml
(** [div_sum n] is sum of divisors of [n] *)
let div_sum n =
  List.init (n - 1) (( + ) 1)
  (* iterate over list of numbers under [n] and sum all that divides *)
  |> List.filter (fun x -> n mod x = 0)
  |> List.fold_left ( + ) 0

(** [is_amicible n] checks if [n] is amicibly by calling [div_sum]*)
let is_amicible n =
  if n < 2 then
    false
  else
    (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
    let pair = div_sum n in
    n <> pair && n = div_sum pair

(** [solve max] finds sum of amicible numbers under [max]*)
let solve max =
  List.init max (( + ) 1)
  |> List.filter (fun x -> is_amicible x)
  |> List.fold_left ( + ) 0
```

#### Реализация с использованием отображений

```ocaml
(** [div_sum n] is sum of divisors of [n] *)
let div_sum n =
  List.init (n - 1) (( + ) 1)
  |> List.filter (fun x -> n mod x = 0)
  |> List.fold_left ( + ) 0

(** [solve n] finds sum of amicible numbers under [n] by firstly generating list
    of numbers and its sum of divisors, and then finding amicible numbers by
    searching pairs in that array*)
let solve n =
  let paired =
    List.init n (( + ) 1)
    (* claculate all sums *)
    |> List.map (fun x -> (x, div_sum x))
    (* when number equals its sum of divs, it is can't have a pair *)
    |> List.filter (fun (a, b) -> a <> b)
  in
  paired
  |> List.filter_map (fun (a, b) ->
         (* find such pair that is reverse of our, what means that other number has sum equals to our number*)
         paired |> List.find_opt (( = ) (b, a)) |> Option.map (fun (a, _) -> a))
  |> List.fold_left ( + ) 0
```

#### Реализация с синтаксисом циклов

```ocaml
(** [div_sum n] is sum of divisors of [n] calculated directly by bruteforcing
    all numbers under [n] and adding them to sum into variable [sum] *)
let div_sum n =
  let sum = ref 0 in
  for i = 1 to n - 1 do
    if n mod i == 0 then sum := !sum + i
  done;
  !sum

(** [solve max] finds sum of amicible numbers under [max]*)
let solve max =
  let sum = ref 0 in
  for i = 1 to max - 1 do
    (* when [i] and [j] are amicible, j = div_sum i and i = div_sum j *)
    let j = div_sum i in
    if i <> j && div_sum j = i then sum := !sum + i
  done;
  !sum
```

#### Реализация на Python

```python
def d(n):
  return sum(i for i in range(1, n) if n % i == 0)

mx = 10000
ds = [d(i) for i in range(mx)]

out = 0
for i in range(1, mx):
  j = ds[i]
  if j != i and j <= mx and i == ds[j]:
    out += i

print(out)
```

</details>

## Вывод

В ходе работы удалось так или иначе применить все обозначенные способы. Стоит отметить при этом, что большинство из них пришлось реализовать чисто для примера, и они не дают оптимального решения на по производительности, ни по качеству кода:

- не очень понял чем отличаются задание по модульному коду и использованию отображений, потому как по факту, если работать в списочных терминах, все равно будем map-filter-fold, но для примера я в одном из модульных заданий взял рекурсивную генерацию и списочную обработку.
- самые лакночничные решения получились на списках и последовательностях, но, замечу, что реально нужно использовать не List в таком случае, а например модуль Iter, чтобы все наложения отображений и фильтраций последовательности откладывать до ее реального вычисления, так как листы иммутабельные и такие операции при большом чиле объектов обойдутся дорого.
- на бесконечные последовательности задачи подходящей хорошо не было, поэтому я сделал этот метод в 9 задаче только, ибо в 21 это просто перенести с list на seq или iter, тут бы больше подошла задача с явным динамическим программированием, все таки этот способ выигрывает когда есть последовательность в которой задано рекуррентное соотношение на предыдущие значения, а тут просто перебор списков.
- рекурсивные алгоритмы сразу получались с хвостовой рекурсией, они еще и более аккуратно так выглядят.
- циклы очень плохи в OCaml и пользоваться ими в привычном императивном стиле невыгодно, в частности из-за отсутствия возможности произвольно выйти из цикла или получить оттуда значение, обе задачи решаются громоздко через исключения и сохранение в переменную, но это вводит мутабельное состояние и уже выходит из функциональной парадигмы.
