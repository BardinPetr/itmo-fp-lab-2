# Отчет по лабораторной работе №2 по предмету "Функциональное программирование"

> Выполнил: студент группы P3319 Бардин Петр Алексеевич

> Вариант: oa-bag

## Постановка задачи

**Разработать мультимножество на основе хеш-таблицы с открытой адресацией**

### Требования:

#### Функции:

- добавление и удаление элементов;
- фильтрация;
- отображение (map);
- свертки (левая и правая);
- структура должна быть моноидом.

#### Ограничения:

- Структуры данных должны быть неизменяемыми.
- Библиотека должна быть протестирована в рамках unit testing.
- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
- Структура должна быть полиморфной.
- Требуется использовать идиоматичный для технологии стиль программирования.

## Спроектированный интерфейс структуры

<details>
  <summary>Интерфейс модуля</summary>

```ocaml
module type BAG = sig
  type elt
  type t

  val create : int -> t
  (** [create cap] creates empty multiset with capacity = [cap] *)

  val to_list : t -> (elt * int) list
  (** [to_list multiset] is representation of multiset as list of items paired
      with multiplicities *)

  val to_rep_seq : t -> elt Seq.t
  (** [to_rep_seq multiset] is representation of multiset as sequence with
      repetitions *)

  val equal : t -> t -> bool
  (** [equal ms1 ms2] is true if contents of multistes is equal*)

  val size : t -> int
  (** [size multiset] is the count of distinct elements in the [multiset] *)

  val total : t -> int
  (** [total multiset] is the total count of all elements in the [multiset] *)

  val add : elt -> t -> t
  (** [add elem multiset] is a new multiset with [elem] added *)

  val merge : t -> t -> t
  (** [merge ms1 ms2] is a new multiset as a join of [ms1] and [ms2]*)

  val copy : t -> t
  (** [copy ms] is a new multiset with a data of [ms]*)

  val count : elt -> t -> int
  (** [count elem multiset] is the count [elem] elements in the [multiset] *)

  val mem : elt -> t -> bool
  (** [mem elem multiset] is if [elem] is present in the [multiset] *)

  val remove : elt -> t -> t
  (** [remove elem multiset] returns a new multiset with one [elem] removed if
      present. Does not affect multiset if [elem] is not present *)

  val of_list : elt list -> t
  (** [of_list lst] creates multiset from list *)

  val fold : ('acc -> elt * int -> 'acc) -> 'acc -> t -> 'acc
  (** [fold f init ms] is result of applying function [f] to distinct elements
      in multiset in such that result would be [f](...([f] ([f] [init] x1 c1) x2
      c2) ...), where c1 is multiplicity of element x1. Order is not defined *)

  val filter : (elt * int -> bool) -> t -> t
  (** [filter pred ms] is new multiset which is a copy of [ms], but only with
      element that match [pred]. Iterates over distinct elements and their
      multiplicities, when [pred] is false, whole equal group is removed*)

  val map : (elt -> elt) -> t -> t
  (** [map f ms] is new multiset accuired by applying [f] to each distinct
      element of [ms] and replaces all its copies with return value of [f]. It
      does not distinguish between copies of element and calls predicate once
      for all*)

  val mapc : (elt * int -> elt * int) -> t -> t
  (** [mapc f ms] is new multiset accuired by applying [f] to each distinct
      element of [ms] with its multiplicity, [f] should return new value and its
      new multiplicity. It does not distinguish between copies of element and
      calls predicate once for all*)
end
```

</details>

Для полиморфного исполнения был выбран идеоматичный стандартной библиотеке OCaml стиль с использованием функтора создания модуля по типу данных.

```ocaml
module Make (Typ : HashedType) : BAG with type elt = Typ.t
```

## Реализация

За основу взят массив ячеек алгебраического типа, для определения пустых и удаленных ячеек. Данные хранятся в паре значение + количество.

```ocaml
type 'a cell =
  | Empty
  | Deleted
  | Value of 'a * int

type t = { capacity : int; total : int; size : int; data : elt cell array }
```

Размер массива задается при создании и может быть изменен только полным перехешированием во время операции добавления при превышении уровня загрузки.

Для реализации хеш-таблицы используется хеш-функция, предоставляемая полиморфно в модуль вместе с типом данных. Используется линейный пробирование для поиска ячеек и классические процедуры поиска, добавления и удаления.

Операции добавления и удаления реализованы через поиск изменяемой ячейки и последующее создание копии массива данных этой измененной ячейкой.

Операция свертки реализована полным проходом по внутреннему массиву, его предварительной фильтрации по наличию данных и затем преобразованию с использованием свертки списков. Наличие правой и левой свертки смысла не имеет, так как данная структура не определяет порядок обработки.

Операции filter и map выражена через операцию свертки для добавления выбранных и измененных элементов в новое мультимножество.

Такое решение по реализации операций продиктовано следующими особенностями:

- все такие операции по-умолчанию сохраняют иммутабельность
- добавление в хеш-таблицу возможно только последовательными вставками, поэтому решение с тем, чтобы сразу делать добавления в новый мультисет имеет ту же эффективность, что и использование любой временной структуры данных для обработки с последующим построением мультисета.

## Тестирование

В рамках unit-тестирования проверяются:

- добавление (в т.ч. с повторениями)
- удаление
- объединение
- автоматическое увеличение емкости
- преобразования в и из списков
- подсчет количества элементов
- сравнение
- map, filter, fold

В рамках property-based тестирования проверяются:

- свойства моноида:
  - ассоциативность по объединению
  - единичный элемент
- соответствие определению мультимножетсва (тестирование на повторяющихся элементах)
- иммутабельность по добавлению

## Вывод

В ходе работы удалось попробовать реализовать свою иммутабельную структуру данных в функциональной парадигме. Были изучены разные способы внесения изменений и организации внутренней структуры. Было изучено применение функторов и модулей первого порядка для реализации полиморфизма структуры данных. В ходе выполнения работ были изучены эталонные реализации List, Set и Map из стандартной библиотеки, оформление интерфейса и способы организации модулей в решении основаны на них.

Проведено тестирование структуры данных на юнит тестах и с использованием тестирования свойств, в рамках которого научился использовать генераторы случайных структур с использованием QCheck и определять тестируемые свойства.
