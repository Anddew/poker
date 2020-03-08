# Poker

## TODO
* Show
* Parser with error handling
* recursive read from console (IO?)

* Андрей:
  - выделить в Combination weight в отдельный ADT, тогда будет меньше наследования с передачей полей и будет выглядить аккуратнее
  - меня смущают регексы :я бы зарефакторил на скаловую логику для большей читаемости
  - прочекать ворнинги, например eitherValue.right зачем?
  - зарефакторить findTopCombination, слишком сложный и длинный. каждый кейс может быть чем-то типа smart constructor
  - LazyList поменять на рекурсию