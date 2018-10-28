# AGPascalTokenizer

Библиотека токенайзер для Delphi/FPC. Основана на работе [PyPascalTokenizer](https://github.com/Artem3213212/PyPascalTokenizer) [Артёма Гаврилова](https://github.com/Artem3213212).

## API

### TToken

TToken это тип `record` из:

* Text: `string` с текстом токена.
* &begin: `TTokenizerPos` со стартовой позицией токена.
* &end: `TTokenizerPos` с конечной позицией токена.
* ended: `boolean` Истина, если это был постедний токен.

Имеет конструктор Create с перечисленными переменными.

### TTokenizerPos

тип `record`, сохраняет позицию в формате:

* x: `integer` начало зависит от платформы (`NEXTGEN`), индекс символа в строке.
* y: `integer` начало с 0, номер строки.

### `Class` PasTokenizer

Главный класс токенайзера.

* s: `TStrings` приватная, используйте конструктор. Список строк исходного кода.
* get_next(): функция, возвращает `TToken`. Возвращает следующий токен и изменяет свою позицию.
* read_next(): функция, не имплементированно, Возвращает следующий токен, не изменяя свою позицию.
* is_ended(): функция, не имплементированно, проверка что текст закончился.

Имеет конструктор Create с параметром `s`.

### `Class` PasTokenizerStack

Не имплементированно.

### `Class` PasTokenizerParallelStack

Не имплементированно.

## Утилиты

Вспомогательные функции для анализа текста.

* is_name(s: `string`): `boolean` Проверка что идентификатор правильный.
* is_comment(s: `string`): `boolean` Проверка что комментарий правильный.
* is_string(s: `string`): `boolean` Проверка что это строковое значение.

## Авторы

[Артём Гаврилов](https://github.com/Artem3213212) и [Владислав Уткин](https://github.com/viad00).
