# AGPascalTokenizer

Библиотека токенайзер для Delphi/FPC. Основана на работе [PyPascalTokenizer](https://github.com/Artem3213212/PyPascalTokenizer) [Артёма Гаврилова](https://github.com/Artem3213212).

## API

### TAGToken

TAGToken это тип `record` из:

* Text: `string` с текстом токена.
* &begin: `TAGTokenizerPos` со стартовой позицией токена.
* &end: `TAGTokenizerPos` с конечной позицией токена.
* ended: `boolean` Истина, если это был постедний токен.

Имеет конструктор Create с перечисленными переменными.

### TAGTokenizerPos

тип `record`, сохраняет позицию в формате:

* x: `integer` начало зависит от платформы (`NEXTGEN`), индекс символа в строке.
* y: `integer` начало с 0, номер строки.

### `Class` TAGPasTokenizer

Главный класс токенайзера.

* get_next(): функция, возвращает `TAGToken`. Возвращает следующий токен и изменяет позицию.
* pos: `TAGTokenizerPos` свойство, содержит позицию где токенайзер .
* is_ended: `boolean` свойство, проверка что текст закончился.

Имеет конструктор Create с параметром `input`: `TStrings`.

### `Class` TAGPasTokenizerStack

Стек из токенов автоматически заполняется из input'а.

* Push(t: `TAGToken`) кладёт токен в стек.
* Pop(): `TAGToken` достать токен из стека.
* Last: `TAGToken` свойство, выдающееся вершина стека.
* is_ended: `boolean` свойство, проверка конца текста.

Имеет конструктор Create с параметрами `input`: `TStrings` и GetComments: `boolean=True`.

### `Class` TAGPasTokenizerParallelStack

Стек из токенов автоматически заполняется из input'а(во втором потоке).

* Push(t: `TAGToken`) кладёт токен в стек.
* Pop(): `TAGToken` достать токен из стека.
* Last: `TAGToken` свойство, выдающееся вершина стека.
* ended: `boolean` свойство, проверка конца текста.

Имеет конструктор Create с параметрами input: `TStrings`, GetComments: `boolean=True` и stackMax:`integer=1000`.

## Утилиты

Вспомогательные функции для анализа текста.

* IsName(s: `string`): `boolean` Проверка что это  идентификатор.
* IsComment(s: `string`): `boolean` Проверка что это комментарий.
* IsDirective(s: `string`): `boolean` Проверка что это директива компилятора.
* IsString(s: `string`): `boolean` Проверка что это строковое значение.

## Авторы

[Артём Гаврилов](https://github.com/Artem3213212) и [Владислав Уткин](https://github.com/viad00).
