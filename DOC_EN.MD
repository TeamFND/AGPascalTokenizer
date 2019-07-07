# AGPascalTokenizer

Tokenizer Library for Delphi/FPC. Based on [PyPascalTokenizer](https://github.com/Artem3213212/PyPascalTokenizer) work by [Artem Gavrilov](https://github.com/Artem3213212).

## API

### TAGToken struct

TAGToken is a `record` type of:

* Text: `string` with token text.
* &begin: `TAGTokenizerPos` with start position of token.
* &end: `TAGTokenizerPos` with end position of token.
* ended: `boolean` flag, true if it was the last token.

Has Create constructor with all values.

### TAGTokenizerPos

`record`, saves position of token if format:

* x: `integer` platform-depend-base (`NEXTGEN`) character index in line.
* y: `integer` 0-based line index.

### `Class` TAGPasTokenizer

Main class of tokenizer.

* get_next(): `TAGToken` function, returns. Get next token and change pos
* pos: `TAGTokenizerPos` property, containing position whear tokenizer was stoped after last get_next(), writeble.
* is_ended: `boolean` property, check if text ended.

Has constructor Create with `input`: `TStrings` param.

### `Class` TAGPasTokenizerStack

Stack of tokens automaticaly filled from input.

* Push(t: `TAGToken`) push token to stack.
* Pop(): `TAGToken` pop token from stack.
* Last: `TAGToken` property, seek stask head.
* ended: `boolean` property, check if text ended.

Has constructor Create with input: `TStrings` and GetComments: `boolean=True` param.

### `Class` TAGPasTokenizerParallelStack

Stack of tokens automaticaly filled from input(in second tread).

* Push(t: `TAGToken`) push token to stack.
* Pop(): `TAGToken` pop token from stack.
* Last: `TAGToken` property, seek stask head.
* ended: `boolean` property, check if text ended.

Has constructor Create with input: `TStrings`, GetComments: `boolean=True` and stackMax:`integer=1000` param.

## Utils

Helper functions to analyze token text.

* IsName(s: `string`): `boolean` Check is identifier (can be reserved word too).
* IsComment(s: `string`): `boolean` Check is comment.
* IsDirective(s: `string`): `boolean` Check is compiler directive.
* IsString(s: `string`): `boolean` Check is string constant.

## Author

[Artem Gavrilov](https://github.com/Artem3213212) and [Vladislav Utkin](https://github.com/viad00).
