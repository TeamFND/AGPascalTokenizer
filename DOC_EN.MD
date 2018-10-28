# AGPascalTokenizer

Tokenizer Library for Delphi/FPC. Based on [PyPascalTokenizer](https://github.com/Artem3213212/PyPascalTokenizer) work by [Artem Gavrilov](https://github.com/Artem3213212).

## API

### TToken struct

TToken is a `record` type of:

* Text: `string` with token text.
* &begin: `TTokenizerPos` with start position of token.
* &end: `TTokenizerPos` with end position of token.
* ended: `boolean` flag, true if it was the last token.

Has Create constructor with all values.

### TTokenizerPos

`record`, saves position of token if format:

* x: `integer` platform-depend-base (`NEXTGEN`) character index in line.
* y: `integer` 0-based line index.

### `Class` PasTokenizer

Main class of tokenizer.

* s: `TStrings` private, use constructor. List of source code strings.
* get_next(): function, returns `TToken`. Get next token and change end pos
* read_next(): function, not implemented, get next token, but don't change end pos.
* is_ended(): function, not implemented, check if text ended.

Has constructor Create with `s` param.

### `Class` PasTokenizerStack

Not implemented.

### `Class` PasTokenizerParallelStack

Not implemented.

## Utils

Helper functions to analyze token text.

* is_name(s: `string`): `boolean` Check for valid identifier (can be reserved word too).
* is_comment(s: `string`): `boolean` Check for valid comment.
* is_string(s: `string`): `boolean` Check for string constant.

## Author

[Artem Gavrilov](https://github.com/Artem3213212) and [Vladislav Utkin](https://github.com/viad00).
