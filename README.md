# regex
A regex evaluator written in Haskell.

This is a toy implementation of regex evaluation which supports the following features:

- `*` matching zero or more items
- `+` matching one or more items
- `?` matching zero or one item
- `^` match the beginning of the string
- `$` match the end of the string
- `|` alternative matches
- `*` matching zero or more items
- `.` match any character
- `()` capture groups
- `[]` alternative character matching
- `[^]` negation of alternative character matching
- `{1}`,`{1,2}`, `{,1}`, `{1,}` match quantifiers
- `\d` matching digits
- `\\` character escaping
