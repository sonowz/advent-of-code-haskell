# Advent of Code Haskell Solution

This solution aims to write elegant, production level code.

- `relude` [package](http://hackage.haskell.org/package/relude) is used as alternative `Prelude`
- Use `newtype`s for better readablity and more strict typecheck
- Use typeclasses for composability
- Use pure functions (which never throw exception) only, except functions in input processing
