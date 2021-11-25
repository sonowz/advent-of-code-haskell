# Advent of Code Haskell Solution

This solution aims to write elegant, production level code.

- `relude` [package](http://hackage.haskell.org/package/relude) is used as alternative `Prelude`
- Use `newtype`s for better readablity and more strict typecheck
- Use typeclasses for composability
- Use pure functions (which never throw exception) only, except functions in input processing

# Instruction

```bash
# Runs code in year 2019, day 1 with input file stored in 'inputs/' directory.
./run.sh 2019 01
```

# 2018 Solutions

Solutions in 2018 does **NOT** belong to this `stack` project.

In order to build & run 2018 code, you might want to use plain `GHC`:
```bash
ghc Day01.hs -o Day01
./Day01 < ../inputs/Y2018/Day01.txt
```
