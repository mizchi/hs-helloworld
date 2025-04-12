# hs-helloworld

Haskell for the first time in 10 years

- [x] Install
- [x] Run
- [x] Test
- [x] CI

## Setup

Install ghcup https://www.haskell.org/ghcup/install/

```bash
$ ghcup install ghc 9.10.1
$ ghcup install cabal 3.14.2.0
$ ghcup install hls 2.9.0.1
$ ghcup set ghc 9.10.1

# checkout cabal
$ curl -s -L https://stackage.org/lts/cabal.config -o cabal.project.freeze
$ cabal build
```

https://marketplace.visualstudio.com/items?itemName=haskell.haskell

## Dev

```bash
$ cabal run
$ cabal test
```

## LICENSE

MIT