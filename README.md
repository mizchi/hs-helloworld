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
$ cabal update
$ cabal freeze
```

https://marketplace.visualstudio.com/items?itemName=haskell.haskell

Optional: Set your ormolu path

```jsonc
{
  // vscode ormolu can not use env var like $HOME
  "ormolu.path": "/home/mizchi/.cabal/bin/ormolu",
  "[haskell]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "sjurmillidahl.ormolu-vscode"
  }
}
```

## Dev

```bash
$ cabal run
$ cabal test

# format with gild
$ cabal exec -- cabal-gild --io helloworld.cabal
```

## Examples

このプロジェクトには、Haskellの基本的な言語機能を学ぶためのサンプルコードが含まれています。
サンプルコードは `examples` ディレクトリに配置されています。

### サンプルの実行方法

```bash
# 全てのサンプルを実行
$ cabal run examples -- all

# 基本的な型と関数のサンプルを実行
$ cabal run examples -- basic

# リスト操作と高階関数のサンプルを実行
$ cabal run examples -- list

# モナドのサンプルを実行
$ cabal run examples -- monad
```

### サンプルの内容

1. **BasicTypes.hs** - 基本的な型と関数の定義、パターンマッチング
   - データ型の定義と使用方法
   - パターンマッチングの基本
   - 再帰関数の実装
   - 型クラスの定義と実装

2. **ListFunctions.hs** - リスト操作と高階関数
   - リスト生成関数
   - map、filter、foldの実装
   - リスト内包表記
   - 高階関数の使用例
   - 部分適用と関数合成

3. **Monads.hs** - モナドの基本
   - Maybeモナド
   - Eitherモナド
   - IOモナド
   - 独自モナドの実装

## LICENSE

MIT
