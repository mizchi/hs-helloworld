all: build test run

run:
	@cabal run helloworld-exe -- https://jsonplaceholder.typicode.com/todos/1

build:
	@cabal build --enable-benchmarks all

format:
	@ormolu -i **/*.hs

check:
	@hlint src test
	@ormolu -m check **/*.hs

test: build
	@cabal test
