



aoc : main.hs
	ghc -o aoc main.hs

build : aoc

debug:
	echo main.hs | entr -c -- make run -s

run : build
	./aoc < input

