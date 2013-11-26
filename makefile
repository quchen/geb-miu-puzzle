graph :
	ghc -O2 GraphGenerator.hs
	runhaskell makeGraphs

watch :
	python when-changed.py GraphGenerator.hs makefile makeGraphs.hs -c make graph

clean :
	rm GraphGenerator.o
	rm GraphGenerator.hi
	rm GraphGenerator
	rm miu*.png