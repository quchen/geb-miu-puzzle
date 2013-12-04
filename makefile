all :
	ghc -O2 GraphGenerator.hs
	./GraphGenerator 8  | cpp | dot -Tpng > miu8.png  &
	./GraphGenerator 9  | cpp | dot -Tpng > miu9.png  &
	./GraphGenerator 10 | cpp | dot -Tpng > miu10.png &
	./GraphGenerator 11 | cpp | dot -Tpng > miu11.png &

clean :
	rm GraphGenerator.o
	rm GraphGenerator.hi
	rm GraphGenerator
	rm miu*.png