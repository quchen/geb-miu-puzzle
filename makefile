OUT=out
SRC=src

generator :
	ghc -O2 -isrc $(SRC)/Main.hs -o GraphGenerator


graphviz : generator
	./GraphGenerator 8  | dot -Tpng > $(OUT)/miu8.png  &
	./GraphGenerator 9  | dot -Tpng > $(OUT)/miu9.png  &
	./GraphGenerator 10 | dot -Tpng > $(OUT)/miu10.png &
	./GraphGenerator 11 | dot -Tpng > $(OUT)/miu11.png &

graphvizdot : generator
	./GraphGenerator 8  > $(OUT)/miu8.gv.dot  &
	./GraphGenerator 9  > $(OUT)/miu9.gv.dot  &
	./GraphGenerator 10 > $(OUT)/miu10.gv.dot &
	./GraphGenerator 11 > $(OUT)/miu11.gv.dot &

clean :
	find ./src -name "*.hi" -delete
	find ./src -name "*.o" -delete
	rm GraphGenerator
	rm miu*.png
