OUT=out
SRC=src

generator :
	ghc -O2 -isrc $(SRC)/Main.hs -o GraphGenerator


graphviz : generator
	./GraphGenerator gv 8  | dot -Tpng > $(OUT)/miu8.png  &
	./GraphGenerator gv 9  | dot -Tpng > $(OUT)/miu9.png  &
	./GraphGenerator gv 10 | dot -Tpng > $(OUT)/miu10.png &
	./GraphGenerator gv 11 | dot -Tpng > $(OUT)/miu11.png &

dot : generator
	./GraphGenerator dot 8  > $(OUT)/miu8.export.dot  &
	./GraphGenerator dot 9  > $(OUT)/miu9.export.dot  &
	./GraphGenerator dot 10 > $(OUT)/miu10.export.dot &
	./GraphGenerator dot 11 > $(OUT)/miu11.export.dot &
	./GraphGenerator dot 12 > $(OUT)/miu12.export.dot &
	./GraphGenerator dot 13 > $(OUT)/miu13.export.dot &
	./GraphGenerator dot 14 > $(OUT)/miu14.export.dot &
	./GraphGenerator dot 15 > $(OUT)/miu15.export.dot &
	./GraphGenerator dot 16 > $(OUT)/miu16.export.dot &

clean :
	find ./src -name "*.hi" -delete
	find ./src -name "*.o" -delete
	rm GraphGenerator
	rm miu*.png
