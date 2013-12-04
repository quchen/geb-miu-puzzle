OUT=out
SRC=src

generator :
	ghc -O2 -isrc $(SRC)/Main.hs -o GraphGenerator


graphviz : generator
	./GraphGenerator 8  | dot -Tpng > $(OUT)/miu8.png  &
	./GraphGenerator 9  | dot -Tpng > $(OUT)/miu9.png  &
	./GraphGenerator 10 | dot -Tpng > $(OUT)/miu10.png &
	./GraphGenerator 11 | dot -Tpng > $(OUT)/miu11.png &

dot : generator
	./GraphGenerator 8  > $(OUT)/miu8.export.dot  &
	./GraphGenerator 9  > $(OUT)/miu9.export.dot  &
	./GraphGenerator 10 > $(OUT)/miu10.export.dot &
	./GraphGenerator 11 > $(OUT)/miu11.export.dot &
	./GraphGenerator 12 > $(OUT)/miu12.export.dot &
	./GraphGenerator 13 > $(OUT)/miu13.export.dot &
	./GraphGenerator 14 > $(OUT)/miu14.export.dot &
	./GraphGenerator 15 > $(OUT)/miu15.export.dot &
	./GraphGenerator 16 > $(OUT)/miu16.export.dot &

clean :
	find ./src -name "*.hi" -delete
	find ./src -name "*.o" -delete
	rm GraphGenerator
	rm miu*.png
