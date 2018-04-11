default:
	rm -f Dokens.hs
	rm -f Drammar.hs
	alex Dokens.x
	happy Drammar.y
	ghc --make -o myinterpreter Main.hs

clean:
	rm Dokens.hs
	rm Drammar.hs
	rm -f *.o
	rm -f *.hi
	rm -f myinterpreter
