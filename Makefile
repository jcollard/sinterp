all: compile

compile: sinterp

sinterp: Main.hs
	ghc Main.hs -o sinterp

clean: Main.o
	rm *.o *.hi
	rm sinterp
