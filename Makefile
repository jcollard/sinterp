all: compile

compile: sinterp

sinterp: *.hs
	ghc Main.hs -o sinterp

clean: Main.o
	rm *.o *.hi
	rm sinterp
