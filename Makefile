.PHONY: all build configure clean stdlib

all: build

build: configure stdlib
	cabal build

stdlib: stdlib/stdlib.c
	gcc -c stdlib/stdlib.c -o stdlib/stdlib.o

configure:
	cabal update
	cabal configure

clean:
	cabal clean
	$(RM) stdlib/stdlib.o
