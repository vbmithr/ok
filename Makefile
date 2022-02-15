.PHONY: all clean

all:
	dune build @install @runtest

clean:
	dune clean
