SRC=src
TARGET=native
MAIN=main.$(TARGET)
EXE=menhirbrav
FLAGS=-use-menhir -use-ocamlfind -pkgs str

.PHONY: all clean

all:
	ocamlbuild $(FLAGS) $(SRC)/$(MAIN)
	mv $(MAIN) $(EXE)

clean:
	ocamlbuild -clean
