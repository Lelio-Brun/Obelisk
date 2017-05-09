SRC=src
DOC=doc
TARGET=native
MAIN=main.$(TARGET)
EXE=menhirbrav
FLAGS=-use-menhir -use-ocamlfind -pkgs str
IMAGES=tabular syntax backnaur

.PHONY: all doc clean

all:
	ocamlbuild $(FLAGS) $(SRC)/$(MAIN)
	mv $(MAIN) $(EXE)

%.png: all
	./$(EXE) latex -$* $(SRC)/parser.mly -o $*.tex
	pdflatex $*.tex
	convert $*.pdf -format png $(DOC)/$*.png
	rm -f $*.tex $*.pdf $*.aux $*.log

doc: $(IMAGES:%=%.png)
	echo $^

clean:
	ocamlbuild -clean
