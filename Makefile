SRC=src
DOC=doc
TARGET=native
MAIN=main.$(TARGET)
EXE=menhirbrav
FLAGS=-use-menhir -use-ocamlfind -pkgs str
PARSER=$(SRC)/parser.mly
IMAGES=tabular syntax backnaur

.PHONY: all latex html doc clean

all:
	ocamlbuild $(FLAGS) $(SRC)/$(MAIN)
	mv $(MAIN) $(EXE)

%.png: all
	./$(EXE) latex -$* $(PARSER) -o $*.tex
	pdflatex $*.tex
	convert -density 150 $*.pdf -format png $(DOC)/$*.png
	rm -f $*.tex $*.pdf $*.aux $*.log

latex: $(IMAGES:%=%.png)

html: all
	./$(EXE) html $(PARSER) -o test.html
	wkhtmltoimage -f png --width 800 test.html $(DOC)/html.png
	rm -f test.html

doc: latex html

clean:
	ocamlbuild -clean
